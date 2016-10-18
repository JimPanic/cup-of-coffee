{Instance, words} = require './helper'

Location = {
  file:   undefined
  line:   0
  column: 0
  index:  0
}

Token = {
  type:   undefined
  value:  undefined
  length: undefined
  start:  undefined
  end:    undefined
}

TokenStream = {
  code:   undefined
  tokens: []
}

BOM             = 65279
TRAILING_SPACES = /\s+$/
NEWLINE         = /^\n/

IDENTIFIER = /// ^
  (?!\d)
  ( (?: (?!\s)[$\w\x7f-\uffff] )+ )
///

OPERATOR = /// ^ (
  ?: [-=]>             # function
   | [-+*/%<>&|^!?=]=  # compound assign / compare
   | >>>=?             # zero-fill right shift
   | ([-+:])\1         # doubles
   | ([&|<>*/%])\2=?   # logic / shift / power / floor division / modulo
   | \?(\.|::)         # soak access
   | \.{2,3}           # range or splat
   | .                 # any other character
) ///

NUMBER = ///
  ^ 0b[01]+    |              # binary
  ^ 0o[0-7]+   |              # octal
  ^ 0x[\da-f]+ |              # hex
  ^ \d*\.?\d+ (?:e[+-]?\d+)?  # decimal
///i

REGEX = /// ^
  / (?!/) ((
  ?: [^ [ / \n \\ ]  # every other thing
   | \\[^\n]         # anything but newlines escaped
   | \[              # character class
       (?: \\[^\n] | [^ \] \n \\ ] )*
     \]
  )*) (/)?
///

REGEX_MODIFIER = /^[imgy]+/

FUNCTION = /^[-=]>/
COMMA = /^,/

# Unary tokens
UNARY = words 'new typeof delete do'
UNARY_MATH  = words '! ~ not'

# Logical tokens
LOGIC = words '&& || & | ^'

# Bit-shifting tokens
SHIFT = words '<< >> >>>'

# Comparison tokens
COMPARE = words '== != < > <= >='

# Mathematical tokens
MATH = words '+ - * / % // %%'

# Relational tokens that are negatable with the unary not token
RELATION = words 'in of instanceof'

# Boolean tokens
BOOL = words 'true false yes no'

COMMENT = /^#[^#\n]*/
BLOCK_COMMENT = /^###.*###$/

INLINE_STRING_QUOTES         = ['"', "'"]
INLINE_STRING_ESCAPED_QUOTES = ['\\"', "\\'"]
BLOCK_STRING                 = ['"""', "'''"]


STATEMENTS = words 'break continue debugger'

COMPOUND_ASSIGN = words \
  '-= += /= *= %= ||= &&= ?= <<= >>= >>>= &= ^= |= **= //= %%='

ACCESSORS = /^(\.\.\.|\.\.|\.|::|@)/

ACCESSOR_TYPE = {
  '.':  'property-accessor'
  '@':  'this-accessor'
  '::': 'prototype-accessor'
}

BRACKET_TYPE = {
  '(': 'open-round-bracket'
  ')': 'close-round-bracket'
  '[': 'open-square-bracket'
  ']': 'close-square-bracket'
  '{': 'open-curly-bracket'
  '}': 'close-curly-bracket'
}

RANGES = {
  '..':  'range-exclusive'
  '...': 'range-inclusive'
}

SPLAT = '...'

WHITESPACE = /^[^\n\S]+/

# Return a function that analyses a the next chunk in given stream based on the
# passed in type and expression.
# The type can be a function or a string. In case it is a function, it is called
# with the identifier and the previous token.
# The expression must be a RegExp object.
#
# The returned function in turn takes and *always* returns a stream.
inline = (type, expression) ->
  # Transform the type into an id function if it is no function yet.
  # It is then called by the returned tokenize function.
  callableType = {
    false:  () -> type
    true: type
  }[type.constructor.name is 'Function']

  (stream) ->
    match = expression.exec stream.code

    return stream unless match

    [value, ...]    = match
    [..., previous] = stream.tokens
    [start, end]    = advance previous, value

    t = callableType(value, previous)

    return stream unless t

    token = Instance Token, previous, {
      start
      end
      type:   t
      value:  value
      length: value.length
    }

    {
      code:   stream.code[value.length..]
      tokens: [stream.tokens..., token]
    }

# Return a function to analyse block tokens based on given condition and
# consumer functions, the initial offset (usually length of start indicator like
# '###' -> 3) and the type.
#
# * type      String
# * offset    Integer
# * consumer  [String, String, String, Integer, String] -> Bool
# * condition TokenStream -> Bool
block = (type, offset, consumer, condition) ->
  (stream) ->
    return stream unless condition stream

    endIndex = offset + consume stream.code[offset - 1..], 1, consumer

    return stream unless endIndex?

    endLocation     = blockEndLocation(stream.code[..endIndex - 1])
    [..., previous] = stream.tokens
    [start, end]    = advanceBlock previous, endLocation

    token = Instance Token, previous, {
      start
      end
      type:   type
      value:  stream.code[..endIndex - 1]
      length: end.index - start.index
    }

    {
      code:   stream.code[endIndex..]
      tokens: [stream.tokens..., token]
    }

# Function to create start and end location objects. It is only usable for
# tokens without newlines.
#
# Takes the previous token and the value of the current token to determine new
# index and column values, merged with the previous token's end location.
advance = (previous, value) ->
  start = previous.end
  end = Instance Location, start, {
    column: start.column + value.length
    index:  start.index  + value.length
  }

  [start, end]

# Function to create start and end location objects. Usable only for tokens
# possibly spanning over multiple lines.
#
# Takes the previous token and the end position of the current token. This end
# position is usually gathered by `blockEndLocation`.
advanceBlock = (previous, {index, line, column}) ->
  start = previous.end
  end   = Instance Location, start, {
    index:  start.index  + index
    line:   start.line   + line
    column: start.column + (column - 1)
  }

  [start, end]

# Function to calculate the end position of a value possibly spanning multiple
# lines.
#
# Takes the value and uses a `reduce` operation to go through every char,
# aggregating line, column and index values. Returns an object resembling parts
# of a `Location` object.
blockEndLocation = (value) ->
  countNewlines = (seed, char, index) ->
    seed.index = index

    if char is "\n"
      seed.line  += 1
      seed.column = 0
    else
      seed.column += 1

    seed

  value.split('').reduce countNewlines, {index: 0, line: 0, column: 0},

# Consume characters in code from given startIndex until the passed condition is
# met or the index falls out of bounds.
#
# Return either the index at which the condition was met or undefined if the
# condition was not met at the end of the code string.
consume = (code, start, condition) ->
  for index in [start..code.length - 1]
    next  = code[index + 1] if index <= (code.length - start)
    last  = code[index]
    lastN = code[(index - start)..index]

    return index if condition last, lastN, next, index, code

  return undefined

# Consume one character at a time, either into a previous token of type invalid
# or into a new one. If consecutive invalid characters are found, the previous
# token is updated with the new end position and value.
#
# This function does *not* check boundaries of the parameters. This is to be
# done by the tokenize function.
consumeUnknown = (stream) ->
  [..., previous] = stream.tokens

  if previous.type is 'unknown'
    previous.end.index  += 1
    previous.end.column += 1
    previous.value      += stream.code[0]

    return Instance stream, { code: stream.code[1..] }

  [start, end] = advance previous, stream.code[0]

  token = Instance Token, previous, {
    start: start
    end:   end
    type:  'unknown'
    value: stream.code[0]
  }

  {
    code:   stream.code[1..]
    tokens: [stream.tokens..., token]
  }


module.exports = {
  inline
  block

  advance
  advanceBlock
  blockEndLocation
  consume
  consumeUnknown

  Location
  Token
  TokenStream

  ACCESSORS
  ACCESSOR_TYPE
  BLOCK_COMMENT
  BLOCK_STRING
  BOOL
  BRACKET_TYPE
  COMMENT
  COMMA
  COMPARE
  COMPOUND_ASSIGN
  FUNCTION
  IDENTIFIER
  INLINE_STRING_QUOTES
  LOGIC
  SHIFT
  MATH
  NEWLINE
  NUMBER
  OPERATOR
  RANGES
  REGEX
  REGEX_MODIFIER
  RELATION
  STATEMENTS
  UNARY
  UNARY_MATH
  WHITESPACE
}
