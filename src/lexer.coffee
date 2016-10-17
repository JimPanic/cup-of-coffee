###

A set of functions to tokenize a string.

`lexer` is meant to be the entry point to analyse a string. It takes two
parameters, a string containing code and an associated (file) name, and returns
the created set of tokens.

The set of tokens returned by `lexer` is aggregated in `tokenize`, which is
called initially with the result of `initialToken`. This is necessary for the
subsequent tokenize functions as they all need a reference to the previous token
and its location information. `initialToken` takes care of this by providing the
location zero and an empty token holding that information.

All the tokenize functions adhere to the signature `TokenStream` ->
`TokenStream` in any case. The basic trail of execution is as follows:

 - take stream and see if a token of form X can be found at its beginning
 - return the stream as-is if not
 - form a new token with location information based on the last token of the
   stream and the matched data in code
 - return a new stream object containing the new set of tokens and the code
   without the newly generated token's value

`tokenize` calls each tokenize function in a specific order, compares their
argument with the return value and restarts that process if a token has been
added. (This was recursive but node unfortunately does not support TCO yet.
That's why the `tokenize` function looks so awful - it's mimicking recursion
ugh.)

###

F          = require 'fundament/lib/function'
A          = require 'fundament/lib/array'
{isString} = require 'fundament/lib/types'

Sequence = F.sequence

Token = {
  type:   undefined
  value:  undefined
  length: undefined
  start:  undefined
  end:    undefined
}

Location = {
  file:   undefined
  line:   0
  column: 0
  index:  0
}

TokenStream = {
  code:   undefined
  tokens: []
}

Tokenizer = (type, expression) ->
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

# This is supposed to be(come) the generalized version of the tokenize functions
# for blocks.
BlockTokenizer = (type, offset, fn, condition) ->
  (stream) ->
    return stream unless condition stream

    endIndex = offset + consume stream.code[offset - 1..], 1, fn

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

# Returns what type of identifier given token is based on current and previous
# token type/value.
#
# NOTE: This may not be necessary here, but since it is mostly not context
#       sensitive, there is no harm done keeping it here.
identifierType = (identifier, previous) ->
  types_as_is = RELATION.concat words 'unless import export when'

  return 'unary'     if identifier in UNARY
  return 'logic'     if identifier in LOGIC
  return 'bool'      if identifier in BOOL
  return 'statement' if identifier in STATEMENTS
  return identifier  if identifier in types_as_is

  'identifier'

lines = (stream) ->
  return stream unless stream.code[0] is '\n'

  [..., previous] = stream.tokens
  [start, end]    = advance previous, stream.code[0]

  end.line  += 1
  end.column = 0

  token = Instance Token, previous, {
    start
    end
    type:   'newline'
    value:  stream.code[0]
    length: 1
  }

  {
    code:   stream.code[1..]
    tokens: [stream.tokens..., token]
  }

strings = (stream) ->
  # Make sure blockStrings is called *before* strings. strings will not take
  # triplets into account.
  return stream unless stream.code[0] in INLINE_STRING_QUOTES

  quote         = stream.code[0]
  escaped_quote = '\\' + stream.code[0]

  endIndex = consume stream.code, 1, (last, lastN, next, index, code) ->
    last is quote and (
      lastN                      isnt escaped_quote or
      code[index - 2..index - 1] is   '\\\\'
    )

  return stream unless endIndex?

  endLocation     = blockEndLocation stream.code[..endIndex]
  [..., previous] = stream.tokens
  [start, end]    = advanceBlock previous, endLocation

  token = Instance Token, previous, {
    start
    end
    type:   'string-literal'
    value:  stream.code[..endIndex]
    length: endIndex + 1
  }

  {
    code:   stream.code[token.length..]
    tokens: [stream.tokens..., token]
  }

blockStrings = (stream) ->
  return stream unless stream.code[..2] in BLOCK_STRING

  endIndex = consume stream.code, 3, (last, lastN) -> lastN in BLOCK_STRING

  return stream unless endIndex?

  endLocation     = blockEndLocation stream.code[..endIndex]
  [..., previous] = stream.tokens
  [start, end]    = advanceBlock previous, endLocation


  token = Instance Token, previous, {
    start
    end
    type:   'block-string'
    value:  stream.code[..endIndex]
    length: end.index - start.index
  }

  {
    code:   stream.code[token.length..]
    tokens: [stream.tokens..., token]
  }

accessors = (stream) ->
  isAccessor = (stream.code[0]   is '.'    or
                stream.code[0]   is '@'    or
                stream.code[..1] is '::')  and
               (stream.code[..1] isnt '..' and
                stream.code[..2] isnt '...')

  return stream unless isAccessor

  [..., previous] = stream.tokens
  [start, end]    = advance previous, stream.code[0]

  value  = stream.code[0]
  value += stream.code[1] if stream.code[..1] is '::'

  token = Instance Token, previous, {
    start
    end
    type:   ACCESSOR_TYPE[value]
    value:  value
    length: value.length
  }

  {
    code:   stream.code[value.length..]
    tokens: [stream.tokens..., token]
  }

operatorType = (operator, previous) ->
  return 'colon'                if operator is ':'
  return 'terminator'           if operator is ';'
  return 'asterisk'             if operator is '*'
  return 'existential'          if operator is '?'
  return 'suppress-newline'     if operator is '\\'
  return 'assignment'           if operator is '='
  return 'compare'              if operator in COMPARE
  return 'compound-assignment'  if operator in COMPOUND_ASSIGN
  return 'math'                 if operator in MATH
  return 'unary'                if operator in UNARY
  return 'unary-math'           if operator in UNARY_MATH
  return 'shift'                if operator in SHIFT
  return 'logic'                if operator in LOGIC
  return BRACKET_TYPE[operator] if operator in Object.keys BRACKET_TYPE
  return 'splat-or-range'       if operator in Object.keys RANGES

  undefined

## Helper functions

# Function that always returns a new "instance" of the parameters passed. This
# is only to be used with and for data objects. (I don't know what happens if
# you pass more complex objects to it.)
Instance = (args...) -> Object.assign {}, args...

# Remove BOM from the beginning if there is one
stripBOM = (code) ->
  return code.slice(1) if code.charCodeAt(0) is BOM
  code

# Remove trailing spaces (NOTE: Why exactly is this necessary?)
stripTrailingSpaces = (code) -> code.replace(TRAILING_SPACES, '')

# Remove carriage returns
stripCarriageReturns = (code) -> code.replace(/\r/g, '')

clean = Sequence stripBOM,
                 stripTrailingSpaces,
                 stripCarriageReturns

# Utility function to split up a string by whitespaces. Used for all the
# following literals. Similar to `%w()` in Ruby.`
#
# Takes a string and returns an array with each word as an element.
words = (string) -> string.split(/\s/).filter (string) -> string isnt ''

# Make sure there is always a token to get initial location data from in all
# other tokenizing functions.
#
# Takes a string and a filename, returns a tuple of the form `[code, tokens...]`
initialToken = (code, filename) ->
  location = {
    file:   filename
    index:  0
    column: 1
    line:   1
  }

  token = Instance Token, {
    start:  Instance Location, location
    end:    Instance Location, location
    type:   'initial'
    value:  ''
    length: 0
  }

  { code: code, tokens: [token] }

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

## Constants

BOM             = 65279
TRAILING_SPACES = /\s+$/

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

identifiers   = Tokenizer identifierType,   IDENTIFIER
operators     = Tokenizer operatorType,     OPERATOR
comments      = Tokenizer 'comment',        COMMENT
numbers       = Tokenizer 'number',         NUMBER
whitespaces   = Tokenizer 'whitespace',     WHITESPACE
regexModifier = Tokenizer 'regex-modifier', REGEX_MODIFIER
commas        = Tokenizer 'comma',          COMMA
functions     = Tokenizer 'function-arrow', FUNCTION

blockComments = \
  BlockTokenizer 'multi-line-comment',
                 3,
                 ((_, __, next, index, code) -> \
                    code[index - 2..index] is '###' and next isnt '#'),
                 ((stream) -> stream.code[..2] is '###')

javascript = \
  BlockTokenizer 'javascript',
                 1,
                 ((last, lastN) -> last is '`' and lastN isnt '\\`'),
                 ((stream) -> stream.code[0] is '`')

regexConsumer = (last, lastN, _, index, code) ->
  last is '/' and
  (lastN isnt '\\/' or code[index - 2..index - 1] is '\\\\/')

regexNoModifier = BlockTokenizer \
  'regex',
  1,
  regexConsumer,
  ((stream) -> (REGEX.exec stream.code)? )

blockRegexConsumer = (_, ___, next, index, code) ->
  code[index - 2..index] is '///'

blockRegexNoModifier = \
  BlockTokenizer 'block-regex',
                 3,
                 blockRegexConsumer,
                 ((stream) -> stream.code[..2] is '///')

regex      = (stream) -> regexModifier regexNoModifier stream
blockRegex = (stream) -> regexModifier blockRegexNoModifier stream

# Main entry point for the lexer
#
# Takes a string and an optional filename, returns an array of token objects.
lexer = (code, filename) ->
  stream = tokenize initialToken code, filename
  stream.tokens

# Turn code and a stream of tokens into a stream of tokens recursively
# tokenizing the string.
#
# This function will call all tokenize functions in specific order and then
# itself until no more code is available or a complete lexing pass has not
# changed the string to analyse.
tokenize = (stream) ->
  newStream = stream

  while newStream.code.length
    [..., last] = stream.tokens
    oldStream = newStream

    # This is a hack. But it does work for now.
    newStream = identifiers oldStream;   continue if newStream isnt oldStream
    newStream = blockComments newStream; continue if newStream isnt oldStream
    newStream = comments newStream;      continue if newStream isnt oldStream
    newStream = whitespaces newStream;   continue if newStream isnt oldStream
    newStream = lines newStream;         continue if newStream isnt oldStream
    newStream = blockStrings newStream;  continue if newStream isnt oldStream
    newStream = strings newStream;       continue if newStream isnt oldStream
    newStream = numbers newStream;       continue if newStream isnt oldStream
    newStream = blockRegex newStream;    continue if newStream isnt oldStream
    newStream = regex newStream;         continue if newStream isnt oldStream
    newStream = javascript  newStream;   continue if newStream isnt oldStream
    newStream = functions newStream;     continue if newStream isnt oldStream
    newStream = commas newStream;        continue if newStream isnt oldStream
    newStream = operators newStream;     continue if newStream isnt oldStream
    newStream = accessors newStream;     continue if newStream isnt oldStream

    # No tokenize function found a valid string. Consume one character at a time
    # into a token with type 'unknown' until a valid one is found or the end of
    # the source has been reached.
    if newStream.tokens.length is oldStream.tokens.length
      newStream = consumeUnknown oldStream

  newStream

module.exports = {
  lexer
  tokenize

  identifiers
  identifierType
  blockComments
  comments
  whitespaces
  lines
  blockStrings
  strings
  numbers
  blockRegex
  regex
  javascript
  functions
  commas
  operators
  operatorType
  accessors

  Token
  Location
  TokenStream

  IDENTIFIER
  OPERATOR
  UNARY
  UNARY_MATH
  LOGIC
  SHIFT
  COMPARE
  MATH
  RELATION
  BOOL
  COMMENT
  BLOCK_COMMENT
  INLINE_STRING_QUOTES
  STATEMENTS
  WHITESPACE

  helper: {
    clean
    stripBOM
    stripTrailingSpaces
    stripCarriageReturns
    words
    initialToken
    advance
    advanceBlock
    blockEndLocation
    consumeUnknown
  }
}
