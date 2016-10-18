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
added.

Example:

# Given data
stream = { code: 'a = 1 + 2', tokens: [{initial token}]}

# Call highest precedence tokenize function (`identifiers`) with given data:

identifiers(stream)

# will return the following (new) TokenStream object:
# => {
#           # .-- already analyzed tokens ('a') are removed from the start
#      code: ' = 1 + 2'
#      tokens: [{initial token}, {identifier token for 'a'}]
                                # ^-- new token is added to the list
#    }

Example:

# If a tokenize function has not found a match, it will return the input stream
# as-is:

stream = { code: 'a = 1 + 2', tokens: [...]}

# There is no number at the beginning of the stream's code. So it will return
# its input immediately.

stream == numbers(stream)
# => true

###

Analysis = require './analysis'
Helper   = require './helper'

{inline, block, consume}       = Analysis
{Token, Location, TokenStream} = Analysis
{Instance, words, clean}       = Helper
{
  ACCESSORS
  ACCESSOR_TYPE
  BLOCK_COMMENT
  BLOCK_STRING
  BOOL
  BRACKET_TYPE
  COMMA
  COMMENT
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
} = Analysis

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

accessorType = (type) -> ACCESSOR_TYPE[type]

lines = (stream) ->
  newStream = inline('newline', NEWLINE)(stream)

  return stream if newStream is stream

  [..., token] = newStream.tokens

  token.end.line  += 1
  token.end.column = 0

  newStream

stringCondition = (quote) -> (stream) -> stream.code[0] is quote
stringConsumer  = (quote) -> (last, lastN, _, index, code) ->
  last is quote and (
    lastN                      isnt '\\' + quote or
    code[index - 2..index - 1] is   '\\\\')

stringTokenizer = Analysis.INLINE_STRING_QUOTES.map (quote) ->
  block \
    'string-literal', 1,
    stringConsumer(quote),
    stringCondition(quote)


identifiers   = inline identifierType,   IDENTIFIER
operators     = inline operatorType,     OPERATOR
comments      = inline 'comment',        COMMENT
numbers       = inline 'number',         NUMBER
whitespaces   = inline 'whitespace',     WHITESPACE
regexModifier = inline 'regex-modifier', REGEX_MODIFIER
commas        = inline 'comma',          COMMA
functions     = inline 'function-arrow', FUNCTION
accessors     = inline accessorType,     ACCESSORS

strings = stringTokenizer.reduce.bind(stringTokenizer, ((seed, fn) -> fn seed))

blockStrings = block \
  'block-string', 3,
  ((_, __, ___, index, code) -> code[(index - 2)..index] in BLOCK_STRING),
  ((stream)   -> stream.code[..2] in BLOCK_STRING)

blockComments = \
  block 'multi-line-comment', 3,
         ((_, __, next, index, code) -> \
            code[index - 2..index] is '###' and next isnt '#'),
         ((stream) -> stream.code[..2] is '###')

javascript = \
  block 'javascript', 1,
         ((last, lastN) -> last is '`' and lastN isnt '\\`'),
         ((stream) -> stream.code[0] is '`')

regexConsumer = (last, lastN, _, index, code) ->
  last is '/' and
  (lastN isnt '\\/' or code[index - 2..index - 1] is '\\\\/')

regexNoModifier = \
  block 'regex', 1, regexConsumer,
        ((stream) -> (REGEX.exec stream.code)? )

blockRegexNoModifier = \
  block 'block-regex', 3,
         ((_, ___, next, index, code) -> code[index - 2..index] is '///')
         ((stream)                    -> stream.code[..2] is '///')

regex      = (stream) -> regexModifier regexNoModifier stream
blockRegex = (stream) -> regexModifier blockRegexNoModifier stream

# Main entry point for the lexer
#
# Takes a string and an optional filename, returns an array of token objects.
lexer = (code, filename) ->
  stream = tokenize Helper.initialToken code, filename
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
      newStream = Analysis.consumeUnknown oldStream

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
}
