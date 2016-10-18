## Helper functions

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

# Utility function to split up a string by whitespaces. Used for all the
# following literals. Similar to `%w()` in Ruby.`
#
# Takes a string and returns an array with each word as an element.
words = (string) -> string.split(/\s/).filter (string) -> string isnt ''

# Function that always returns a new "instance" of the parameters passed. This
# is only to be used with and for data objects. (I don't know what happens if
# you pass more complex objects to it.)
Instance = (args...) -> Object.assign {}, args...

# Remove BOM from the beginning if there is one
stripBOM = (code) ->
  return code.slice(1) if code.charCodeAt(0) is BOM
  code

# Remove spaces
stripTrailingSpaces  = (code) -> code.replace(TRAILING_SPACES, '')
stripCarriageReturns = (code) -> code.replace(/\r/g, '')

clean = (code) ->
  # stripTrailingSpaces
  stripCarriageReturns stripBOM code

module.exports = {
  initialToken
  clean
  Instance
  words
}
