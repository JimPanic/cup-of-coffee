
```coffee
# Let the lexer analyse itself.
fs     = require 'fs'
coffee = require './src/lexer'

filename = 'src/lexer'
code     = fs.readFileSync("#{filename}.coffee").toString()
tokens   = coffee.lexer code, filename

shortTokens = tokens.map (token) ->
  [
    token.type
    token.start.line, token.end.line
    token.value.replace(/\n/g, '\\n').replace(/\s/g, '˽')
  ]

# Show one token per line, its type, start and end position as well as the
# value (with whitespaces replaced by demonstrating special characters)
process.stdout.write JSON.stringify shortTokens

# Show each token's value (reconstruct the file)
#process.stdout.write JSON.stringify tokens
#tokens.forEach (token) ->
#  process.stdout.write "#{token.value}"
```
