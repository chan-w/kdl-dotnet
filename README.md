# kdl-dotnet

A parser for the [KDL Document Language](https://kdl.dev/) written in F# using [FParsec](https://www.quanttec.com/fparsec/).

## Status
This is a work in progress. It seems to parse most of the files in ```examples/``` correctly, except ```multiline_comment.kdl```
Things that don't work correctly include: 
- strings with unicode escapes that have more or fewer than 4 characters
- multiline comments (only parses the first node of ```multiline_comment.kdl```)

## Usage
```F#
open KDL

// Parse a file
Parser.parseFile "examples/small.kdl"
// Parse a string
Parser.parseString "a /- b=\"cat\" \"c\""