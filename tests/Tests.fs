module Tests

open System
open Xunit
open KDL.Parser
open KDL.ParserTestHelp

[<Fact>]
let ``knull parses null`` () =
    Assert.True(parsed knull "null")

(* kidentifier parses one character with XID_Start property and zero or more characters with XID_Continue property using default settings*)
[<Fact>]
let ``kidentifier does not parse first character decimal digit`` () =
    Assert.False(parsed kidentifier "0abc")

let compareKStringResult parser parsestring s =
    match (sucessResult parser parsestring) with
    | Some (KString s) ->  Assert.True( s.Equals(s))
    | None -> (printfn "Failed to parse"); Assert.False(true)
    | Some(value) -> failwith "Not Implemented"

[<Fact>]
let ``kstring UTF-16 unicode escape`` () =
    (*match (sucessResult kstring "\"\u00E7\u00E7\"") with
    | Some (KString s) ->  Assert.True( s.Equals("\u00E7" + "\u00E7"))
    | None -> (printfn "Failed to parse"); Assert.False(true)
    | Some(value) -> failwith "Not Implemented"*)
    compareKStringResult kstring "\"\u00E7\u00E7 \u0ACE\"" (("\u00E7" + "\u00E7 ") + "\u0ACE")

(* Fails with variable number of hexidecimal digits*)
[<Fact>]
let ``kstring UTF-8 unicode escape`` () =
    compareKStringResult kstring "\"\uE7\uA8\"" (("\xE7") + "\xA8")

[<Fact>]
let ``kstring whitespace`` () =
    let s = "abc\n\
             as     \t\n\r\
             w "
    compareKStringResult kstring ("\"" + (s + "\"")) (s)