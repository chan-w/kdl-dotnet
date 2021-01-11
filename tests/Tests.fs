module Tests

open System
open Xunit
open FParsec
open KDL.Parser
open KDL.ParserTestHelp
 (*
let KDLEqual k1 k2 =
    match k1 with
    | KRawString (s) -> (match k2 with 
                        | KRawString (s2) -> s.Equals(s2)
                        | _ -> false)
    | KInt (n) -> (match k2 with
                    | KInt (n2) -> n = n2
                    | _ -> false)
    | KFloat (f) -> (match k2 with
                    | KFloat(f2) -> (abs (f - f2)) < 0.5 (* Set a threshold for considering floats equal*)
                    | _ -> false)
    | KBool (b) -> (match k2 with
                    | KBool(b2) -> b = b2
                    | _ -> false)
    | KNull -> (match k2 with 
                |KNull -> true
                | _ -> false)
    | KIdentifier (s) -> (match k2 with
                           | KIdentifier(s2) -> true
                           | _ -> false)
    | KNode (name) *)


 
[<Fact>]
let ``knull parses null`` () =
    Assert.True(parsed knull "null")

(* kidentifier parses one character with XID_Start property and zero or more characters with XID_Continue property using default settings*)
[<Fact>]
let ``kidentifier does not parse first character decimal digit`` () =
    Assert.False(parsed kidentifier "0abc")

let compareResult parser parsestring s =
    match (sucessResult parser parsestring) with
    | Some (KString sr) | Some (KRawString sr) | Some (KIdentifier sr) ->  Assert.Equal(s, sr)//Assert.True( s.Equals(s)
    | None -> (printfn "Failed to parse"); Assert.False(true)
    | Some(value) -> failwith "Not Implemented"

[<Fact>]
let ``kstring UTF-16 unicode escape`` () =
    (*match (sucessResult kstring "\"\u00E7\u00E7\"") with
    | Some (KString s) ->  Assert.True( s.Equals("\u00E7" + "\u00E7"))
    | None -> (printfn "Failed to parse"); Assert.False(true)
    | Some(value) -> failwith "Not Implemented"*)
    compareResult kstring "\"\u00E7\u00E7 \u0ACE\"" (("\u00E7" + "\u00E7 ") + "\u0ACE")

(* Fails with variable number of hexidecimal digits*)
(*[<Fact>]
let ``kstring UTF-8 unicode escape`` () =
    compareResult kstring "\"\uE7\uA8\"" (("\xE7") + "\xA8")
    *)
(*
[<Fact>]
let ``kstring whitespace`` () =
    let s = @"abc\n\
             as     \t\n\r\
             w "
    compareResult kstring ("\"" + (s + "\"")) (s)
*)
[<Fact>]
 let ``id raw string`` () =
    match run ident "r\"myidentifier\"" with
    |Success(r, _, _) -> Assert.Equal("myidentifier", r)
    |Failure(r, _, _) -> Assert.Equal("myindentifier", r)

 (*
[<Fact>]
let ``parse single node`` () = 
    Assert*)
// run knodereal "foo 1 \"hello\" key=\"val\" 3 \"goodbye\" 600 -1 {a {b \"b\" {c null}}}";;