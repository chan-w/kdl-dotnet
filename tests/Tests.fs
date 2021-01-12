module Tests

open System
open Xunit
open FParsec
open KDL.Parser

/// Helper functions for tests
module ParserTestHelp =
    let parseStringWithP p str =
        runParserOnString p UserState.Default "" str
        
    let parsed p str = 
        match parseStringWithP p str with
        | Success(_) -> true
        | Failure(_) -> false

    let sucessResult p str =
        match parseStringWithP p str with
        | Success(result, _, _) -> Some result
        | Failure(_) -> None

    let parseSuccess parseFunction input =
        match (parseFunction input) with
        | Success (result, _, _) -> (true, result)
        | Failure(errMsg, _, _) -> (false, errMsg)

    let rec KDLEqual k1 k2 =
        match k1 with
        | KRawString (s) | KString(s) -> (match k2 with 
                                            | KString(s2) | KRawString (s2) -> s.Equals(s2)
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
        | KNode (name, l, m, c) -> (match k2 with
                                    | KNode (name2, l2, m2, c2) -> name = name2 && (List.forall2 KDLEqual l l2) && (List.forall2 KDLEqual c c2) && (List.forall2 (fun (k, v) (k2, v2) -> k = k2 && (KDLEqual v v2)) (Map.toList(m)) (Map.toList(m2)))
                                    | _ -> false)
        | KDoc (l) -> (match k2 with 
                        | KDoc(l2) -> List.forall2 KDLEqual l l2
                        | _ -> false)
                        
    let compareResult parser input s =
        match parseStringWithP parser input with
        | Success(res, _, _) -> (match res with 
                                | (KString sr) |  (KRawString sr) |  (KIdentifier sr) -> Assert.Equal(s, sr)
                                |  _ -> failwith "Returned non-string result" )
        | Failure(errMsg, _, _) -> failwith errMsg

open ParserTestHelp

[<Fact>]
let ``knull parses null`` () =
    Assert.True(parsed knull "null")

[<Fact>]
let ``kidentifier does not parse first character decimal digit`` () =
    Assert.False(parsed kidentifier "0abc")

[<Fact>]
let ``kstring four character unicode escape`` () =
    compareResult kstring "\"\u00E7\u00E7 \u0ACE\"" (("\u00E7" + "\u00E7 ") + "\u0ACE")

(* Fails with variable number of hexidecimal digits*)
[<Fact>]
let ``kstring two character unicode escape`` () =
    compareResult kstring "\"\uE7\uA8\"" (("\xE7") + "\xA8")

[<Fact>]
let ``kstring whitespace`` () =
    let a = """abc\n\
             as     \t\n\r\
             w"""
    compareResult kstring ("\"" + (a + "\"")) (a)

[<Fact>]
let ``raw string (r#")`` () =
    let a = """r#" text \n\n  "#"""
    compareResult krawstring a """ text \n\n  """

[<Fact>]
let ``raw string (r#") containing quote (")`` () =
    let a = """r#" text "  "#"""
    compareResult krawstring a """ text "  """

[<Fact>]
let ``raw string (r#") containing quote with two hashes ("##)`` () =
    let a = """r#" text " "## "#"""
    compareResult krawstring a """ text " "## """

[<Fact>]
let ``raw string (r##") containing quote with one hash ("#)`` () =
    let a = """r##" text \"# "#" \ "## \"##"""
    compareResult krawstring a  """ text \"# "#" \" """

   
[<Fact>]
let ``hexadecimal underscore`` () =
    match parseStringWithP kint "0xABC_123" with
    | Success(KInt(r), _, _) -> Assert.True((r = (int64 0xABC123)))
    | Success(_) -> failwith "0xABC_123 not parsed as integer"
    | Failure (errMsg, _, _) -> failwith errMsg

[<Fact>]
let ``ident raw string`` () =
    match parseStringWithP ident "r\"myidentifier\"" with
    |Success(r, _, _) -> Assert.Equal("myidentifier", r)
    |Failure(r, _, _) -> Assert.Equal("myindentifier", r)

[<Fact>]
let ``slapdash prop-and-args`` () =
    match (parseStringWithP KDLDocument "a /- b=\"cat\" \"c\"") with
    | Success(k, _, _) -> Assert.True(KDLEqual k (KDoc([KNode ("a", [KString "c"], Map [], [])])))
    | Failure(errMsg, _, _) -> failwith errMsg

[<Fact>]
let ``open/close comment`` () =
    let doc = """tag /*foo=true*/ bar=false"""
    match (parseStringWithP KDLDocument doc) with
    | Success(k, _, _) -> Assert.True(KDLEqual k (KDoc [KNode ("tag", [], Map([("bar", KBool false)]), [])]))
    | Failure(errMsg, _, _) -> failwith errMsg

[<Fact>]
let ``two open/close comments`` () =
    let doc = """tag /*foo=true*/ bar=false "s1" /*"s2"*/ "s3" """
    match (parseStringWithP KDLDocument doc) with
    | Success(k, _, _) -> Assert.True(KDLEqual k (KDoc [KNode ("tag", [KString ("s1"); KString ("s3")], Map([("bar", KBool false)]), [])]))
    | Failure(errMsg, _, _) -> failwith errMsg

[<Fact>]
let ``int and float`` () =
    let doc = """a 5.5 1 n=55.5 0b10001 0x43245 0890.8 1"""
    match (parseStringWithP KDLDocument doc) with
    | Success(k, _, _) -> Assert.True(KDLEqual k (KDoc [KNode ("a", [KFloat (5.5); KInt ((int64 1)); KInt 17L; KInt 275013L; KFloat 890.8; KInt 1L], Map([("n", KFloat 55.5)]), [])]))
    | Failure(errMsg, _, _) -> failwith errMsg