namespace KDL
open FParsec

module ParserTestHelp =
    let parsed p str = 
        match run p str with
        | Success(_) -> true
        | Failure(_) -> false
    let sucessResult p str =
        match run p str with
        | Success(result, _, _) -> Some result
        | Failure(_) -> None

module Parser =
    (* Solve issues caused by value restriction: www.quanttec.com/fparsec/tutorial.html#fs-value-restriction *)
    type DummyState = unit
    type Parser<'t> = Parser<'t, DummyState>

    type UserState = {RawStringStack: char list}
                        with
                        static member Default = {RawStringStack = []}

    type KDL = KString of string
                 | KRawString of string
                 | KInt of int64 (* could consider using only one number type *)
                 | KFloat of float
                 | KBool of bool
                 | KNull
                 | KIdentifier of string (* Assume only identifiers can be the LHS of properties and the names of nodes *)
                 | KNode of Map<string, (string list) * Map<string, KDL> * KDL> (* KNode has an indentifier and arguments (an ordered list), Properties (a key:value pairing), and a child block *)

    let knull : Parser<_> = stringReturn "null" KNull
    let ktrue : Parser<_> = stringReturn "true" (KBool true)
    let kfalse : Parser<_> = stringReturn "false" (KBool false)
    let kint : Parser<_> = pint64 |>> KInt
    let kfloat : Parser<_> = pfloat |>> KFloat
    
    let str s = pstring s;
    let id : Parser<_> = 
        identifier (IdentifierOptions(
                        normalization = System.Text.NormalizationForm.FormKC
                    )
        )
    let kidentifier = id |>> KIdentifier

    (* Taken from https://www.quanttec.com/fparsec/tutorial.html#parsing-json *)
    let stringLiteral =
        let escape =  anyOf "\"\\bfnrt"
                      |>> function
                          | 'b' -> "\b"
                          | 'f' -> "\u000C"
                          | 'n' -> "\n"
                          | 'r' -> "\r"
                          | 't' -> "\t"
                          | c   -> string c // every other char is mapped to itself

        let unicodeEscape =
        // converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
            let hex2int c = (int c &&& 15) + (int c >>> 6)*9

            str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                |> char |> string
            )

        let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
        let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')

        between (str "\"") (str "\"")
                (stringsSepBy normalCharSnippet escapedCharSnippet)
    
    let kstring : Parser<_> = stringLiteral |>> KString
    
    (* KRawString, KNode (Properties, Arguments), Comments *)

    //let invalid_identifier_character c = function
      //                                  | 
    //let IDOptions = IdentifierOptions(

   // )


    
    //let 
    (* 
    r"..."
    r#"..."# 
    r##"..."##*)
  //r#"a"##"#
    // Count number of opening #'s and store using UserState
    // When encountering a quote, if the next n characters (from UserState) are #, then consume (n + 1) characters and stop parsing
    // In F#, prefix with @, @"..." for a unicode verbatim string
    let rawString : Parser<_> =
        between (str "r\"") (str "\"") (manyChars anyChar)


   (*type Node = *)