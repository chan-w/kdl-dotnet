namespace KDL
open FParsec

module Parser =
    (* Solve issues caused by value restriction: www.quanttec.com/fparsec/tutorial.html#fs-value-restriction *)
    //type UserState = {Hashes: char list} with static member Default = {Hashes=[]}
    type UserState = {Hashes: string} with static member Default = {Hashes=""}
    //type UserState = {Hashes: int} with static member Default = {Hashes=0}

    // type DummyState = unit
    type Parser<'t> = Parser<'t, UserState>

    let str s = pstring s
    let isUnicodeSpace = (function '\u0009'|'\u0020'|'\u00A0'|'\u1680'|'\u2000'|'\u2001'|'\u2002'|'\u2003'|'\u2004'|'\u2005'|'\u2006'|'\u2007'|'\u2008'|'\u2009'|'\u200A'|'\u202F'|'\u205F'|'\u3000' -> true | _ -> false)
    /// FParsec converts \r and \n\r to \n: https://www.quanttec.com/fparsec/reference/charparsers.html#members.manySatisfy
    let isUnicodeNewline = (function '\u000A'|'\u0085'|'\u000C'|'\u2028'|'\u2029' -> true |_ -> false)
    // let multilineComment : Parser<_> = (str "/*") >>. skipManyTill anyChar (str "*/")
    let isBom x = x = '\uFEFF'
    /// https://stackoverflow.com/a/8405610
    let rec multilinecomment o =
        let ign x = skipCharsTillString x false System.Int32.MaxValue
        let commentOpen = "/*"
        let commentClose = "*/"
        between
            (str commentOpen)
            (str commentClose)
            // Use userstate to prevent it from skipping over the commentClose to the next comment open
            (attempt (ign commentOpen >>. multilinecomment >>. ign commentClose) <|> ign commentClose) <| o
            
    /// Based on KDL specification
    let ws:Parser<_> = (attempt multilinecomment) <|> (skipManySatisfy (fun c -> (isUnicodeSpace c) || (isBom c)))
    let ws1 : Parser<_> = multilinecomment <|> (skipMany1Satisfy isUnicodeSpace)
    let newline:Parser<_> = manySatisfy isUnicodeNewline
    let newlineSkip = skipManySatisfy isUnicodeNewline //newline |>> ignore
    let newlineSingleSkip:Parser<_> = skipSatisfy isUnicodeNewline
    let singleLineComment : Parser<_> = (str "//") >>. skipRestOfLine false >>. newlineSkip// |>> ignore
    let singleLineCommentStr = (str "//") .>> skipRestOfLine false .>> newlineSkip 
    // let singleLineComment : Parser<_> = (many( (str "//") >>. skipRestOfLine true)) |>> ignore
    //let linespace = ws >>. (attempt singleLineComment) <|> (attempt newlineSkip) <|> ws .>> ws
    // let linespace = ws >>. (attempt singleLineComment) <|> (attempt newlineSkip) <|> ws// .>> ws
    let linespace = (attempt singleLineComment) <|> (attempt newlineSingleSkip) <|> ws1
    let escline = (str "\\") >>. ws .>> ((attempt singleLineComment) <|> newlineSingleSkip)
    let nodeSpace = ws1 <|> (ws .>> escline .>> ws)
    
    type KDL = KString of string
                 | KRawString of string
                 | KInt of int64 (* Add a Number type that is a union of int, float, and bigint *)
                 | KFloat of float
                 | KBool of bool
                 | KNull
                 | KIdentifier of string
                 | KNode of string * KDL list * Map<string, KDL> * KDL list (* Name, arguments, properties, and a child block *)
                 | KDoc of KDL list (* Document contains 0 or more nodes*)

    type NodeUserState = {Name: string
                          Arguments: KDL list
                          Properties: Map<string, KDL>
                          Children: KDL list} with
                      static member Default = {Name="";
                                                Arguments=[];
                                                Properties=Map.empty<string, KDL>;
                                                Children=[]}


    let knull : Parser<_> = stringReturn "null" KNull
    let ktrue : Parser<_> = stringReturn "true" (KBool true)
    let kfalse : Parser<_> = stringReturn "false" (KBool false)
    /// TODO: Handle _'s in binary/hexadecimal/octal integers and fail when encountering a decimal point (.)
    let kint : Parser<_> = pint64 |>> KInt
    let kfloat : Parser<_> = pfloat |>> KFloat
    

    (* Taken from https://www.quanttec.com/fparsec/tutorial.html#parsing-json *)
    /// TODO: Handle unicode escape of 1 to 6 charaacters
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
    

    /// TODO: Consider #'s
    let rawString : Parser<_> =
    (* 
    r"..."
    r#"..."# 
    r##"..."##*)
  //r#"a"##"#
    // Count number of opening #'s and store using UserState
    // When encountering a quote, if the next n characters (from UserState) are #, then consume (n + 1) characters and stop parsing
    // In F#, prefix with @, @"..." for a unicode verbatim string or try using """ """, backslash line continuation should also work
        // Set a list of hashes in UserState when reading the open quote
        // Remove the hashes until empty and the next character is not a hash, such that the parser has encountered exactly as many hashes as in userstate, otherwise fail
        (*let setHash s = updateUserState (fun ustate -> {ustate with Hashes=(Seq.toList s)})
        let popHash = updateUserState (fun ustate -> {ustate with Hashes=List.tail ustate.Hashes})
        let emptyHash = userStateSatisfies (fun ustate -> match ustate with [] -> true | _ -> false) 
        let rawStringOpen = (str "r") >>. (manySatisfy (fun c -> c = '#') .>> (str "\"")) |>> setHash
        let rawStringClose = (str "\"") >>. *)
        let setHash s = updateUserState (fun ustate -> {ustate with Hashes=s})
        let matchHash s = userStateSatisfies (fun ustate -> String.length ustate.Hashes = String.length s) .>> nextCharSatisfiesNot (fun c -> c = '#')
        let rawStringOpen = (str "r") >>. (manySatisfy (fun c -> c = '#') .>> (str "\"")) |>> setHash
        let rawStringEnd = (str "\"") >>. manyChars (pchar '#') |>> matchHash
        let rawStringRest = manyCharsTill anyChar rawStringEnd
        // need to get the result from rawStringOpen
        (*let rawStringClose = (str "\"") 
        between (str "r\"") (str "\"") (manyChars (noneOf "\""))*)
        rawStringOpen >>. rawStringRest

        
    let krawstring = rawString |>> KString
    
    let ident : Parser<_> = 
        // TODO: check that c < 0x10FFFF
        let validIdentifierCharacter c = (c > '\u0020') && (c<>'\"') && (c <> '\\') && (c <> '<')&&(c <> '>')&&(c <> '{')&&(c <> '}')&&(c <> ';')&&(c <> '[')&&(c <> ']')&&(c <> '=')
        let validInitialCharacter c = validIdentifierCharacter c && (c < '0' || c > '9')
        let bare = identifier (IdentifierOptions(
                                isAsciiIdStart = validInitialCharacter,
                                isAsciiIdContinue = validIdentifierCharacter,
                                normalization = System.Text.NormalizationForm.FormKC
                    )
        )
        (* rawString can start with r, which would be a valid part of a bare identifier*)
        (attempt rawString) <|> bare <|> stringLiteral
        // bare <|> stringLiteral <|> rawString

    let kidentifier = ident |>> KIdentifier

    (* KRawString, Comments *)
    let value = choice [krawstring
                        kstring
                        kint
                        kfloat
                        ktrue
                        kfalse
                        knull
    ]

    // Need to be able to parse child nodes, which is mutually recursive with being able to parse one node
    let knodes, knodesRef = createParserForwardedToRef<KDL list, UserState>()

    /// Parse a slapdash comment before a parser p and call ignoreFunction on the results of p if slapdash found
    let slapDash p ignoreFunction = ((str "/-") >>. ws >>. p |>> ignoreFunction) <|> p
    // If we encounter a comment, we still need to know where the node ends
    // Use UserState to build the node through sucessive computations and handle both props and args
    /// node := ('/-' ws*)? identifier (node-space node-props-and-args)* (node-space* node-children ws*)? node-terminator
    let node = //kidentifier .>>. prop
        //let prop = pipe3 kidentifier (str "=") value (fun k _ v -> (k, v))
        let contents = NodeUserState.Default
        // let prop = kidentifier .>> (str "=") >>. value
        // Return functions that create update copies of contents
        //let prop = pipe3 id (str "=") value (fun k _ v -> (fun s cont -> (cont {s with Properties=s.Properties.Add(k, v)})))
        let prop = pipe3 ident (str "=") value (fun k _ v -> (fun s -> {s with Properties=s.Properties.Add(k, v)}))

        // let comment = (str "/-") .>> ws
        // let attr = value |>> (fun v -> (fun s cont -> (cont {s with Arguments=v::s.Arguments})))
        // let nodeName = id |>> (fun name -> (fun s cont -> (cont {s with Name=name})))
        let slapDashID p = slapDash p (fun _ -> id)
        let attr = value |>> (fun v -> (fun s  -> {s with Arguments=v::s.Arguments}))
        let nodeName = ident |>> (fun name -> (fun s  -> {s with Name=name}))
        (*let child = knode |>> (fun child -> (fun s cont -> (cont {s with Children=child::s.Children})))
        let children = *)
        let terminator = (*eof <|>*) (attempt (str ";")) <|> (attempt singleLineCommentStr) <|> newline
        let children = between (str "{") (str "}") (ws >>. knodes)
        // let addChildren = children |>> (fun children -> (fun s cont -> (cont {s with Children=children})))
        let addChildren = (attempt (children |>> (fun children -> (fun s -> {s with Children=children})))) <|> (terminator |>> (fun children -> (id)))

        //(nodeName .>>. (many (prop <|> attr)) .>>. addChildren)
        //let propAttr = (many (((attempt prop) <|> attr) .>> ws))
        let propAttr = (many ((slapDashID ((attempt prop) <|> attr)) .>> (((many nodeSpace) |>> ignore) <|> ws)))

        (*pipe3 (nodeName .>> ws)  (slapDash propAttr (fun _ -> [id])) (slapDash addChildren (fun _ -> id)) (fun a b c -> 
                                                                                                let transforms = a::(c::b)
                                                                                                List.foldBack (fun t s -> (t s)) transforms contents
                                                                                            )*)
        (*pipe3 (nodeName .>> ws)  (slapDash ((many nodeSpace) >>. propAttr) (fun _ -> [id])) (slapDash ((many nodeSpace) >>. addChildren) (fun _ -> id)) (fun a b c -> 
                                                                                                let transforms = a::(c::b)
                                                                                                List.foldBack (fun t s -> (t s)) transforms contents
                                                                                            )*)
        // Apply all functions to contents
        pipe3 (nodeName .>> ws)  ((many nodeSpace) >>. propAttr) (slapDash ((many nodeSpace) >>. addChildren) (fun _ -> id)) (fun a b c -> 
                                                                                                let transforms = a::(c::b)
                                                                                                List.foldBack (fun t s -> (t s)) transforms contents
                                                                                            )

    /// nodes := linespace* (node nodes?)? linespace*
    let nodes = (many linespace) >>. ws >>. (many ((slapDash node (fun _ -> NodeUserState.Default)) .>> ws .>> (many linespace)))

    /// TODO: make this tail recursive using List.rev
    let rec filterMap f m l =
        match l with
        | h::t -> if (f h) then (m h)::(filterMap f m t) else (filterMap f m t)
        | [] -> []
    
    // let knodereal = nodes |>> List.map (fun x -> KNode(x.Name, x.Arguments, x.Properties, x.Children)) 
    let knodereal = nodes |>> filterMap (fun x -> x.Name <> "") (fun x -> KNode(x.Name, x.Arguments, x.Properties, x.Children)) 

    do knodesRef := knodereal;

    let KDLDocument = knodereal .>> eof |>> KDoc

    /// Parse a file at path using the KDLDocument parser and UTF-8 encoding (as given in specification)
    let parseFile path =                                                                
         runParserOnFile (KDLDocument) UserState.Default (path) (System.Text.Encoding.UTF8)
