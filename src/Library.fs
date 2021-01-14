namespace KDL
open FParsec

module Parser =
    type UserState = {Hashes: int} with static member Default = {Hashes=0}

    type Parser<'t> = Parser<'t, UserState>

    let str s = pstring s

    let isUnicodeSpace = (function '\u0009'|'\u0020'|'\u00A0'|'\u1680'|'\u2000'|'\u2001'|'\u2002'|'\u2003'|'\u2004'|'\u2005'|'\u2006'|'\u2007'|'\u2008'|'\u2009'|'\u200A'|'\u202F'|'\u205F'|'\u3000' -> true | _ -> false)
    /// FParsec converts \r and \n\r to \n: https://www.quanttec.com/fparsec/reference/charparsers.html#members.manySatisfy
    let isUnicodeNewline = (function '\u000A'|'\u0085'|'\u000C'|'\u2028'|'\u2029' -> true |_ -> false)
    let isBom x = x = '\uFEFF'

    /// TODO: Fix multiline comments in documents with multiple multiline comments
    /// Taken from https://stackoverflow.com/a/8405610
    let rec multilinecomment o =
        let ign x = skipCharsTillString x false System.Int32.MaxValue
        let commentOpen = "/*"
        let commentClose = "*/"
        between
            (str commentOpen)
            (str commentClose)
            // This appears to skip over the commentClose to the next comment open
            ((attempt (ign commentOpen >>. multilinecomment >>. ign commentClose)) <|> ign commentClose) <| o
    
    // commentClose <|> commentOpen <|> anyChar
    (*let multilinecomment =
        let inc = updateUserState (fun us -> {us with Hashes=us.Hashes + 1})
        let dec = updateUserState (fun us -> {us with Hashes=us.Hashes - 1})
        let matched = userStateSatisfies (fun us -> us.Hashes = 0)
        let commentOpen = (str "/*") >>. inc
        let commentClose = (str "*/") >>. dec
        let rec tryClose = (attempt (commentClose >>. matched)) <|> (attempt commentOpen) <|> (anyChar >>. tryClose)
        matched >>. commentOpen >>. tryClose*)

    /// Parse zero or more whitespace characters
    let ws:Parser<_> = (attempt multilinecomment) <|> (skipManySatisfy (fun c -> (isUnicodeSpace c) || (isBom c)))
    /// Parse one or more whitespace characters
    let ws1 : Parser<_> = multilinecomment <|> (skipMany1Satisfy (fun c -> (isUnicodeSpace c) || (isBom c)))
    let newline:Parser<_> = manySatisfy isUnicodeNewline
    let newlineSkip = skipManySatisfy isUnicodeNewline
    let newlineSingleSkip:Parser<_> = skipSatisfy isUnicodeNewline
    let singleLineComment : Parser<_> = (str "//") >>. skipRestOfLine false >>. newlineSkip
    let singleLineCommentStr = (str "//") .>> skipRestOfLine false .>> newlineSkip 
    let linespace = (attempt singleLineComment) <|> (attempt newlineSingleSkip) <|> ws1
    let escline = (str "\\") >>. ws .>> ((attempt singleLineComment) <|> newlineSingleSkip)
    let nodeSpace = ws1 <|> (ws .>> escline .>> ws)
    
    type KDL = KString of string
                 | KRawString of string
                 | KInt of int64 (* Consider adding a Number type that is a union of int, float, and bigint *)
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
    let underscoreFloat : Parser<_> = many1Satisfy2 (fun c -> (c >= '0' && c <= '9') || (c = '+') || (c = '-')) (fun c -> (c >= '0' && c <= '9') || (c = '.') || (c = '_') || (c = 'E') || (c = 'e') || (c = '+') || (c='-')) |>> (fun s -> (float (s.Replace("_", ""))))
    let underscoreInt : Parser<_> = 
        let decimal = many1Satisfy2 (fun c -> (c >= '0' && c <= '9') || (c = '+') || (c = '-')) (fun c -> (c >= '0' && c <= '9') || (c = '_')) .>> notFollowedBy (pchar '.') |>> int64
        let notDecimal = pipe3 (pchar '0') (anyOf "box") (many1Satisfy (fun c -> (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f') || (c = '_'))) (fun a b c -> (int64 (((string a) + (string b) + c.Replace("_","")))))
        (attempt notDecimal) <|> (decimal)
    let kint = (attempt (underscoreInt)) |>> KInt
    let kfloat = (attempt (underscoreFloat)) |>> KFloat
    
    /// TODO: Handle unicode escape of 1 to 6 charaacters
    /// Taken from https://www.quanttec.com/fparsec/tutorial.html#parsing-json
    let stringLiteral =
        let escape =  anyOf "\"\\/bfnrt"
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
    
    let rawString : Parser<_> =
        let inc = updateUserState (fun us -> {us with Hashes=us.Hashes + 1})
        let dec = updateUserState (fun us -> {us with Hashes=us.Hashes - 1})
        let matched = userStateSatisfies (fun us -> us.Hashes = 0)
        let rawStringOpen = matched >>. (str "r") >>. (many (pchar '#' >>. inc)) .>> str "\""
        let rawStringClose = (str "\"") .>> (many (pchar '#' >>. dec)) .>> matched
        rawStringOpen >>. manyCharsTill anyChar (attempt rawStringClose) 

        
    let krawstring = rawString |>> KString
    
    let ident : Parser<_> = 
        // TODO: check that c < 0x10FFFF
        let validIdentifierCharacter c = (c > '\u0020') && (c<>'\"') && (c <> '\\') && (c <> '<')&&(c <> '>')&&(c <> '{')&&(c <> '}')&&(c <> ';')&&(c <> '[')&&(c <> ']')&&(c <> '=')
        let validInitialCharacter c = validIdentifierCharacter c && (c < '0' || c > '9')
        (*let bare = identifier (IdentifierOptions(
                                isAsciiIdStart = validInitialCharacter,
                                isAsciiIdContinue = validIdentifierCharacter//,
                                //normalization = System.Text.NormalizationForm.FormKC
                    )
        )*)
        let bare = many1Chars2 (satisfy validInitialCharacter) (satisfy validIdentifierCharacter)
        (* rawString can start with r, which would be a valid part of a bare identifier, so rawstring should go first*)
        (attempt rawString) <|> bare <|> stringLiteral

    let kidentifier = ident |>> KIdentifier

    let value = choice [krawstring
                        kstring
                        kint
                        kfloat
                        ktrue
                        kfalse
                        knull
    ]

    // Need to be able to parse child nodes to parse a single node one node
    let knodes, knodesRef = createParserForwardedToRef<KDL list, UserState>()

    /// Parse a slapdash comment before a parser p and call ignoreFunction on the results of p if slapdash found
    let slapDash p ignoreFunction = ((str "/-") >>. ws >>. p |>> ignoreFunction) <|> p

    /// node := ('/-' ws*)? identifier (node-space node-props-and-args)* (node-space* node-children ws*)? node-terminator
    let node =
        let contents = NodeUserState.Default
        // Define parsers that return functions that create updated copies of a NodeUserState
        let prop = pipe3 ident (str "=") value (fun k _ v -> (fun s -> {s with Properties=s.Properties.Add(k, v)}))
        let slapDashID p = slapDash p (fun _ -> id)
        let attr = value |>> (fun v -> (fun s  -> {s with Arguments=v::s.Arguments}))
        let nodeName = ident |>> (fun name -> (fun s  -> {s with Name=name}))
        let terminator = (attempt (str ";")) <|> (attempt singleLineCommentStr) <|> newline
        let children = between (str "{") (str "}") (ws >>. knodes)
        let addChildren = (attempt (children |>> (fun children -> (fun s -> {s with Children=children})))) <|> (terminator |>> (fun children -> (id)))
        let propAttr = (many ((slapDashID ((attempt prop) <|> attr)) .>> (((many nodeSpace) |>> ignore) <|> ws)))

        // Apply functions to contents
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
    
    // Slapdash commented nodes have empty strings for names
    let knodereal = nodes |>> filterMap (fun x -> x.Name <> "") (fun x -> KNode(x.Name, x.Arguments, x.Properties, x.Children)) 

    do knodesRef := knodereal;

    let KDLDocument = knodereal .>> eof |>> KDoc

    /// Parse a file at path using the KDLDocument parser and UTF-8 encoding
    let parseFile path =                                                                
         runParserOnFile (KDLDocument) UserState.Default (path) (System.Text.Encoding.UTF8)
    let parseString s =
        runParserOnString (KDLDocument) UserState.Default "" s