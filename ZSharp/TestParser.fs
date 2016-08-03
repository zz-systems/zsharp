namespace ZzSystems.ZSharp

module TestParser = 

    open FParsec
    open IndentParser

//    let isBlank = fun c -> c = ' ' || c = '\t'
//    let ws1 = skipMany1SatisfyL isBlank "whitespace"
//    let comment = pstring "#" >>. skipRestOfLine false
//    let wsBeforeEOL = skipManySatisfy isBlank >>. optional comment
//    let identifier = asciiLower |>> string
//    let keyword str = pstring str >>? nextCharSatisfiesNot (fun c -> isLetter c || isDigit c) <?> str
//
//    let indentedStatements, indentedStatementsRef = createParserForwardedToRef()
//
//    let print = keyword "print" >>. (ws1 >>. identifier .>> wsBeforeEOL |>> Print)
//    let loop = keyword "loop" >>. (pipe4 (ws1 >>. identifier) (ws1 >>. pint32) (ws1 >>. pint32 .>> wsBeforeEOL) 
//                                            indentedStatements
//                                            (fun id min max stmts -> Loop(id, min, max, stmts)))
//    let statement = print <|> loop
//
//    do indentedStatementsRef := indentedMany1 statement "statement"
//
//    let document = indentedStatements .>> spaces .>> eof

