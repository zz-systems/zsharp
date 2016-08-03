// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
namespace ZzSystems.ZSharp

module Program = 
    open FParsec;
    open Parser;
    open System;
    open IndentParser;

    type Identifier = string

    type Statement = Loop of Identifier * int * int * Statement list
                    | Print of Identifier
                    
    
    let program =
        """ 
        //with System;
        
        //namespace Test:

        public class MyClass with Module, IMarker:     
            private int _x;
            private Object _context;

            public this(int x) with base(2345):
                _x = x;
                int y = 4421 * ( 123.5 - 22312 * acb³)²;

                lol ( 1², "iksDee", y);

            public bool MyMethod(int lol):
                
                parallel for(int i = 0; i < 20; i++):
                    printf(i);

                return true;

            public proxy bool MyProxy: _context.Blah;

            public bool MyProperty:
                get:
                    return 1;
                set: 
                    x = value;
        
        enum TestEnum:
            | One | lül
            | iksDee
            | Two
            | BOOM
    """

    [<EntryPoint>]
    let main argv = 
        
        parse program |> function 
            | Success (result, us, _) ->
                System.Diagnostics.Trace.WriteLine <| (sprintf "Success: %A" result)
                //System.Diagnostics.Trace.WriteLine <| (sprintf "Debug:\n\n%s" us.Debug.Message)
            | Failure (msg, _, us)   ->
                System.Diagnostics.Trace.WriteLine <| (sprintf "Failure: %A\n" msg)
                //System.Diagnostics.Trace.WriteLine <| (sprintf "Debug:\n\n%s" us.Debug.Message)

//        let isBlank = fun c -> c = ' ' || c = '\t'
//        let ws1 = skipMany1SatisfyL isBlank "whitespace"
//        let comment = pstring "#" >>. skipRestOfLine false
//        let wsBeforeEOL = skipManySatisfy isBlank >>. optional comment
//        let identifier = asciiLower |>> string
//        let keyword str = pstring str >>? nextCharSatisfiesNot (fun c -> isLetter c || isDigit c) <?> str
//
//        let indentedStatements, indentedStatementsRef = createParserForwardedToRef()
//
//        let print = keyword "print" >>. (ws1 >>. identifier .>> wsBeforeEOL |>> Print)
//        let loop = keyword "loop" >>. (pipe4 (ws1 >>. identifier) (ws1 >>. pint32) (ws1 >>. pint32 .>> wsBeforeEOL) 
//                                             indentedStatements
//                                             (fun id min max stmts -> Loop(id, min, max, stmts)))
//        let statement = print <|> loop
//
//        do indentedStatementsRef := indentedMany1 statement "statement"
//
//        let document = indentedStatements .>> spaces .>> eof
//
//        let test str =
//            match runParserOnString document (UserState.Create()) "" str with
//            | Success(result, _, _)   -> printfn "Success: %A" result
//            | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
//
//        test @"
//        loop i 1 10
//          loop k 1 10
//            print k
//          print i
//        print j
//
//        ";
        0 // return an integer exit code
