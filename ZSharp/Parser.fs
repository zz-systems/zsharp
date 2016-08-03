namespace ZzSystems.ZSharp

open ParserTypes
open FParsec 
open IndentParser
open FParsecTrace

module Parser = 
    // Whitespaces 
    let maxCount = System.Int32.MaxValue
//    let pcomment = pstring "//" >>. many1Satisfy ((<>) '\n')
//    let pspaces = spaces >>. many (spaces >>. pcomment >>. spaces)
    //let pmlcomment = pstring "/*" >>. skipCharsTillString "*/" true (maxCount)
    let pcomment = pstring "//" >>. skipRestOfLine false
    let isBlank = fun c -> c = ' ' || c = '\t'
    
    let ws  = skipManySatisfy isBlank >>. optional pcomment
    let ws1 = skipMany1SatisfyL isBlank "whitespace"
    
    //let ws = pspaces >>. many (pspaces >>. pmlcomment >>. pspaces) |>> ignore
    //let ws1 = spaces1
    let str_ws s = pstring s .>> ws
    let str_ws1 s = pstring s .>> ws1

    let keyword str = pstring str >>? nextCharSatisfiesNot (fun c -> isLetter c || isDigit c) <?> str
    let keyword_ws s = keyword s .>> ws
    let keyword_ws1 s = keyword s .>> ws1
    

    // Literals
    type Lit = NumberLiteralOptions
    let numberFormat = Lit.AllowMinusSign ||| Lit.AllowFraction  ||| Lit.AllowExponent

    let pnumber : Parser<Literal, UserState> =       
        numberLiteral numberFormat "number" <!> "numberLiteral"
        |>> fun nl -> 
                if nl.IsInteger then Literal(int nl.String)
                else Literal(float nl.String)
    
    let ptrue   = keyword_ws "true"     <!> "true"  |>> fun _ -> Literal(true)
    let pfalse  = keyword_ws "false"    <!> "false" |>> fun _ -> Literal(false)

    let pbool   = ptrue <|> pfalse <!> "pbool"

    let pstringLiteral = 
        let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
        let unescape c = match c with
                            | 'n' -> '\n'
                            | 'r' -> '\r'
                            | 't' -> '\t'
                            | c   -> c
        let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
        between (pstring "\"") (pstring "\"")
                (manyChars (normalChar <|> escapedChar)) <!> "pstringLiteral"
        |>> fun s -> Literal(s)

    let pliteral = pnumber <|> pbool <|> pstringLiteral <!> "pliteral"


    // Access

    let ppublic     = keyword_ws1 "public"      <!> "ppublic"    |>> fun _ -> Public
    let pprotected  = keyword_ws1 "protected"   <!> "pprotected" |>> fun _ -> Protected
    let pprivate    = keyword_ws1 "private"     <!> "pprivate"   |>> fun _ -> Private
    let pinternal   = keyword_ws1 "internal"    |>> fun _ -> Internal
    let paccess =
        opt (ppublic <|> pprotected <|> pprivate <|> pinternal) <!> "paccess"
        |>> (fun access -> defaultArg access Internal)


    // Modifiers
    let psealed     = keyword_ws1 "sealed"      <!> "psealed"    |>> fun _ -> Sealed
    let pstatic     = keyword_ws1 "static"      <!> "pstatic"    |>> fun _ -> Static
    let pproxy      = keyword_ws1 "proxy"       <!> "pproxy"     |>> fun _ -> Proxy
    let pparallel   = keyword_ws1 "parallel"    <!> "pparallel"  |>> fun _ -> Parallel
    let pmodifier   = psealed <|> pstatic <|> pproxy <!> "pmodifier"

    // Expressions

    let pexpr, pexprimpl = createParserForwardedToRef()

    let reserved = ["for"; "foreach"; "while"; "if"; "parallel"; "fn"; "module"; "using"; "return"; "proxy"; "class"; "struct"; "to"]
    let pidentifierraw = 
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
        <!> "pidentifierraw"

    let pidentifier =
        pidentifierraw 
        >>= fun s -> 
            if reserved |> List.exists ((=) s) then fail "keyword"
            else preturn s
        <!> "pidentifier"

    let pidentifier_ws = pidentifier .>> ws <!> "pidentifier_Ws"
    let pvar = (pidentifier |>> fun x -> Variable(x)) <!> "pvar"

    let pargref = keyword_ws1 "ref" <!> "pargref" |>> fun _ -> RefArg
    let pargout = keyword_ws1 "out" <!> "pargout" |>> fun _ -> OutArg
    let pargtype = (opt pargref <|> opt pargout) <!> "pargtype"
                    |>> function Some x -> x | None -> ValueArg
    let parg = pargtype .>>. pexpr <!> "parg" |>> fun (by, e) -> Arg (by, e)
    let pinvoke = 
        pidentifier_ws .>>. 
        between (str_ws "(") (str_ws ")") (sepBy parg (str_ws ",")) <!> "pinvoke"
        |>> fun (name, args) -> MethodInvoke(name, args)

    let pcast =         
        pexpr .>> keyword_ws1 "as" .>>. pidentifier_ws <!> "pcast" |>> fun (e, name) -> Cast(name, e) // (1 + 2) as float

    let pvalue = 
        (pliteral |>> fun x -> Value(x)) <|> attempt pinvoke <|> attempt pvar <|> attempt pcast <!> "pvalue"

    // Term
    type Assoc = Associativity
    let opp = OperatorPrecedenceParser<Expression, unit, UserState>()
    pexprimpl := opp.ExpressionParser

    let term = attempt (between (str_ws "(") (str_ws ")") pexpr) <|> pvalue .>> ws <|> pexpr <!> "term"
    opp.TermParser <- term

    let inops = ["+";"-";"*";"/";"%"  
                 "&&"; "||"; ">>"; "<<"; "&"; "|"; "^"; 
                 "==";"!=";"<=";">=";"<";">";"??"  
                 "."]
    for op in inops do
        opp.AddOperator(InfixOperator(op, ws, 1, Assoc.Left, fun x y -> InfixOp(x, op, y)))

    let preops = ["-"; "++"; "--"]
    for op in preops do
        opp.AddOperator(PrefixOperator(op, ws, 1, true, fun x -> PrefixOp(op, x)))

    let postops = ["++"; "--"; "²"; "³"]
    for op in postops do
        opp.AddOperator(PostfixOperator(op, ws, 1, true, fun x -> PostfixOp(x, op)))

    let pexpr' = between (str_ws "(") (str_ws ")") pexpr <!> "pexpr'"

    // Statement blocks
    let pstatement, pstatementimpl = createParserForwardedToRef()
    let psinglestatement = pstatement <!> "psinglestatement" |>> fun statement -> [statement]
    let pstatementblock =
        (str_ws ":") >>. (opt ws >>. indentedMany1 pstatement "statementblock") <!> "pstatementblock" 

//    let pstatementblock =
//         psinglestatement <|>
//         between (str_ws "{") (str_ws "}") (many pstatement) 

    // Assignment 
    let pdefine = pipe2 (pidentifier .>> ws1)(pidentifier) 
                        (fun ty name -> Define(ty, name)) <!> "pdefine"
    
    let pdefinition = pdefine <!> "pdefiniton" |>> fun d -> Definition(d)
    let passign = pipe3 pidentifier_ws (str_ws "=") pexpr   
                    (fun var _ expr -> Assign(var, expr)) <!> "passign"
    
    let pconstruct =
        pipe4
            (pidentifier .>> ws1)
            pidentifier_ws
            (str_ws "=")
            pexpr
            (fun ty name _ e -> Construct(ty, name, e)) <!> "pconstruct"

    let passignment = attempt passign <|> attempt pconstruct <!> "passignment" |>> fun c -> Assignment(c)

    // Flow statements

    let pif =
        pipe2  (keyword "if" >>. pexpr')  pstatementblock
              (fun e block -> If(e, block)) <!> "pif"

    let pifelse =
        pipe3 (keyword "if" >>. pexpr') pstatementblock (keyword "else" >>. pstatementblock)
              (fun e t f -> IfElse(e, t, f)) <!> "pifelse"

    // TODO: switch/case

    // Iteration statements
    let pforargs =
        let pinit = attempt passign <|> attempt pconstruct
        pipe3 
            (sepBy pinit (str_ws ",") .>> str_ws ";")
            (pexpr .>> str_ws ";")
            (sepBy pexpr (str_ws ","))
            (fun from until steps -> from, until, steps)
            <!> "pforargs"

    let pfor = 
        pipe3
            (opt pparallel) 
            (keyword "for" >>. between (str_ws "(") (str_ws ")") pforargs) // Remove () ?
            pstatementblock
            (fun modifier (inits, condition, iterators) block -> For(inits, condition, iterators, block, (defaultArg modifier Sequential)))
            <!> "pfor"

    let pforeachargs =
        pipe3 pdefine (keyword "in") pexpr
              (fun define _ collection -> define, collection)
              <!> "pforeachargs"

    let pforeach =
        pipe3
            (opt pparallel) 
            (keyword "foreach" >>. pforeachargs) pstatementblock
            (fun modifier (define, collection) block -> ForEach(define, collection, block, (defaultArg modifier Sequential)))
            <!> "pforeach"

    // TODO: While
    // TODO: Until

    let preturn = keyword_ws1 "return" >>. pexpr <!> "preturn" |>> fun e -> Return(e)

    let pbreak = keyword_ws1 "break" <!> "pbreak" |>> fun _ -> Break

    let pcontinue = keyword_ws1 "continue" <!> "pcontinue" |>> fun _ -> Continue

    // TODO: Goto?

    // Exception statements
    let pthrow = keyword "raise" >>. pexpr <!> "pthrow" |>> fun e -> Throw(e)
    
    let ptry = keyword "try" >>. pstatementblock <!> "ptry" |>> fun block -> Try(block)

    let pfinally = keyword "finally" >>. pstatementblock <!> "pfinally" |>> fun block-> Finally(block)

    let pexception = between (str_ws "(") (str_ws ")") pidentifier_ws <!> "pexception" // TODO: Allow definition
    let pcatch = keyword "catch" >>. pexception .>>. pstatementblock <!> "pcatch"
              |>> fun (ex,block) -> Catch(ex, block)

    // Lock statement
    let plock = 
        keyword "lock" >>. pexpr' .>>. pstatementblock <!> "plock"
        |>> fun (expr, stmts) -> Lock(expr, stmts)

    // Statement impl
    let paction = pexpr |>> fun e -> Action(e)

    pstatementimpl := 
        attempt pifelse <|> attempt pif <|>
        attempt pfor    <|> attempt pforeach <|> 
        attempt pthrow <|>
        attempt ptry <|> attempt pcatch <|> attempt pfinally <|>
        attempt plock <|>
        attempt (preturn        .>> str_ws ";") <|>
        attempt (pbreak         .>> str_ws ";") <|>
        attempt (pcontinue      .>> str_ws ";") <|>
        attempt (pdefinition    .>> str_ws ";") <|>
        attempt (passignment    .>> str_ws ";") <|>
        attempt (paction        .>> str_ws ";") 

       
        <!> "pstatement"

    // Parameters 
    let pref     = keyword "ref"      <!> "pref" |>> fun _ -> ByRef
    let pout     = keyword "out"      <!> "pout" |>> fun _ -> Out
    let pparams  = keyword "var"      <!> "pvar" |>> fun _ -> Params
    let pby = (opt pout <|> opt pref <|> opt pparams)  <!> "pby"
            |>> function Some x -> x | None -> ByValue

    let pparam = 
        pipe3 pby pidentifier_ws pidentifier_ws
            (fun by tpe name -> Param(by, tpe, name)) 
        <!> "pparam"

    let pparamlist = str_ws "(" >>. sepBy pparam (str_ws ",") .>> str_ws ")" <!> "pparamlist"

    // Members
    let pmemberinfo =
        pipe4 paccess (opt pmodifier) pidentifier_ws pidentifier_ws
            (fun access modifier ty name -> MemberInfo(access, modifier, ty, name))
        <!> "pmemberinfo"

    let preadonly = keyword "readonly" <!> "preadonly"
    let pfieldpreamble = 
        pipe3 paccess (opt pmodifier) (opt preadonly)
            (fun access modifier ro -> (access, modifier, Option.isSome ro))
        <!> "pfieldpreamble"

    let pfield = 
        pipe4 pfieldpreamble pidentifier_ws pidentifier_ws (str_ws ";")
            (fun (access, modifier, ro) rt name _ -> Field(access, modifier, ro, rt, name, None))
        <!> "pfield"
    
    let pget = keyword "get" >>. pstatementblock <!> "pget"
    let pset = keyword "set" >>. pstatementblock <!> "pset"

    let ppropertyblock = str_ws ":" >>. (opt (indented pget "getter")) .>>. (opt (indented pset "setter")) <!> "ppropertyblock"
        
    let pproperty = 
        pipe2 pmemberinfo ppropertyblock
            (fun mi (getter, setter) -> Property(mi, getter, setter))
        <!> "pproperty"

    let pmethod = 
        pipe3 pmemberinfo pparamlist pstatementblock
            (fun mi ps block -> Method(mi, ps, block))
        <!> "pmethod"

    let pconstructorpreamble =
        pipe4 paccess (opt pmodifier) (keyword "this") pparamlist
            (fun access modifier _ ps -> access, modifier, ps)
      
    let ppreconstructor =
        keyword_ws1 "with" >>. pidentifier_ws .>>. 
        between (str_ws "(") (str_ws ")") (sepBy parg (str_ws ",")) <!> "ppreconstructor"
        |>> fun (name, args) -> PreConstruct(name, args)

    let pconstructor =
        pipe3 pconstructorpreamble (opt ppreconstructor) pstatementblock
            (fun (access, modifier, ps) preconstructor block -> Constructor(access, modifier, "cons", ps, preconstructor, block))
        <!> "pconstructor"
  

    let pmember =
        attempt pfield <|> attempt pmethod <|> attempt pproperty <|> attempt pconstructor
        <!> "pmember"

    let pmembersblock = (str_ws ":") >>. (indentedMany1 pmember "memberblock") |>> (fun members -> members) <!> "pmembersblock"

    let penumblock = 
        (str_ws ":") >>. (indentedMany1 (many1 ((str_ws "|") >>. pidentifier_ws)) "enumblock")
        |>> List.collect (fun items -> items) |>> List.mapi (fun i name -> EnumValue(name, i))
        <!> "penumblock"

    // Types
    let pclasspreamble = 
        paccess .>>. (opt pmodifier) .>> (keyword_ws1 "class")
        <!> "pclasspreamble"

    let pimplements = 
        opt (keyword_ws1 "with" >>. sepBy1 (pidentifier_ws) (str_ws ","))
        |>> function Some x -> x | None -> []
        <!> "pimplements"

    let pclass =
        pipe4 pclasspreamble pidentifier_ws pimplements pmembersblock
            (fun (access, modifier) name implements block -> Class(access, modifier, name, implements, block))
        <!> "pclass"

    let pstruct =
        pipe4 paccess (keyword_ws1 "struct") pidentifier_ws pmembersblock 
            (fun access _ name block -> Struct(access, name, block))
        <!> "pstruct"

    let pinterface =
        pipe5 paccess (keyword_ws1 "interface") pidentifier_ws pimplements pmembersblock
            (fun access _ name implements block -> Interface(access, name, implements, block))
        <!> "pinterface"

    let penum =
        pipe4 paccess (keyword_ws1 "enum") pidentifier_ws penumblock
            (fun access _ name block -> Enum(access, name, block))
        <!> "penum"

    let pdelegate =
        pipe5 paccess (keyword "delegate") pidentifier_ws pidentifier_ws pparamlist
            (fun access _ rettype name ps -> Delegate(access, name, rettype, ps))
        <!> "pdelegate"

    let ptypedeclaration =
        pclass <|> pstruct <|> pinterface <|> penum <|> pdelegate
        <!> "ptypedeclaration"

    // Scopes
    let pnsscope, pscopeimpl = createParserForwardedToRef()
    let pnsscopesblock = (str_ws ":") >>. (indentedMany1 pnsscope "namespaceblock") <!> "pnsscopeblock"

    let pns = sepBy1 pidentifier_ws (str_ws ".") <!> "pns"

    let palias =
        keyword_ws1 "with" >>. pns .>> keyword "as" .>>. pidentifier_ws
        |>> fun (name, alias) -> Alias(alias, name)
        <!> "palias"

    let popen =
        keyword_ws1 "with" >>. pns
        |>> fun name -> Import(name)
        <!> "popen"

    let pimport = (attempt popen <|> attempt palias) .>> str_ws ";" <!> "pimport"
        
//    let pnsblock =
//        pipe3 (opt ((indentedMany1 pimport "imports"))) (keyword_ws "namespace" >>. pns) pnsscopesblock
//            (fun imports name block -> 
//                let types = Types([], [])
//                Namespace(defaultArg imports [], name, block))    
//
//    let ptypes = 
//        pipe2 (opt ((indentedMany1 pimport "imports"))) (indentedMany1 ptypedeclaration "declarationblock")
//            (fun imports classes -> Types(defaultArg imports [], classes))

    let pnsblock = 
        pipe2 (keyword_ws1 "namespace" >>. pns) pnsscopesblock
            (fun name block -> 
                let types = Types([], [])
                Namespace([], name, block))
        <!> "pnsblock"

    let ptypes = 
        indentedMany1 ptypedeclaration "declarationblock"
        |>>    (fun classes -> Types([], classes))
        <!> "ptypes"

    pscopeimpl := ws >>. (ptypes) <!> "pscope"

    let parse input = 
        runParserOnString pnsscope (UserState.Create()) "test" input