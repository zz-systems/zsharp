namespace ZzSystems.ZSharp

// Primitives 

type Name = string
type VarName = Name
type TypeName = Name
type MemberName = Name
type LabelName = Name
type Value = obj
type Literal = Literal of Value

// Expressions
type ArgType = ValueArg | RefArg  | OutArg
type Expression =
    | Value of Literal
    | Variable of VarName
    | MethodInvoke of MemberName * Arg list
    | PropertyGet of MemberName
    | Cast of TypeName * Expression
    | InfixOp of Expression * string * Expression
    | PrefixOp of string * Expression     
    | PostfixOp of Expression * string
    | TernaryOp of Expression * Expression * Expression
and Arg = Arg of ArgType * Expression

// Modifiers
type Access     = Public | Private | Protected | Internal
type Modifier   = Static | Sealed | Override | Virtual | Abstract | Proxy | Parallel | Sequential

// Statements
type Define = Define of TypeName * VarName
type Init = 
    | Assign of Name * Expression
    | Construct of TypeName * Name * Expression 
    
type Condition = Expression
type Iterator = Expression

type Statement = 
    | Definition of Define 
    | Assignment of Init
    | PropertySet of MemberName * Expression
    | Action of Expression
    | If of Expression * Block
    | IfElse of Expression * Block * Block
    | Switch of Expression * Case list
    | For of Init list * Condition * Iterator list * Block * Modifier
    | ForEach of Define * Expression * Block * Modifier
    | While of Expression * Block
    //| DoWhile of Block * Expression
    | Throw of Expression
    | Try of Block
    | Catch of TypeName * Block
    | Finally of Block
    | Lock of Expression * Block
    | Using of Expression * Block
    | Label of LabelName
    | Goto of LabelName
    | Break
    | Continue
    | Return of Expression
and Case =
    | Case of Literal * Block
    | Default of Block
and Block = Statement list

// Members

type ReturnType     = TypeName
type MemberInfo     = MemberInfo of Access * Modifier option * ReturnType * Name
type IsReadOnly     = bool
type ParamType      = ByValue | ByRef | Out | Params
type Param          = Param of ParamType * TypeName * VarName
type PreConstruct   = PreConstruct of Name * Arg list

type Member =
    | Field of Access * Modifier option * IsReadOnly * ReturnType * Name * Expression option
    | Property of MemberInfo * Block option * Block option
    | Method of MemberInfo * Param list * Block
    | Constructor of Access * Modifier option * Name * Param list * PreConstruct option * Block

// Types
type Members = Member list
type Implements = Name list
type EnumValue = EnumValue of Name * Value

type ZSharpType =
    | Class of Access * Modifier option * Name * Implements * Members
    | Struct of Access * Name * Member list
    | Interface of Access * Name * Implements * Member list 
    | Enum of Access * TypeName * EnumValue list
    | Delegate of Access * Name * ReturnType * Param list

// Namespaces 
type Import = 
    | Import of Name list
    | Alias of Name * Name list

type NamespaceScope = 
    | Namespace of Import list * Name list * NamespaceScope list
    | Types of Import list * ZSharpType list