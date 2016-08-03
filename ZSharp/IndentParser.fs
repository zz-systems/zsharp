namespace ZzSystems.ZSharp
open FParsec
open ParserTypes

module IndentParser =
    let tabStopDistance = 4 // must be a power of 2    

    // If this function is called at the same index in the stream
    // where the function previously stopped, then the previously
    // returned indentation will be returned again. 
    // This way we can avoid backtracking at the end of indented blocks.
    let skipIndentation (stream: CharStream) =    
        let lastParsedIndentation = stream.UserState.LastParsedIndentation
        if lastParsedIndentation.EndIndex = stream.Index then
            lastParsedIndentation.Value
        else
            let mutable indentation = stream.SkipNewlineThenWhitespace(tabStopDistance, false)
            while stream.Peek(0) = '/' && stream.Peek(1) = '/' do
                stream.SkipRestOfLine(false) // skip comment
                indentation <- stream.SkipNewlineThenWhitespace(tabStopDistance, false)

            if (stream.Peek(0) = '/' && stream.Peek(1) = '*') then
                let mutable found = ref false
                indentation <- stream.SkipCharsOrNewlinesUntilString("*/",  System.Int32.MaxValue, found)

            lastParsedIndentation.EndIndex <- stream.Index
            lastParsedIndentation.Value <- indentation
            indentation

    let indentedMany1 (p: Parser<'t>) label : Parser<'t list> =
        fun stream ->
            let oldIndentation = stream.UserState.Indentation
            let indentation = skipIndentation stream
            if indentation <= oldIndentation then 
                Reply(Error, expected (if indentation < 0 then "newline" else "indented " + label))
            else
                stream.UserState <- {stream.UserState with Indentation = indentation}            
                let results = ResizeArray()
                let mutable stateTag = stream.StateTag
                let mutable reply = p stream // parse the first element
                let mutable newIndentation = 0
                while reply.Status = Ok 
                      && (results.Add(reply.Result)
                          newIndentation <- skipIndentation stream
                          newIndentation = indentation)
                   do
                     stateTag <- stream.StateTag
                     reply <- p stream
                if reply.Status = Ok 
                   || (stream.IsEndOfStream && results.Count > 0 && stream.StateTag = stateTag) 
                then
                    if newIndentation < indentation || stream.IsEndOfStream then
                        stream.UserState <- {stream.UserState with Indentation = oldIndentation}
                        Reply(List.ofSeq results)
                    else
                        Reply(Error, messageError "wrong indentation")
                else // p failed
                    Reply(reply.Status, reply.Error) 

    let indented (p: Parser<'t>) label : Parser<'t> =
        fun stream ->
            let oldIndentation = stream.UserState.Indentation
            let indentation = skipIndentation stream
            if indentation <= oldIndentation then 
                Reply(Error, expected (if indentation < 0 then "newline" else "indented " + label))
            else
                stream.UserState <- {stream.UserState with Indentation = indentation}            
                
                let mutable stateTag = stream.StateTag
                let mutable reply = p stream // parse the first element
                let mutable newIndentation = 0
                               
                if reply.Status = Ok 
                   || (stream.IsEndOfStream && stream.StateTag = stateTag) 
                then
                    if newIndentation < indentation || stream.IsEndOfStream then
                        stream.UserState <- {stream.UserState with Indentation = oldIndentation}
                        Reply(reply.Result)
                    else
                        Reply(Error, messageError "wrong indentation")
                else // p failed
                    Reply(reply.Status, reply.Error) 