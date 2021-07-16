module Zanaptak.ReflectionUtils

open Microsoft.FSharp.Reflection
open System

let getUnionCaseName (case : 'T) =
#if FABLE_COMPILER
    Fable.Core.Reflection.getCaseName case
#else
    let (caseInfo, _args) = FSharpValue.GetUnionFields(case, typeof<'T>)
    caseInfo.GetCustomAttributes()
    |> Seq.tryPick (function
        | :? CompiledNameAttribute as att -> Some att.CompiledName
        | _ -> None
    )
    |> Option.defaultValue caseInfo.Name
#endif

let getUnionCaseTag (case : 'T) =
#if FABLE_COMPILER
    Fable.Core.Reflection.getCaseTag case
#else
    let (caseInfo, _args) = FSharpValue.GetUnionFields(case, typeof<'T>)
    caseInfo.Tag
#endif
