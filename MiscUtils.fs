module Zanaptak.MiscUtils

open System
open System.Text.RegularExpressions

module List =
    let add item ( list : 'a list ) = list @ [ item ]
    /// Returns a new list that contains the elements of the second list followed by the elements of the first.
    let appendList appendedList ( baseList : 'a list ) = List.append baseList appendedList
    let cons item ( list : 'a list ) = item :: list
    let rec insert index item ( list : 'a list ) =
        match index,list with
        | 0, xs -> item::xs
        | _, [] -> [ item ]
        | i, x::xs -> x::insert (i-1) item xs
    let rec setItem index item ( list : 'a list ) =
        match index,list with
        | 0, x::xs -> item::xs
        | _, [] -> []
        | i, x::xs -> x::setItem (i-1) item xs
    let rec removeAt index ( list : 'a list ) =
        match index,list with
        | 0, x::xs -> xs
        | _, [] -> []
        | i, x::xs -> x::removeAt (i-1) xs
    let rec remove item ( list : 'a list ) =
        match list with
        | [] -> []
        | x::xs when x = item -> xs
        | x::xs -> x::remove item xs
    let rec replace olditem newitem ( list : 'a list ) =
        match list with
        | [] -> []
        | x::xs when x = olditem -> newitem::xs
        | x::xs -> x::replace olditem newitem xs
    let isNotEmpty ( list : 'a list ) = not ( List.isEmpty list )
    let isIn ( list : 'a list ) item = List.contains item list

module Map =
    /// Add/Update item if function returns Some, Remove item if function returns None
    let change key f map =
        Map.tryFind key map
        |> f
        |> function
             | Some v -> Map.add key v map
             | None -> Map.remove key map

type OptionBuilder() =
    member this.Bind( m , f ) = Option.bind f m
    member this.Return( x ) = Some x
    member this.ReturnFrom( x ) = x
    member this.Zero() = None
let opt = new OptionBuilder()

module Option =
    /// Return Some( int-value ) if int is non-zero
    let ofInt x = if x <> 0 then Some x else None
    /// Return Some( bool-value ) if bool is true
    let ofBool x = if x then Some x else None
    /// Return Some() if condition is true.
    /// Useful as a short-circuiting assertion in an option computation expression, e.g. do! Option.require( x > 0 )
    let require condition = if condition then Some() else None
    let toSeq = function
        | Some x -> Seq.singleton x
        | None -> Seq.empty

type ResultBuilder() =
    member this.Bind( m , f ) = Result.bind f m
    member this.Return( x ) = Ok x
    member this.ReturnFrom( x ) = x
    member this.Zero() = Error()
let res = new ResultBuilder()

type Queue< 'a > = {
    Front : 'a list
    Back : 'a list
}

module Queue =

    let private ofParts front back : Queue< 'a > = { Front = front ; Back = back }

    let empty< 'a > : Queue< 'a > = ofParts [] []
    let ofItem item = ofParts [ item ] []
    let ofList items = ofParts items []
    let ofSeq items = ofParts ( List.ofSeq items ) []
    let isEmpty queue = queue.Front.IsEmpty
    let hasItem queue = not queue.Front.IsEmpty
    let length queue = queue.Front.Length + queue.Back.Length

    let add item queue =
        if isEmpty queue then ofItem item
        else ofParts queue.Front ( item :: queue.Back )

    let addList items queue =
        if isEmpty queue then ofList items
        else ofParts queue.Front ( List.rev items @ queue.Back )

    let next queue =
        match queue.Front with
        | item :: remainingFront ->
            let nextQueue =
                if remainingFront.IsEmpty then ofParts ( List.rev queue.Back ) []
                else ofParts remainingFront queue.Back
            item , nextQueue
        | _ -> raise ( System.ArgumentException( "Queue is empty" ) )

    let tryNext queue =
        match queue.Front with
        | item :: remainingFront ->
            let nextQueue =
                if remainingFront.IsEmpty then ofParts ( List.rev queue.Back ) []
                else ofParts remainingFront queue.Back
            Some ( item , nextQueue )
        | _ -> None

    let toSeq queue =
        seq {
            yield! queue.Front
            yield! List.rev queue.Back
        }

    let toList queue =
        [
            yield! queue.Front
            yield! List.rev queue.Back
        ]

type Stack< 'a > = {
    Stack : 'a list
}

module Stack =

    let ofList items = { Stack = items }
    let empty< 'a > : Stack< 'a > = ofList []
    let ofItem item = ofList [ item ]
    let ofSeq items = ofList ( List.ofSeq items )
    let isEmpty stack = stack.Stack.IsEmpty
    let hasItem stack = not stack.Stack.IsEmpty
    let length stack = stack.Stack.Length

    let push item stack = ofList ( item :: stack.Stack )
    let pushList items stack = ofList ( items @ stack.Stack )

    let pop stack =
        match stack.Stack with
        | item :: remainingStack -> item , ofList remainingStack
        | _ -> raise ( System.ArgumentException( "Stack is empty" ) )

    let tryPop stack =
        match stack.Stack with
        | item :: remainingStack -> Some ( item , ofList remainingStack )
        | _ -> None

    let toSeq stack = List.toSeq stack.Stack
    let toList stack = stack.Stack

let tryParseWith ( fn : string -> bool * 'a ) = fn >> function
    | true, x -> Some x
    | false, _ -> None

let tryParseInt = tryParseWith System.Int32.TryParse
let tryParseFloat = tryParseWith System.Double.TryParse
let tryParseDecimal = tryParseWith System.Decimal.TryParse

let tryParseIntDefault def value = tryParseInt value |> Option.defaultValue def

let exnToResult f x = try Result.Ok ( f x ) with ex -> Result.Error ex.Message
let exnToOption f x = try Some ( f x ) with _ -> None

let rec clamp a b value =
    if a > b then clamp b a value
    else max a ( min b value )

let withLowerBound bound value = max bound value
let withUpperBound bound value = min bound value

let isStringNotEmpty s = not ( System.String.IsNullOrWhiteSpace( s ) )
let isStringEmpty s = System.String.IsNullOrWhiteSpace( s )

let trimString ( s : System.String ) = s.Trim()

let regexMatches pattern input =
    let matches = Regex.Matches( input , pattern )
    matches |> Seq.cast< Match > |> Seq.map ( fun m -> m.Value )

/// Performs regex match and returns list of groups -- note first item in list is entire match
let regexGroups pattern input =
    let m = Regex.Match( input , pattern )
    if m.Success then [ for i in 0 .. m.Groups.Count - 1 -> m.Groups.[ i ].Value ] else []

/// Performs regex match and matches against list of groups -- note first item in list is entire match
let (|RegexGroups|_|) pattern input =
    let groups = regexGroups pattern input
    if not ( List.isEmpty groups ) then Some groups else None

let mapFst fn (a,b) = (fn a, b)
let mapSnd fn (a,b) = (a, fn b)

let pluralStr singularWord pluralWord count =
    if count = 1 then "1 " + singularWord
    else sprintf "%i %s" count pluralWord

let spaceBeforeInnerCapitals value = Regex.Replace( value , "\\B([A-Z])" , " $1" )

/// Check if CLIMutable record is null, e.g. from an ORM. (Can't use "= null" syntax on records.)
let isNullRecord r = System.Object.ReferenceEquals( r , null )
/// Check if CLIMutable record is not null, e.g. from an ORM. (Can't use "<> null" syntax on records.)
let isNotNullRecord r = not ( isNullRecord r )
/// Convert nullable CLIMutable record to Some r if non-null, else None.
let nullableRecordToOption r = if isNullRecord r then None else Some r

