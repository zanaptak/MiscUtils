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

type MaybeBuilder() =
  member this.Bind( m , f ) = Option.bind f m
  member this.Return( x ) = Some x
  member this.ReturnFrom( x ) = x
  member this.Zero() = Some ()
let maybe = new MaybeBuilder()

type ResultBuilder() =
  member this.Bind( m , f ) = Result.bind f m
  member this.Return( x ) = Ok x
  member this.ReturnFrom( x ) = x
  member this.Zero() = Ok ()
let result = new ResultBuilder()

let tryParseWith fn = fn >> function
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

let (|RegexGroups|_|) pattern input =
  let m = Regex.Match( input , pattern )
  if m.Success then Some [ for i in 0 .. m.Groups.Count - 1 -> m.Groups.[ i ].Value ] else None

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

module Option =
  let toSeq = function
    | Some x -> Seq.singleton x
    | None -> Seq.empty
