module Zanaptak.DebugUtils

open System

type Bit private () =
    static let bitString ( suffix : string ) ( bytes : byte array ) =
        let byteStrings = bytes |> Array.map ( fun b -> Convert.ToString( b , 2 ).PadLeft( 8 , '0' ) )
        "0b" + ( String.concat "_" byteStrings ) + suffix
    static let bitStringBigint ( value : bigint ) =
        let byteStrings =
            value.ToByteArray()
            |> Array.rev
            |> Array.map ( fun b -> Convert.ToString( b , 2 ).PadLeft( 8 , '0' ) )
        let spacerStrings =
            let extraBytes = byteStrings.Length % 8
            if extraBytes > 0 then Array.replicate ( 8 - extraBytes ) "        " else Array.empty
        let bits =
            Array.append spacerStrings byteStrings
            |> Array.chunkBySize 8
            |> Array.map ( fun chunk -> String.concat " " chunk )
            |> String.concat "\n"
        "\n"
            + "63                                                                    0\n"
            + "-------- -------- -------- -------- -------- -------- -------- --------\n"
            + bits
            + "\n"
    static let toBigEndian ( bytes : byte array ) =
        if BitConverter.IsLittleEndian then Array.rev bytes else bytes
    static member string ( value : bigint ) = bitStringBigint value
    static member string ( value : int64 ) = BitConverter.GetBytes value |> toBigEndian |> bitString "L"
    static member string ( value : uint64 ) = BitConverter.GetBytes value |> toBigEndian |> bitString "UL"
    static member string ( value : int32 ) = BitConverter.GetBytes value |> toBigEndian |> bitString ""
    static member string ( value : uint32 ) = BitConverter.GetBytes value |> toBigEndian |> bitString "u"
    static member string ( value : int16 ) = BitConverter.GetBytes value |> toBigEndian |> bitString "s"
    static member string ( value : uint16 ) = BitConverter.GetBytes value |> toBigEndian |> bitString "us"
    static member string ( value : sbyte ) = bitString "y" [| byte value |]
    static member string ( value : byte ) = bitString "uy" [| value |]

let binaryFraction numerator divisor maxFractionalDigits =

    let rec integerLoop index value chars =
        if value = 0 && index > 0 then chars |> List.toArray |> System.String
        else
            let chars = if index % 8 = 0 && index > 0 then '_' :: chars else chars
            integerLoop ( index + 1 ) ( value >>> 1 ) ( ( value &&& 1 |> string |> char ) :: chars )

    let leftSide = integerLoop 0 ( numerator / divisor ) []

    let rec fractionLoop index currNumerator chars =
        if ( currNumerator = 0 && index > 0 ) || index >= maxFractionalDigits then
            chars |> List.rev |> List.toArray |> System.String
        else
            let chars = if index % 8 = 0 && index > 0 then '_' :: chars else chars
            let currDigit = if divisor > currNumerator then 0 else 1
            fractionLoop ( index + 1 ) ( ( currNumerator - ( divisor * currDigit ) ) <<< 1 ) ( ( currDigit |> string |> char ) :: chars )
    let rightSide = fractionLoop 0 ( ( numerator % divisor ) <<< 1 ) []

    leftSide + "." + rightSide

let ticksToSeconds ( ticks : int64 ) = ticks / 10_000_000L
let ticksToMillis ( ticks : int64 ) = ticks / 10_000L
let ticksToMicros ( ticks : int64 ) = ticks / 10L

let secondsToYears ( seconds : float ) = seconds / 60. / 60. / 24. / 365.2425
let tenthsToYears ( tenths : float ) = tenths / 10. |> secondsToYears
let hundredthsToYears ( hundths : float ) = hundths / 100. |> secondsToYears
let millisToYears ( millis : float ) = millis / 1000. |> secondsToYears
let microsToYears ( micros : float ) = micros / 1_000_000. |> secondsToYears
let years ( num : float ) =
    printfn "years from seconds %f" ( secondsToYears num )
    printfn "years from tenths  %f" ( tenthsToYears num )
    printfn "years from hundths %f" ( hundredthsToYears num )
    printfn "years from millis  %f" ( millisToYears num )
    printfn "years from micros  %f" ( microsToYears num )

#if FABLE_COMPILER
let debugfn fmt =
    Printf.kprintf System.Console.WriteLine fmt
#else
#if INTERACTIVE
let debugfn fmt =
    Printf.kprintf System.Console.WriteLine fmt
#else
let debugfn fmt =
    Printf.kprintf System.Diagnostics.Debug.WriteLine fmt
#endif
#endif
