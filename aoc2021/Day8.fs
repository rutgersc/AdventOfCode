module aoc2021.Day8

open Helpers
open System

let parseSignalPatterns lines : (string [] * string []) [] =
    let p (l: string) =
        let v i = l.Split("|").[i].Split (" ") |> Array.filter ((=) "" >> not)
        v 0, v 1

    Array.map p lines

let digitsSegments =
    [ 0, [| 0 ; 1 ; 2 ; 4 ; 5 ; 6 |]
      1, [| 2 ; 5 |]
      2, [| 0 ; 2 ; 3 ; 4 ; 6 |]
      3, [| 0 ; 2 ; 3 ; 5 ; 6 |]
      4, [| 1 ; 2 ; 3 ; 5 |]
      5, [| 0 ; 1 ; 3 ; 5 ; 6 |]
      6, [| 0 ; 1 ; 3 ; 4 ; 5 ; 6 |]
      7, [| 0 ; 2 ; 5 |]
      8, [| 0 ; 1 ; 2 ; 3 ; 4 ; 5 ; 6 |]
      9, [| 0 ; 1 ; 2 ; 3 ; 5 ; 6 |] ]
    |> Map.ofSeq

let digitSegments wiring =
    let wiring = Seq.zip [ 0 .. 6 ] wiring |> Map.ofSeq

    digitsSegments
    |> Map.map (fun _ segments -> segments |> Array.map (fun i -> wiring.[i]))

let digits =
    digitSegments [ 'a' .. 'g' ]
    |> Map.values
    |> Seq.mapi (fun i v -> i, String.Join ("", v))
    |> Map.ofSeq

let segmentsCounts = digitsSegments |> Map.map (fun _ segments -> Array.length segments)

segmentsCounts
|> Seq.sortBy (fun v -> v.Value)
|> Seq.iter (printfn "%A")

let solve1 (lines: (string [] * string []) []) =
    let containsDigits target digit =
        target
        |> List.map (fun d -> segmentsCounts.[d])
        |> List.contains (String.length digit)

    lines
    |> Seq.collect (snd >> Seq.filter (containsDigits [ 1 ; 4 ; 7 ; 8 ]))
    |> Seq.length

let decodeEntry (digitStrs: string []) (targetDigits: string []) =
    let digitsWithLength len = digitStrs |> Seq.filter (fun str -> str.Length = len)
    let withoutCharsOf ignoredChars = chars >> List.except ignoredChars >> List.head

    let digitByLengthOf d str =
        digitsSegments
        |> Seq.find (fun kvp -> kvp.Key = d)
        |> fun kvp -> kvp.Value.Length = String.length str

    let one = digitStrs |> Seq.find (digitByLengthOf 1)
    let four = digitStrs |> Seq.find (digitByLengthOf 4)
    let seven = digitStrs |> Seq.find (digitByLengthOf 7)
    let eight = digitStrs |> Seq.find (digitByLengthOf 8)

    let six =
        digitsWithLength 6
        |> Seq.find (fun candidate -> one |> Seq.filter (candidate.Contains) |> Seq.length = 1)

    let zero =
        digitsWithLength 6
        |> Seq.except [ six ]
        |> Seq.find (fun candidate -> four |> Seq.filter (candidate.Contains >> not) |> Seq.length = 1)

    let nine = digitsWithLength 6 |> Seq.except [ zero ; six ] |> Seq.head

    let s0 = seven |> Seq.find (one.Contains >> not)
    let s2 = one |> withoutCharsOf six
    let s3 = four |> withoutCharsOf zero
    let s5 = one |> withoutCharsOf [ s2 ]
    let s1 = four |> withoutCharsOf [ s2 ; s3 ; s5 ]
    let s6 = nine |> withoutCharsOf [ yield! four ; s0 ]
    let s4 = eight |> withoutCharsOf nine

    let hashDigit (digit: string) = String.Join ("", digit |> Seq.sort)

    let wiring = [ s0 ; s1 ; s2 ; s3 ; s4 ; s5 ; s6 ]

    let digitByHash =
        digitSegments wiring
        |> Seq.map (fun kvp -> hashDigit (String.Join ("", kvp.Value)), kvp.Key)
        |> Map.ofSeq

    targetDigits
    |> Array.map (fun digit -> Map.find (hashDigit digit) digitByHash)
    |> fun chars -> String.Join ("", chars)
    |> int

let solve2 = Array.sumBy (uncurry decodeEntry)


let answerExample1 = readLines "Day8-example.txt" |> parseSignalPatterns |> solve1
let answer1 = readLines "Day8.txt" |> parseSignalPatterns |> solve1

let test =
    [| "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf" |]
    |> parseSignalPatterns
    |> solve2

let answerExample2 = readLines "Day8-example.txt" |> parseSignalPatterns |> solve2 // 61229
let answer2 = readLines "Day8.txt" |> parseSignalPatterns |> solve2 // 1010472
