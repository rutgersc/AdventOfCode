module aoc2021.Day10

open Helpers

let openChars = "([{<"
let closeChars = ")]}>"

let map =
    Seq.concat [ chars openChars |> Seq.zip (chars closeChars)
                 chars closeChars |> Seq.zip (chars openChars) ]
    |> Map.ofSeq

let flip = fun c -> map.[c]

let parseCorruptChar l r =
    match map.[l] = r with
    | true -> None
    | false -> Some (l, r)

let illegalCharScore c = (Seq.zip closeChars [ 3 ; 57 ; 1197 ; 25137 ] |> Map.ofSeq).[c]
let closeScore c = (Seq.zip closeChars [ 1UL .. 4UL ] |> Map.ofSeq).[c]

let trackChunks (stack, corrupt) char =
    match corrupt with
    | Some _ -> stack, corrupt
    | None ->
        if openChars |> Seq.contains char then
            char :: stack, corrupt
        else
            match stack with
            | pop :: stack -> stack, parseCorruptChar pop char

let solve1 lines =
    let findCorrupt = Seq.scan trackChunks ([], None) >> Seq.choose snd >> Seq.truncate 1

    lines
    |> Seq.collect findCorrupt
    |> Seq.sumBy (snd >> illegalCharScore)

let solve2 lines =
    let findIncomplete =
        Seq.fold trackChunks ([], None)
        >> fun (stack, corrupt) ->
            match corrupt with
            | Some _ -> None
            | None -> Some (joinChars stack)

    let getScore = Seq.map flip >> Seq.fold (fun s c -> (s * 5UL) + closeScore c) 0UL

    let scores =
        lines
        |> Seq.choose findIncomplete
        |> Seq.map getScore
        |> Seq.sort
        |> Seq.toList

    scores.[scores.Length / 2]

let answerExample1 = readLines "Day10-example.txt" |> solve1 // 26397
let answer1 = readLines "Day10.txt" |> solve1 // 436497

let answerExample2 = readLines "Day10-example.txt" |> solve2 // 288957
let answer2 = readLines "Day10.txt" |> solve2 // 2377613374
