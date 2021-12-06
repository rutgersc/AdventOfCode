module aoc2021.Day6

open Helpers
open System

let traceFishes d inp =
    printfn $"{d + 1}\t%A{inp}"
    inp

type FishState = int
let fishAge = 6
let newFishAge = fishAge + 2

let readFishAges (lines: string []) = lines.[0] |> fun line -> line.Split "," |> Array.map int


let solve1 maxDays fishes =
    let rec go days (fishes: array<int>) =
        let chunkSize = Math.Max(500, fishes.Length / 600)
        printfn $"{days} {Array.length fishes} {chunkSize}"

        if days = 0 then
            fishes
        else
            fishes
            |> Array.chunkBySize chunkSize
            |> Array.Parallel.collect (fun block ->
                block
                |> Array.collect (function
                    | 0 -> [| fishAge ; newFishAge |]
                    | s -> [| s - 1 |]))
            // |> traceFishes (maxDays - days)
            |> go (days - 1)

    go maxDays fishes |> Seq.length

let time f arg =
    let sw = Diagnostics.Stopwatch.StartNew()
    let res = f arg
    sw.Stop()
    printfn $"{sw.Elapsed.Seconds}"
    res

// let answerExample1a = readLines "Day6-example.txt" |> readFishAges |> solve1 18 // 26
// let answerExample1b = readLines "Day6-example.txt" |> readFishAges |> solve1 80 // 5934

// let answer1 = readLines "Day6.txt" |> readFishAges |> solve1 80 // 363101

let answerExample2 = readLines "Day6-example.txt" |> readFishAges |> (time (solve1 256)) // 26

// let answer2 = readLines "Day5.txt" |> readVents |> solve2 // 19929
