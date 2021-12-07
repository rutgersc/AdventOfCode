module aoc2021.Day6

open Helpers

let traceFishes d inp =
    printfn $"{d + 1}\t%A{inp}"
    inp

let fishAge = 6L
let newFishAge = fishAge + 2L

let parseFishAges = Array.map int64

let countFishes fishes =
    [| for age in 0L .. 8L do
           yield fishes |> Array.filter ((=) age) |> Array.length |> int64 |]

let solve1 maxDays (fishes: int64 array) =
    let rec go days fishes =
        if days = 0 then
            fishes
        else
            fishes
            |> Array.collect (function
                | 0L -> [| fishAge ; newFishAge |]
                | s -> [| s - 1L |])
            |> traceFishes (maxDays - days)
            |> go (days - 1)

    go maxDays fishes |> Seq.length

let solve2 maxDays fishes =
    let rec go days (fishes: array<int64>) =
        if days = 0 then
            fishes
        else
            [| fishes.[1] // 0
               fishes.[2]
               fishes.[3]
               fishes.[4]
               fishes.[5]
               fishes.[6]
               fishes.[7] + fishes.[0] // 6
               fishes.[8]
               fishes.[0] |] // 8
            |> go (days - 1)

    go maxDays fishes |> Seq.sum


let answerExample1a = readCsvLine "Day6-example.txt" |> parseFishAges |> solve1 18 // 26

let answerExample1b = readCsvLine "Day6-example.txt" |> parseFishAges |> solve1 80 // 5934

let answer1 = readCsvLine "Day6.txt" |> parseFishAges |> solve1 80 // 363101

let answerExample1a2 =
    readCsvLine "Day6-example.txt"
    |> parseFishAges
    |> countFishes
    |> solve2 18 // 26

let answerExample1a22 =
    readCsvLine "Day6-example.txt"
    |> parseFishAges
    |> countFishes
    |> solve2 80 // 5934

let answerExample2 =
    readCsvLine "Day6-example.txt"
    |> parseFishAges
    |> countFishes
    |> solve2 256 // 26984457539

let answer2 = readLines "Day6.txt" |> parseFishAges |> countFishes |> solve2 256 // 1644286074024L
