module aoc2021.Day7

open Helpers

let parseCrabPositions = Array.map int

let possiblePositions xs = [ Seq.min xs .. Seq.max xs ]

let findCheapestFuelConsumption fuelFormula submarines =
    possiblePositions submarines
    |> Seq.map (fun pos -> submarines |> Seq.sumBy (fuelFormula pos))
    |> Seq.min

let solve1 = findCheapestFuelConsumption (fun targetPos crabPos -> abs (crabPos - targetPos))

let solve2 =
    findCheapestFuelConsumption (fun targetPos crabPos ->
        abs (crabPos - targetPos)
        |> float
        |> fun n -> (0.5 * n) * (1.0 + n) |> int)

let answerExample1 = readCsvLine "Day7-example.txt" |> parseCrabPositions |> solve1
let answer1 = readCsvLine "Day7.txt" |> parseCrabPositions |> solve1

let answerExample2 = readCsvLine "Day7-example.txt" |> parseCrabPositions |> solve2
let answer2 = readCsvLine "Day7.txt" |> parseCrabPositions |> solve2
