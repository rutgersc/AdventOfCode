module aoc2021.Day7

open Helpers
open System

let traceFishes d inp =
    printfn $"{d + 1}\t%A{inp}"
    inp

let parseCrabPositions (lines: string []) = lines.[0] |> fun line -> line.Split "," |> Array.map int

let possiblePositions xs = [ Seq.min xs .. Seq.max xs ]

let

let solve1 crabPositions =
    let fuelConsumption targetPos crabPos = crabPos - targetPos |> abs

    possiblePositions crabPositions
    |> Seq.map (fun candidatePos ->
        crabPositions |> Array.map (fuelConsumption candidatePos) |> Seq.sum
        )
    |> Seq.min

let solve2 crabPositions =
    let fuelConsumption targetPos crabPos =
        abs (crabPos - targetPos)
        |> float
        |> fun n -> (0.5 * n) * (1.0 + n)
        |> int

    possiblePositions crabPositions
    |> Seq.map (fun candidatePos ->
        let totalFuel = crabPositions |> Array.map (fuelConsumption candidatePos) |> Seq.sum
        candidatePos, crabPositions, totalFuel)
    |> Seq.minBy (fun (_, _, fuel) -> fuel)

let answerExample1 = readLines "Day7-example.txt" |> parseCrabPositions |> solve1 //
let answer1 = readLines "Day7.txt" |> parseCrabPositions |> solve1 //

let answerExample2 = readLines "Day7-example.txt" |> parseCrabPositions |> solve2 //
let answer2 = readLines "Day7.txt" |> parseCrabPositions |> solve2 //
