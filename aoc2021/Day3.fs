module aoc2021.Day3

open Helpers

open System

let joinChars (cs: string[]) = String.Join("", cs)
let toInt i = Convert.ToInt32(i, 2)
let flipBit b = if b = 1 then 0 else 1
let bitCountToBit count = if count > 0 then 1 else 0
let bitArrayToDecimal arr = arr |> Array.map string |> joinChars |> toInt

let getBitCounts (bitblock: int[][]) =
    let countBits (state: int[]) (curr: int[]) =
        for i = 0 to curr.Length - 1 do
            state.[i] <- state.[i] + (if curr.[i] = 1 then 1 else -1)

        state

    bitblock
    |> Seq.fold countBits (Array.zeroCreate bitblock.[0].Length)

let calculatePowerConsumption (diagnosticReport: int[][]) =
    let bits = getBitCounts diagnosticReport |> Array.map bitCountToBit

    let gamma = bits |> bitArrayToDecimal
    let epsilon = bits |> Array.map flipBit |> bitArrayToDecimal

    gamma * epsilon

type DaiganosticReport =
    | Equal
    | NotEqual of {| MostCommon: int; LeastCommon: int |}

type DiagnosticRatingCriteria =
    | OxygenGenerator
    | Co2Scrubber

let traceBitBLock bitBlockInts  =
    let f = bitBlockInts |> Array.mapi (sprintf "%i %A") |> String.concat "\n"
    printfn $"\n --> \n{f}"
    bitBlockInts

let determineMostCommonBit bitCount =
    if bitCount = 0 then
        Equal
    else if 0 < bitCount then
            NotEqual {| MostCommon = 1; LeastCommon = 0 |}
        else
            NotEqual {| MostCommon = 0; LeastCommon = 1 |}

let bitToKeep =
    function
    | OxygenGenerator, Equal -> 1
    | OxygenGenerator, NotEqual v -> v.MostCommon
    | Co2Scrubber, Equal -> 0
    | Co2Scrubber, NotEqual v -> v.LeastCommon

let getLifeSupportRating diagnosticReport =
    let findInDiagnosticsReport criteria (report: int[][]) =
        let mutable bitBlock = report

        let bitLength = bitBlock.[0].Length - 1
        for bitIndex = 0 to bitLength  do
            if bitBlock.Length <> 1 then
                let bitCounts = getBitCounts bitBlock
                let bitCount = bitCounts.[bitIndex]
                let bitToKeep = bitToKeep (criteria, determineMostCommonBit bitCount)
                printfn $"    BitIndex:%A{bitIndex} keep:{bitToKeep}"

                let bitLinePred (bitLine: array<int>) =
                    let targetBit = bitLine.[bitIndex]
                    targetBit = bitToKeep

                bitBlock <- Array.filter bitLinePred (traceBitBLock bitBlock)

        bitArrayToDecimal (bitBlock.[0])

    let oxy = diagnosticReport |> findInDiagnosticsReport OxygenGenerator
    let co2 = diagnosticReport |> findInDiagnosticsReport Co2Scrubber
    oxy, co2, oxy * co2


let answerExample1 = readBitArrays "Day3-example.txt" |> calculatePowerConsumption // 198
let answer1 = readBitArrays "Day3.txt" |> calculatePowerConsumption // 3374136

let answerExample2 = readBitArrays "Day3-example.txt" |> getLifeSupportRating // 230
let answer2 = readBitArrays "Day3.txt" |> getLifeSupportRating // 4432698
