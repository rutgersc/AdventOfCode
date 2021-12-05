module aoc2021.Day5

open Helpers
open System

type Point = int * int

let readVents (lines: string []) =
    lines
    |> Seq.collect (fun l -> l.Split "->")
    |> Seq.collect (fun p -> p.Trim().Split ",")
    |> Seq.map int
    |> Seq.chunkBySize 4
    |> Seq.map (function
        | [| x1 ; y1 ; x2 ; y2 |] -> (x1, y1), (x2, y2)
        | _ -> failwith "")

let isNotDiagonal (x1, y1) (x2, y2) = x1 = x2 || y1 = y2

let allPointsFromStraightLine (x1: int, y1: int) (x2, y2) =
    let (minX, minY) = Math.Min (x1, x2), Math.Min (y1, y2)
    let (maxX, maxY) = Math.Max (x1, x2), Math.Max (y1, y2)

    seq {
        for x in minX..maxX do
            for y in minY..maxY do
                yield x, y
    }

let allPoints (x1: int, y1: int) (x2, y2) =
    if isNotDiagonal (x1, y1) (x2, y2) then
        allPointsFromStraightLine (x1, y1) (x2, y2)
    else

    let (startX, startY, slope) =
        match x1 < x2 with
        | true -> (x1, y1, (if y1 < y2 then 1 else -1))
        | false -> (x2, y2, (if y2 < y1 then 1 else -1))

    seq {
        for (step, x) in [ startX .. (startX + abs (x1 - x2)) ] |> Seq.indexed do
            yield x, startY + (step * slope)
    }

let countPoints (state: Map<Point, int>) p =
    let addPoint =
        function
        | Some c -> Some (c + 1)
        | None -> Some 1

    Map.change p addPoint state

let solve1 (vents: seq<Point * Point>) =
    vents
    |> Seq.filter (uncurry isNotDiagonal)
    |> Seq.collect (uncurry allPointsFromStraightLine)
    |> Seq.fold countPoints Map.empty
    |> Seq.filter (fun kvp -> 2 <= kvp.Value)
    |> Seq.length

let solve2 (vents: seq<Point * Point>) =
    vents
    |> Seq.collect (uncurry allPoints)
    |> Seq.fold countPoints Map.empty
    |> Seq.filter (fun kvp -> 2 <= kvp.Value)
    |> Seq.length

let answerExample1 = readLines "Day5-example.txt" |> readVents |> solve1 // 5

let answer1 = readLines "Day5.txt" |> readVents |> solve1 // 6311

let answerExample2 = readLines "Day5-example.txt" |> readVents |> solve2 // 12

let answer2 = readLines "Day5.txt" |> readVents |> solve2 // 19929
