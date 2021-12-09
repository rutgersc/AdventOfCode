module aoc2021.Day9

open Helpers
open System.Collections.Generic

let parseHeightmap (lines: string []) =
    lines
    |> Array.map (fun l -> l.ToCharArray () |> Array.map (string >> int64))

let neigbours x y =
    seq {
        yield (x + 1), y
        yield x, (y + 1)
        yield (x - 1), y
        yield x, (y - 1)
    }

let index input x y = input |> Array.tryItem y |> Option.bind (Array.tryItem x)

let points input =
    seq {
        for y in [ 0 .. Seq.length input - 1 ] do
            for x in [ 0 .. (Seq.head input |> Seq.length) - 1 ] do
                yield (x, y), Option.get (index input x y)
    }

let lowPoints input =
    seq {
        for (x, y), v in points input do
            let min =
                neigbours x y
                |> Seq.collect (uncurry (index input) >> Option.toList)
                |> Seq.min

            if v < min then yield (x, y), v
    }

let solve1 = lowPoints >> Seq.sumBy (snd >> (+) 1L)

let solve2 (input: int64 [] []) =
    let points = points input |> Seq.filter (fun (_, v) -> v < 9) |> Map.ofSeq

    let addToBasin basins (p: KeyValuePair<_, _>) =
        let neigbours = p.Key |> uncurry neigbours

        let currentBasin =
            let basinKeys = Set.ofSeq neigbours
            fun basin -> basinKeys |> Set.exists (fun s -> Map.containsKey s basin)

        let basins, otherBasins = basins |> List.partition currentBasin

        let basin =
            match Seq.isEmpty basins with
            | true -> Seq.singleton (p.Key, p.Value)
            | false -> basins |> Seq.collect Map.toSeq
            |> Seq.append (
                neigbours
                |> Seq.choose (fun neighbor -> Map.tryFind neighbor points |> Option.map (fun v -> neighbor, v))
            )
            |> Map.ofSeq

        basin :: otherBasins

    points
    |> Seq.fold addToBasin []
    |> Seq.map (fun b -> b.Count)
    |> Seq.sortDescending
    |> Seq.truncate 3
    |> Seq.fold (*) 1

let answerExample1 = readLines "Day9-example.txt" |> parseHeightmap |> solve1 // 15
let answer1 = readLines "Day9.txt" |> parseHeightmap |> solve1 // 496

let answerExample2 = readLines "Day9-example.txt" |> parseHeightmap |> solve2 // 1134
let answer2 = readLines "Day9.txt" |> parseHeightmap |> solve2 // 902880
