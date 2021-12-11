module aoc2021.Day11

open Helpers
open System
open Colorify

let GLOBAL_PRINT_GRIDS = false
let printfC s c = (Format (UI.Theme.Dark)).Write (s, c)

let printGrid (grid: Map<_,_>) =
    printfn ""

    grid
    |> Seq.groupBy (fun kvp -> snd kvp.Key)
    |> Seq.map (fun (_, kvps) -> kvps)
    |> Seq.map (fun line -> line |> Seq.map (fun (v) -> v))
    |> Seq.iter (fun line ->
        for kvp in line do
            let color =
                match kvp.Value with
                | 0 -> Colors.txtWarning
                | _ -> Colors.txtDefault

            printfC $"%A{kvp.Value}" color

        printfn "")

let inspectGrid (grid: Map<_, int>) =
    if GLOBAL_PRINT_GRIDS then
        printGrid grid

    grid

let parseGrid (lines: string []) =
    Map.ofSeq (
        seq {
            for (y, line) in lines |> Seq.indexed do
                for (x, v) in line |> Seq.indexed do
                    yield (x, y), int (string v)
        }
    )

let adjacent (x, y) =
    seq {
        for yOff in [ -1 ; 0 ; 1 ] do
            for xOff in [ -1 ; 0 ; 1 ] do
                let nx = x + xOff
                let ny = y + yOff

                if not (nx = x && ny = y) then
                    yield nx, ny
    }

type EnergySource =
    | Initial
    | Flash

let increase v src inc =
    match src with
    | Flash when v = 0 -> 0
    | _ -> Math.Min (10, (v + inc)) % 10

let rec step flashed state =
    let newFlashes =
        flashed
        |> Set.difference (
            state
            |> Map.filter (fun _ v -> v = 0)
            |> Seq.map (fun kvp -> kvp.Key)
            |> Set.ofSeq
        )

    if newFlashes.Count = 0 then
        flashed, state
    else

    let neighborCount =
        newFlashes
        |> Seq.collect adjacent
        |> Seq.groupBy id
        |> Seq.map (fun (xy, xys) -> xy, xys |> Seq.length)
        |> Map.ofSeq
        |> fun neighbors -> fun k -> neighbors |> Map.tryFind k |> Option.defaultValue 0

    let newState =
        state
        |> Map.map (fun k v -> increase v Flash (neighborCount k))
        |> inspectGrid

    step (Set.union flashed newFlashes) newState

let rec steps state =
    seq {
        let next =
            state
            |> Map.map (fun _ v -> increase v Initial 1)
            |> step Set.empty

        yield next
        yield! steps (snd next)
    }

let fullyFlashedGrid (flashes: Set<_>, grid: Map<_,_>) = Seq.forall flashes.Contains grid.Keys

let solve1 stepCount grid =
    steps grid
    |> Seq.truncate stepCount
    |> Seq.fold (fun s (flashes, _) -> s + Set.count flashes) 0

let solve2 grid =
    steps grid
    |> Seq.tryFindIndex fullyFlashedGrid
    |> Option.map ((+) 1)

let answerStepExample = readLines "Day11-example-steps.txt" |> parseGrid |> solve1 6

let answerExample1 = readLines "Day11-example.txt" |> parseGrid |> solve1 100 // 1656
let answer1 = readLines "Day11.txt" |> parseGrid |> solve1 100 // 1667

let answerExample2 = readLines "Day11-example.txt" |> parseGrid |> solve2 // 195
let answer2 = readLines "Day11.txt" |> parseGrid |> solve2 // 488
