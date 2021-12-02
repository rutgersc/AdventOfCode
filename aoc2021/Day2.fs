module aoc2021.Day2

open Types
open Helpers

type SubmarineCommand =
    | Forward of int
    | Down of int
    | Up of int

let parseCommand (str: string) =
    match str.Split (" ") with
    | [| "forward" ; v |] -> Forward (int v)
    | [| "down" ; v |] -> Down (int v)
    | [| "up" ; v |] -> Up (int v)
    | _ -> failwith "unknown command"

type SubmarineState = { Horizontal: int ; Depth: int ; Aim: int }

let initialState: SubmarineState = { Horizontal = 0 ; Depth = 0 ; Aim = 0 }

let multiplyState state = state.Horizontal * state.Depth

let evolve (state: SubmarineState) (cmd: SubmarineCommand) : SubmarineState =
    match cmd with
    | Forward v -> { state with Horizontal = state.Horizontal + v }
    | Down v -> { state with Depth = state.Depth + v }
    | Up v -> { state with Depth = state.Depth - v }

let evolve2 (state: SubmarineState) (cmd: SubmarineCommand) : SubmarineState =
    match cmd with
    | Forward v ->
        { state with
            Horizontal = state.Horizontal + v
            Depth = state.Depth + (state.Aim * v) }
    | Down v -> { state with Aim = state.Aim + v }
    | Up v -> { state with Aim = state.Aim - v }

let solve1 commands =
    commands
    |> Seq.fold evolve initialState
    |> multiplyState

let solve2 commands =
    commands
    |> Seq.fold evolve2 initialState
    |> multiplyState

let readCmds f = readLines f |> Seq.map parseCommand

let answerExample1 = readCmds "Day2-example.txt" |> solve1
let answerExample2 = readCmds "Day2-example.txt" |> solve2

let answer1 = readCmds "Day2.txt" |> solve1
let answer2 = readCmds "Day2-2.txt" |> solve2
