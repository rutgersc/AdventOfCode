module aoc2021.Day3

open System
open Types
open Helpers
open System

let spanToROSpan (span : Span<'a>) : ReadOnlySpan<'a> =
    Span<_>.op_Implicit(span)


let v (input: string[]) =
    let f (state: int[]) (curr: string) =
        for i = 0 to curr.Length - 1 do
            state.[i] <- state.[i] + (if curr.[i] = '1' then 1 else -1)

        state

    input
    |> Seq.fold f (Array.zeroCreate input.[0].Length)
    |> Array.map (fun count -> if count > 0 then 1uy else 0uy)
    // |> fun i -> printfn $"%A{i}"


let hmm =
    let bitblock  = readLines "Day3-example.txt"
    let aiai = v bitblock
    let z = System.Text.Encoding.ASCII.GetBytes("10110")
    let o = System.Text.Encoding.ASCII.GetString(aiai)

//    let v = v.AsSpan()
//    let hmmm = aiai.AsSpan() :> ReadOnlySpan<byte>
//    let hmmm = aiai.AsSpan() :> ReadOnlySpan<_>.op_Implicit
//    let z = BitConverter.ToInt64 (Span<_>.op_Implicit(aiai.AsSpan()))

//    let z = BitConverter.ToInt16(z)
//Int
    printfn $"{aiai} {v}"
    ()
