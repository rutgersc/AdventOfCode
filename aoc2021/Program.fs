

open System
open aoc2021


let struct (x,y) = Console.GetCursorPosition();

let grid =
    Helpers.readLines "Day11.txt"
    |> Day11.parseGrid
    |> Day11.steps
    |> Helpers.Seq.inspect (fun (flashes, grid) ->
                                        Console.SetCursorPosition (x,y)
                                        System.Threading.Thread.Sleep 500
                                        Day11.printGrid grid)
    |> Seq.tryFindIndex Day11.fullyFlashedGrid


exit 100
