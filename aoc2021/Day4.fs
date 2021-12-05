module aoc2021.Day4

open Helpers

type BingoNumber = string
type BingoBoard = BingoNumber [,]

type BingoBoardState =
    | Counting of board: BingoBoard * rows: Set<BingoNumber> [] * cols: Set<BingoNumber> []
    | Bingo of BingoBoard

let readBoards boardLines =
    boardLines
    |> Array.filter notWhitespace
    |> Array.chunkBySize 5
    |> Array.map (fun board ->
        board
        |> Array.map (fun line -> line.Split " " |> Array.filter notWhitespace))

let isBingo =
    function
    | Bingo _ -> true
    | _ -> false

let readBingoInput (lines: string []) =
    let boardArrays = readBoards lines.[2..]

    let boards =
        boardArrays
        |> Array.map (fun board -> Array2D.init 5 5 (fun i j -> board.[i].[j]))

    let draws = lines.[0] |> fun line -> line.Split ","

    draws, boards

let initialBoardStates boards =
    let initBoard (board: BingoBoard) =
        let rows = [| 0..4 |] |> Array.map (fun i -> Set.ofSeq board[i, *])
        let cols = [| 0..4 |] |> Array.map (fun i -> Set.ofSeq board[*, i])

        Counting (board, rows, cols)

    boards |> Array.map initBoard

let updateBoard (draw: BingoNumber) =
    function
    | Bingo b -> Bingo b
    | Counting (board, rows, cols) ->
        let rows = rows |> Array.map (Set.remove draw)
        let cols = cols |> Array.map (Set.remove draw)

        match Array.append rows cols |> Array.exists Set.isEmpty with
        | true -> Bingo board
        | false -> Counting (board, rows, cols)

let calculateBoardScore (board: BingoBoard) (draws: list<BingoNumber>) =
    let allNumbers =
        [| 0..4 |]
        |> Array.collect (fun i -> [| 0..4 |] |> Array.map (fun j -> board[i, j]))

    let sum =
        Set.difference (Set.ofSeq allNumbers) (Set.ofSeq draws)
        |> Set.map int
        |> Seq.sum

    sum * int draws.Head

let getBingoScore draws =
    function
    | Bingo board -> Some (calculateBoardScore board draws)
    | _ -> None

let solve1 (draws, boards) =
    let findWinningBoard (state, draws, _) draw =
        let newState = state |> Array.map (updateBoard draw)
        let winner = newState |> Seq.tryPick (getBingoScore (draw :: draws))

        newState, draw :: draws, winner

    ((initialBoardStates boards, [], None), draws)
    ||> Array.scan findWinningBoard
    |> Array.tryPick (fun (_, _, score) -> score)

let solve2 (draws, boards) =
    let findLastWinningBoard (state, draws, _) draw =
        let newState = state |> Array.map (updateBoard draw)

        let lastWinningBoard =
            match newState |> Array.forall isBingo with
            | true ->
                Array.except state newState
                |> Array.tryPick (getBingoScore (draw :: draws))
            | false -> None

        newState, draw :: draws, lastWinningBoard

    ((initialBoardStates boards, [], None), draws)
    ||> Seq.scan findLastWinningBoard
    |> Seq.tryPick (fun (_, _, score) -> score)

let answerExample1 = readLines "Day4-example.txt" |> readBingoInput |> solve1 // 4512

let answer1 = readLines "Day4.txt" |> readBingoInput |> solve1 // 35670

let answerExample2 = readLines "Day4-example.txt" |> readBingoInput |> solve2 // 148 * 13 = 1924

let answer2 = readLines "Day4.txt" |> readBingoInput |> solve2 // head: 88, sum: 258, 22704
