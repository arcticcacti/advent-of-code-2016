let keypad =
    array2D [|
            [|1; 2; 3|]
            [|4; 5; 6|]
            [|7; 8; 9|]
            |]

// starting position i.e. '5'
let startPosition = (1,1)

type Move = U | D | L | R

(*
    Input functions
*)

let getInput() =
    let path = "input/day_2_bathroom_security.txt"
    System.IO.File.ReadAllLines(path)

let parseMove =
    function
    | 'U' | 'u' -> U
    | 'D' | 'd' -> D
    | 'L' | 'l' -> L
    | 'R' | 'r' -> R
    | ch -> failwithf "Unrecognised move token %c" ch


(*

*)

/// the key number at a given position (top-left is 0,0)
let keyAtPosition (x, y) =
    try
        Some keypad.[y, x]
    with
    | :? System.IndexOutOfRangeException -> None
    
/// coordinates after moving in a given direction
let shift (x, y) move =
    match move with
    | U -> x  , y-1
    | D -> x  , y+1
    | L -> x-1, y
    | R -> x+1, y

/// move from a key to an adjacent one, remaining if the move cannot be made
let doMove from move =
    let newPosition = shift from move
    match keyAtPosition newPosition with
    | None -> from
    | Some x -> newPosition 


/// the final position after following a sequence of moves from a start position
let followPath start moves =
    moves
    |> Seq.map parseMove
    |> Seq.fold doMove start


/// get the keypad code
let getCode moveSeqs =
    let keyCollector start =
        followPath start >> fun endPos -> keyAtPosition endPos, endPos

    moveSeqs
    |> Array.mapFold keyCollector startPosition
    |> fst
    |> Array.collect Option.toArray

(*
    Runnables
*)

let test() =
    let input = [|"ULL"; "RRDDD"; "LURDL"; "UUUUD"|]
    let expected = [|1; 9; 8; 5|]
    let result = getCode input
    if result = expected then
        printfn "Test passed"
    else
        printfn "Test failed\nExpected: %A\nGot:      %A" expected result

let part1() =
    getInput()
    |> getCode
    |> Seq.map (sprintf "%d")
    |> String.concat ""
    |> printfn "Code is: %A"

    
part1()
test()
1