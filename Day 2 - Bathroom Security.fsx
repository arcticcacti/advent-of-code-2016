/// keypad layout - spaces represent non-keys, i.e. invalid coords
let basicKeypad =
    array2D [|
            [|'1'; '2'; '3'|]
            [|'4'; '5'; '6'|]
            [|'7'; '8'; '9'|]
            |]

let diamondKeypad =
    array2D [|
            [|' '; ' '; '1'; ' '; ' ' |]
            [|' '; '2'; '3'; '4'; ' ' |]
            [|'5'; '6'; '7'; '8'; '9' |]
            [|' '; 'A'; 'B'; 'C'; ' ' |]
            [|' '; ' '; 'D'; ' '; ' ' |]
            |]

// starting positions i.e. '5'
let startPosition1 = (1,1)
let startPosition2 = (0,2)

type Move = U | D | L | R

(*
    Input data handling
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

/// the key at a given position on a keypad (top-left is 0,0)
let keyAtPosition (keypad:char[,]) (x, y) =
    try
        match keypad.[y, x] with
        | ' ' -> None
        | c -> Some c
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
let doMove keypadMap from move =
    let newPosition = shift from move
    match keypadMap newPosition with
    | None -> from
    | Some x -> newPosition 


/// the final position after following a sequence of moves from a start position
let followPath keypadMap start moves =
    moves
    |> Seq.map parseMove
    |> Seq.fold (doMove keypadMap) start


/// get the keypad code
let getCode keypadMap firstKeyPos moveSeqs =
    let keyCollector start =
        followPath keypadMap start >> fun endPos -> keypadMap endPos, endPos

    moveSeqs
    |> Array.mapFold keyCollector firstKeyPos
    |> fst
    |> Array.collect Option.toArray

(*
    Runnables
*)

let part1() =
    let keypadMap = keyAtPosition basicKeypad
    getInput()
    |> getCode keypadMap startPosition1
    |> Seq.map (sprintf "%c")
    |> String.concat ""
    |> printfn "Code is: %A"


let part2() =
    let keypadMap = keyAtPosition diamondKeypad
    getInput()
    |> getCode keypadMap startPosition2
    |> Seq.map (sprintf "%c")
    |> String.concat ""
    |> printfn "Code is: %A"


(*
    Tests
*)

let runTest testId keypad startPos input expected =
    let keypadMap = keyAtPosition keypad
    let result = getCode keypadMap startPos input
    if result = expected then
        printfn "Test %A passed" testId
    else
        printfn "Test %A failed\nExpected: %A\nGot:      %A" testId expected result


let part2Test() =
    let input = [|"ULL"; "RRDDD"; "LURDL"; "UUUUD"|]
    let expected = [|'5'; 'D'; 'B'; '3'|]
    runTest 2 diamondKeypad startPosition2 input expected
    

let part1Test() =
    let input = [|"ULL"; "RRDDD"; "LURDL"; "UUUUD"|]
    let expected = [|'1'; '9'; '8'; '5'|]
    runTest 1 basicKeypad startPosition1 input expected


part1()
part2()
part1Test()
part2Test()
1