

type Dir  = N | E | S | W
type Turn = L | R
type Move = Turn * int


(*
      Input parsing
*)

let getInput =
    let path = "input\day_1_taxicab.txt"
    System.IO.File.ReadAllLines(path) |> Array.head

let splitSteps (input:string) =
    input.Split([|", "|], System.StringSplitOptions.None)

/// parse a move string into a (Turn, distance) pair
let parseMove (move:string) :Move =
    let rotation, distance = char move.[0], int move.[1..]
    match rotation with
    | 'L' -> L, distance
    | 'R' -> R, distance
    |  d  -> failwithf "Unrecognised turn direction %c !" d

let allMoves = getInput |> splitSteps |> Array.map parseMove

(*
     Movement functions
*)

/// The direction you face when turning left or right from a starting direction
let turn from rotation =
    match from, rotation with
    | N, L | S, R -> W
    | W, L | E, R -> S
    | S, L | N, R -> E
    | E, L | W, R -> N


/// The position one step away from a start position
let step direction from =
    let x, y = from
    match direction with
    | N -> x  , y+1
    | S -> x  , y-1
    | W -> x+1, y
    | E -> x-1, y


/// A list of the positions in a path, moving a distance in a direction from a starting position
let path direction from distance =
    let rec walk visited from steps =
        if steps < 1 then failwithf "Can't take less than 1 step! %d passed" steps
        let here = step direction from
        let currentPath = here::visited

        match steps with
        | 1 -> currentPath
        | d -> walk currentPath here (d-1)
        
    walk [] from distance


/// The path taken and final facing direction after performing a Move
let doMove (currentPos, currentFacing) move =
    let rotation, distance = move
    let newFacing = turn currentFacing rotation
    let pathTaken = path newFacing currentPos distance
    pathTaken, newFacing


/// folding function that creates a path from a previous path and a Move
let nextPath (lastPath, facing) = doMove (List.head lastPath, facing)


/// folding function that moves between (position, facing) states
let nextPosition state =
    doMove state >> fun (path, facing) -> List.head path, facing


/// taxicab distance of a position from the origin
let blocksAway =
    function
    | None -> None
    | Some(x,y) -> Some(abs x + abs y)


/// the first element of a list that has a later duplicate
let rec firstNonUnique list =
    match list with
    | [] -> None
    | x::xs -> match (List.tryFind ((=)x) xs) with
               | None -> firstNonUnique xs
               | found -> found

let facingAtStart = N


let part1() =
    allMoves
    |> Array.fold nextPosition ((0,0), facingAtStart)
    |> fst
    |> Some
    |> blocksAway
    |> printfn "Final location: %A blocks away"


let part2() =
    allMoves
    |> Seq.scan nextPath ([0,0], facingAtStart)
    |> Seq.map (fst>>List.rev)
    |> List.concat
    |> firstNonUnique
    |> blocksAway
    |> printfn "First location revisited is %A blocks away"

part1()
part2()
1