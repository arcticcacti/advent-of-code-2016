open System.Security.Cryptography
open System.Text

let getInput() = "reyedfim"

let md5 = MD5.Create()


/// append an index to an ID and hash the result
let hashID id index =
    let stuff = id + string index
    Encoding.ASCII.GetBytes stuff
    |> md5.ComputeHash
    |> Seq.map (fun c -> c.ToString("X2"))
    |> Seq.reduce (+)


/// get a valid password character and its position
let passwordChar passwordLength (hash:string) =
    // 6th and 7th chars hold the index and char respectively
    let i = hash.[5]
    let c = hash.[6]
    match System.Int32.TryParse(string i) with
    | true, x when x >= 0 && x <= passwordLength-1
        -> Some(x, c)
    | true, _
        -> None
    | _ 
        -> None


/// generate valid hashes for a given door ID
let validHashes doorId =
    let hash = hashID doorId
    let fiveLeadingZeroes str = str |> Seq.take 5 |> Seq.forall ((=) '0')

    Seq.initInfinite id
    |> Seq.map hash
    |> Seq.filter fiveLeadingZeroes

let printCharInline = fun x -> printf "%c" x; x

let charsToString = Array.ofSeq >> fun arr -> new System.String(arr)


(*
    Main functions
*)

let part1 input =
    let codeChar (hash:string) = hash.[5]

    printf "Getting code: "
    input
    |> validHashes
    |> Seq.take 8
    |> Seq.map (codeChar >> printCharInline)
    |> charsToString
    |> fun x -> printfn "\nDone"; x


let part2 input =
    let setChar array (idx, ch) = Array.set array idx ch; array
    /// display a 'live' view of the password array that updates as entries are added
    let setCharAndDisplayProgress array =
        setChar array >> fun x -> printf "\r[%s]" (charsToString x); x

    let passwordLength = 8

    // Get valid password index/char pairs, and combine the first result for each index into an array
    printfn "Getting code: "
    input
    |> validHashes
    |> Seq.choose (passwordChar passwordLength)
    |> Seq.distinctBy fst
    |> Seq.take passwordLength
    |> Seq.fold setCharAndDisplayProgress (Array.zeroCreate passwordLength)
    |> charsToString
    |> fun x -> printfn "\nDone"; x


(*
    Tests
*)


let printTestResult testId result expected =
    if result = expected then
        printfn "Test %A passed" testId
    else
        printfn "Test %A failed - got: %s" testId result

let test1() =
    let doorId = "abc"
    let expected = "18f47a30"
    let result = part1 doorId
    printTestResult 1 (result.ToLower()) expected

let test2() =
    let doorId = "abc"
    let expected = "05ace8e3"
    let result = part2 doorId
    printTestResult 2 (result.ToLower()) expected




part1 <| getInput()
part2 <| getInput()
test1()
test2()
1