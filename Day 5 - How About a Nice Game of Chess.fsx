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


/// crack the password for a given door ID
let crackId doorId =
    let hash = hashID doorId
    let fiveLeadingZeroes str = str |> Seq.take 5 |> Seq.forall ((=) '0')

    printf "Getting code: "
    Seq.initInfinite id
    |> Seq.map hash
    |> Seq.filter fiveLeadingZeroes
    |> Seq.take 8
    |> Seq.map (fun md5Hash -> md5Hash.[5])


let part1() =
    getInput()
    |> crackId
    |> Seq.iter (fun x -> printf "%c" x)
    printfn "\nDone"


let test1() =
    let doorId = "abc"
    let expected = "18f47a30"
    let result = crackId doorId |> Array.ofSeq |> fun x -> new System.String(x)
    if result.ToLower() = expected then
        printfn "Test 1 passed"
    else
        printfn "Test 2 failed - got: %s" result


part1()
// test1()
1