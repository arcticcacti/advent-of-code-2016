#load "AdventUtils.fsx"

let getInput() = AdventUtils.inputFromFile "day_7.txt"

/// split an ordered sequence into its odd and even elements
let groupAlternating =
    let i = ref -1
    Seq.groupBy (fun _ -> incr i; !i % 2)
    >> Seq.map snd
    >> Seq.take 2
    >> Seq.toArray
    >> fun arrs -> arrs.[0], arrs.[1]


/// check if four elements form an ABBA
let isAbba =
    function
    | a,b,c,d when a=d && b=c && a<>b
        -> true
    | _ 
        -> false


/// whether an input contains an ABBA sequence
let hasAbba s =
    s
    |> Seq.windowed 4
    |> Seq.map (fun a->a.[0], a.[1], a.[2], a.[3])
    |> Seq.exists isAbba


/// split a string into alternating 'normal' sections and sections that were enclosed in square brackets
let splitIp (ip:string) = ip.Split([|'['; ']'|])


/// determine if an IP string conforms to the rules for TLS support
let supportsTls ip =
    let normals, hypernets = ip |> splitIp |> groupAlternating
    if hypernets |> Seq.exists hasAbba then
        false
    else normals |> Seq.exists hasAbba


//
// Main functions
//

let part1() =
    getInput()
    |> Seq.filter supportsTls
    |> Seq.length
    |> printfn "Part 1 - %d IPs support TLS"


// Tests

let test1() =
    let testData = [|
        "abba[mnop]qrst", true;
        "abcd[bddb]xyyx", false;
        "aaaa[qwer]tyui", false;
        "ioxxoj[asdfgh]zxcvbn", true
    |]
    let test = AdventUtils.testResultIsExpected "1" supportsTls 
    testData |> Array.iter test


part1()

test1()