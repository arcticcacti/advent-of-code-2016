
/// read a file in the data directory
let inputFromFile filename =
    let pathToFiles = @"input\"
    System.IO.File.ReadAllLines (pathToFiles + filename)

/// compare values with a predicate and print the results
let printTestResult testId f expected result =
    if result |> f expected then
        printfn "Test (%A) passed" testId
    else
        printfn "Test (%A) failed\nExpected: %A\n Got: %A" testId expected result

/// test f(input) = expected, adding the input to the testId in the output string
let testResultIsExpected testId f (input, expected) =
    let shouldBe = printTestResult (sprintf "%s - %A" testId input) (=)
    f input |> shouldBe expected