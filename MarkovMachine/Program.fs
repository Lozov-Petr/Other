module Program

open ExamplesOfMarkovMachines

[<EntryPoint>]
let main argv =
    let res1 = ``a+b=? -> c``.Run("1+99=?")
    let res2 = ``a+b=? -> a+b=c``.Run("1+99=?")
    let res3 = ``a++``.Run("99++")
    0