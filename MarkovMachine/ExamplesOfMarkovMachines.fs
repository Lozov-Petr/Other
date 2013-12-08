module ExamplesOfMarkovMachines

open MarkovMachine

let ``a+b=? -> c`` =
    let list1 = [for i in 0..8 -> (string i + "#", string (i + 1))] @ [("9#", "#0"); ("#", "1")]
    let list2 = [for i in 0..9 -> (string i + "*", "*" + string i)]
    let list3 = [for i in 1..9 -> (string i + "!", "*" + string (i - 1))] @ [("0!", "!9")]
    let list4 = [("0=", "!9=")] @ [for i in 1..9 -> (string i + "=", "*" + string (i - 1) + "=")]
    let list  = [("+*", "#+")] @ list1 @ [("+0", "+")] @ list2 @ list3 @ list4 @ [("+=?", "")]
    new MarkovMachine(list)

let ``a+b=? -> a+b=c`` =
    let lNum  = [for i in 0..9 -> string i] @ ["="; "+"]
    let lChar = [for i in 'a'..'j' -> string i] @ ["$"; "|"]
    let list1 = [for i in 0..11 -> (lNum.[i] + "?", "?" + lNum.[i] + lChar.[i])] @ [("?", "")]
    let list2 =  List.fold (fun acc i -> acc @ [for j in lNum -> (i + j, j + i)]) [] lChar
    let list3 = [for i in 0..10 -> lChar.[i] + "=", "=" + lChar.[i]]
    let list4 = ("|*", "#|")::[for i in 0..8 -> (lChar.[i] + "#", lChar.[i + 1])] @ [("j#", "#a"); ("#", "b")]
    let list5 = ("|a", "|")::[for i in 0..9 -> (lChar.[i] + "*", "*" + lChar.[i])]
    let list6 = [for i in 1..9 -> (lChar.[i] + "!", "*" + lChar.[i - 1])] @ [("a!", "!j")]
    let list7 = [("a$", "!j$")] @ [for i in 1..9 -> (lChar.[i] + "$", "*" + lChar.[i - 1] + "$")]
    let list8 = ("|$", "%")::[for i in 0..9 -> (lChar.[i] + "%", "%" + lNum.[i])] 
    let list  = list1 @ list2 @ list3 @ list4 @ list5 @ list6 @ list7 @ list8 @ [("%", "")]
    new MarkovMachine(list)

let ``a++`` =
    let lNum = [for i in 0..9 -> string i]
    let list = [for i in 0..8 -> (lNum.[i] + "++", lNum.[i + 1])] @ [("9++", "++0"); ("++","1")]
    new MarkovMachine(list) 