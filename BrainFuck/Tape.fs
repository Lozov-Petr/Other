module mTape

type Tape() =
    
    let mutable left  = []
    let mutable right = []
    let mutable cell  = 0

    member x.ShiftRight() =
        left  <- cell :: left
        cell  <- if right.IsEmpty then 0  else right.Head  
        right <- if right.IsEmpty then [] else right.Tail

    member x.ShiftLeft() =
        right <- cell :: right
        cell  <- if left.IsEmpty then 0  else left.Head  
        left  <- if left.IsEmpty then [] else left.Tail

    member x.Incr() = cell <- cell + 1
    member x.Decr() = cell <- cell - 1

    member x.Write() = cell
    member x.Read(c) = cell <- c 

    override x.ToString() = 
        let printList = List.map string >> String.concat " "
        sprintf "[%s {%d} %s]" 
            (left |> List.rev |> printList) 
             cell
            (printList right)  
