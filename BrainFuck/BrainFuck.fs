module mBrainFuck

open System
open mTape 

type BrainFuck(s : string) =
    
    let tape = new Tape()
    let mutable count = 0

    let readNumber() =
        Console.ReadLine() |> int
    
    member private x.Next()   = count <- count + 1 
    member private x.IsZero() = tape.Write() = 0

    member private x.MoveRight() =
      
        let rec NextStep c n =
            if c = s.Length then failwith "Error in input."
            match s.[c] with
            | '['            -> NextStep (c + 1) (n + 1)
            | ']' when n > 1 -> NextStep (c + 1) (n - 1)
            | ']'            -> c
            |  _             -> NextStep (c + 1)  n  
        
        if x.IsZero() then count <- NextStep count 0

    member private x.MoveLeft() =
      
        let rec NextStep c n =
            if c = -1 then failwith "Error in input."
            match s.[c] with
            | ']'            -> NextStep (c - 1) (n + 1)
            | '[' when n > 1 -> NextStep (c - 1) (n - 1)
            | '['            -> c
            |  _             -> NextStep (c - 1)  n  
        
        if not <| x.IsZero() then count <- NextStep count 0

    member private x.ShiftLeft()  = tape.ShiftLeft()
    member private x.ShiftRight() = tape.ShiftRight()
    member private x.Incr()       = tape.Incr()
    member private x.Decr()       = tape.Decr()
    member private x.Read()       = Console.ReadLine() |> int |> tape.Read
    member private x.Write()      = tape.Write() |> printfn "%A" 
    
    member x.Run log = 
        if count < s.Length then
            match s.[count] with
            | '<'  -> x.ShiftLeft()
            | '>'  -> x.ShiftRight()
            | '+'  -> x.Incr()
            | '-'  -> x.Decr()
            | '.'  -> x.Write()
            | ','  -> x.Read()
            | '['  -> x.MoveRight()
            | ']'  -> x.MoveLeft()
            | ' '  -> x.Next()
            | '\n' -> x.Next()
            |  _   -> failwith "Error in input."
            if log then printfn "%c - %A" (s.[count]) tape
            x.Next()
            x.Run(log)