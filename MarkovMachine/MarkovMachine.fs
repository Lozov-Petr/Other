module MarkovMachine

type MarkovMachine(listSample : (string * string) list) =

    member x.Run(str : string, printIntermediate : bool) =

        let rec work (str : string) list =
            match list with
            | (str1 : string, str2) :: tl ->
                let numSym = str.IndexOf(str1)
                if numSym <> -1 then let newStr = str.Remove(numSym, str1.Length).Insert(numSym, str2)
                                     if printIntermediate then printfn "%s" newStr
                                     work newStr listSample
                                else work str tl
            | _ -> str
        printfn "%s" str 
        work str listSample

    member x.Run(str : string) = x.Run(str, true)
        
