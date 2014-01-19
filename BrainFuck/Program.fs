module Program

open System.IO
open mBrainFuck

[<EntryPoint>]
let main argv =
    
    let path = 
        let a = Array.tryFindIndex (fun (s : string) -> s.ToLower() = "-path") argv
        if a.IsSome && a.Value < argv.Length
           then argv.[a.Value + 1] 
           else failwith "Path not found."
           
    use streamReader = new StreamReader(path)
    let code = streamReader.ReadToEnd()
    let bf = new BrainFuck(code)
    bf.Run(Array.exists ((=) "-log") argv)  
    
    0

