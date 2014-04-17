let rnd = new System.Random()
 
type Pizza() =
    let tasty = rnd.Next(2) > 0
    member this.Tasty = tasty
 
 
[<AbstractClass>]
type Alive() = 
    let mutable dead = false
    abstract member Eat : unit -> unit
    member this.Dead = dead
    member this.Die () =
        dead <- true
    
 
type Human(name:string) = 
    inherit Alive()
    let mutable mood = 7
    member this.Name = name
    member this.Mood = mood
    override this.Eat () = mood <- mood + 3
 
 
type NinjaTurtle(name:string, maskColor:string) =
    inherit Human(name)
    let mutable mood = 10
    member this.MaskColor = maskColor
   
    override this.Eat () = 
        let pizza = new Pizza()
        printfn "%s eats %s pizza." this.Name (if pizza.Tasty then "tasty" else "awful")
        mood <- mood + if pizza.Tasty then 5 else 1
        if mood > 7 then printfn "His mood is good enough to win a battle."
 
 
type Reporter() =
    inherit Human("April")
    member this.CaptureNews (winner:Human) (looser:Human) =
        if looser.GetType () = typedefof<NinjaTurtle> then
            printfn "=== SHOCKING NEWS === "
            printfn "%s's just killed %s!" winner.Name looser.Name
        else
            printfn "%s has destroyed enemy! Cool. :)" winner.Name
        
 
type BadGuy() =
    inherit Human("Shredder")
    member this.AttackTurtles (turtles: NinjaTurtle array) (reporter: Reporter) =
        let turtle = turtles.[rnd.Next(turtles.Length)]
        printfn "%s attacks %s." this.Name turtle.Name
        
        if turtle.Mood > this.Mood then
            this.Die ()
        else turtle.Die ()
 
        if turtle.Dead then
            reporter.CaptureNews this turtle
        else reporter.CaptureNews turtle this
 
 
let turtles = [|new NinjaTurtle("Leonardo", "blue"); new NinjaTurtle("Michelangelo", "orange"); new NinjaTurtle("Donatello", "purple"); new NinjaTurtle("Raphael", "red")|]
let badGuy = new BadGuy()
let reporter = new Reporter()
 
for turtle in turtles do
    turtle.Eat ()
 
badGuy.AttackTurtles turtles reporter