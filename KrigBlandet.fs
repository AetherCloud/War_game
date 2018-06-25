open System.Collections.Generic
open System

let mutable fightCounter = 1

let rand = new System.Random()

let swap (a: _[]) x y =
    let tmp = a.[x]
    a.[x] <- a.[y]
    a.[y] <- tmp

// shuffle an array (in-place)
let shuffle a = Array.iteri (fun i _ -> swap a i (rand.Next(i, Array.length a))) a


let blandet () = 
  let a = 
    [for i in 2..14 do
      yield i]
  let b = a@a@a@a
  let c = List.toArray b
  shuffle c
  let (d1,d2) = List.splitAt (c.Length/2) (Array.toList c)
  (d1, d2)

let getAndRemove (d: LinkedList<int>) =
  let value = d.First.Value
  d.RemoveFirst()
  value

let lootWinnings (w: int list) (deck: LinkedList<int>) =
  for i in w do
    deck.AddFirst(i) |> ignore

let rec doFight (d1: LinkedList<int>) (d2: LinkedList<int>) (winnings: int list)= 
  Console.Write("Dette er kamp nummer: "+(string fightCounter))
  fightCounter <- fightCounter+1
  try
    let i1 = getAndRemove d1
    let i2 = getAndRemove d2
    match (compare i1 i2) with
    | 0 -> //equal
      let newWinnings = winnings@[getAndRemove d1;getAndRemove d1;getAndRemove d1;getAndRemove d2;getAndRemove d2;getAndRemove d2]
      Console.WriteLine(", og der er krig!")
      doFight d1 d2 newWinnings
    | -1 -> //d2 wins
      lootWinnings winnings d2
      d2.AddFirst(i1) |> ignore
      d2.AddFirst(i2) |> ignore
      Console.WriteLine(", og spiller 2 vandt!")
      doFight d1 d2 List.empty
    | 1 -> //d1 wins
      lootWinnings winnings d1
      d1.AddFirst(i1) |> ignore
      d1.AddFirst(i2) |> ignore
      Console.WriteLine(", og spiller 1 vandt!")
      doFight d1 d2 List.empty
  with
    | :? System.NullReferenceException -> Console.WriteLine("\n")
                                          let s = 
                                            if(d1.Count = 0) then "Spiller 2 har vundet!"
                                            else "Spiller 1 har vundet!"
                                          printfn "%s" s

[<EntryPoint>]
let krig s =
  fightCounter <- 0
  let (d1, d2) = blandet()
  let lld1 = new LinkedList<int>(d1)
  let lld2 = new LinkedList<int>(d2)
  doFight lld1 lld2 []
  Console.WriteLine()
  printfn "Klik på en vilkårlig knap for at afslutte..."
  System.Console.ReadKey() |> ignore
  fightCounter