// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Utils

type nat = Cero | Suc of nat

let in_out = [([1], 1)
              ([6;1], 7)
              ([0;21;15], 36)
              ([0;1;2;3;4;5], 15)
              ([1;3;5;7;9], 25)
              ([12;9;6;3], 30)
              ([3;6;12;9;6;3], 39)
              ([1;2;3;4;5;6;7], 28)
              ([1;2;3;4;5;6;7;8], 36)
              ([9;8;7;6;5;4;3;2;1],45)]

let fitness f = 
        List.sumBy (fun (input, output) -> 
                            if f input = output
                            then 1.0
                            else 0.0) in_out

// Original scheme
let scheme =
    <@@ fun M ->
         let rec f x = match x with
                        [] -> 0
                      | x :: xs -> M x xs ((+) : int->int->int) f
         fun () -> fitness f @@>

[<EntryPoint>]
let main argv =
    printfn "Starting..."
    let closure = Utils.closure 10 scheme
    let term_size = 10
    let term_depth = 12
    let population_size = 500
    let generations = 1000
    let bests = 25
    let mutation_prob = 0.25
    let finish fit = fit = 10.0
    let timeOut = 3000
    let seed = System.DateTime.Now.Millisecond
    printfn "Seed: %d" seed
    let loadFile = None
    let saveFile = "pool.save"
    let data = GP_hol.get_gp_data term_size term_depth population_size generations 
                                  bests mutation_prob finish timeOut seed
                                  loadFile saveFile "" closure
    match GP_hol.gp data with
        | Some (_, i) -> printfn "Solution: %s" (Swensen.Unquote.Operators.decompile i.eta_norm)
                         printfn "Solution: %s" (Swensen.Unquote.Operators.decompile i.norm)
        | None -> printfn "oops"
    0 // return an integer exit code
    