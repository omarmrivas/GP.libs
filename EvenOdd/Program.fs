﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Utils

type nat = Cero | Suc of nat

let in_out = [(0, Cero, true)
              (1, Cero, false)
              (0, Suc Cero, false)
              (1, Suc Cero, true)
              (0, Suc (Suc Cero), true)
              (1, Suc (Suc Cero), false)
              (0, Suc (Suc (Suc Cero)), false)
              (1, Suc (Suc (Suc Cero)), true)
              (0, Suc (Suc (Suc (Suc Cero))), true)
              (1, Suc (Suc (Suc (Suc Cero))), false)]

let fitness fs = 
        List.sumBy (fun (i, input, output) -> 
                            if Array.item i fs input = output
                            then 1.0
                            else 0.0) in_out

// Original scheme
let scheme =
    <@@ fun M N ->
         let rec f x = match x with
                        Cero -> true
                      | Suc x -> M x Cero Suc g
             and g x = match x with
                        Cero -> false
                      | Suc x -> N x Cero Suc f
         fun () -> fitness [| f; g |] @@>

[<EntryPoint>]
let main argv =
    printfn "Starting..."
    let closure = Utils.closure 20 scheme
    let term_size = 10
    let term_depth = 12
    let population_size = 100
    let generations = 1000
    let bests = 5
    let mutation_prob = 0.25
    let finish fit = fit = 10.0
    let timeOut = 3000
    let seed = System.DateTime.Now.Millisecond
    printfn "Seed: %d" seed
    let loadFile = None
    let saveFile = "pool.save"
    let serializationFile = "pool.user"
    let data = GP_hol.get_gp_data term_size term_depth population_size generations 
                                  bests mutation_prob finish timeOut seed
                                  loadFile saveFile "" closure
    match GP_hol.gp data with
        | Some (_, i) -> printfn "Solution: %A" i
        | None -> printfn "oops"
    0 // return an integer exit code
    