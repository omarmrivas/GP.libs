// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open Utils
open GP_hol
open Gnuplot
open FSharp.Collections.ParallelSeq

type nat = Zero | Suc of nat

let rec to_nat i =
    if i > 0
    then to_nat (i-1) |> Suc
    else Zero

let rec from_bool = function
    | true -> 1.0
    | false -> 0.0

let in_out = [(0, 0, true)
              (1, 0, false)
              (0, 1, false)
              (1, 1, true)
              (0, 2, true)
              (1, 2, false)
              (0, 3, false)
              (1, 3, true)
              (0, 4, true)
              (1, 4, false)]
              |> List.map (fun (x,y,z) -> (x,to_nat y,z))

let fitness fs =
    let f x y = (List.item x fs) y
    fitness_normal_2 from_bool f in_out

// Constructor-style sum scheme
let constructor_style_evenodd_scheme =
    <@@ fun M N ->
         let rec f x = match x with
                       Zero -> true
                     | Suc x -> M x Zero Suc g
             and g x = match x with
                       Zero -> false
                     | Suc x -> N x Zero Suc f
         fun () -> fitness [f;g] @@>

// destructor of natural numbers
let dec = function
    | Zero -> Zero
    | Suc x -> x

// Destructor-style sum scheme
let destructor_style_evenodd_scheme =
    <@@ fun M N ->
         let rec f x : bool = M (x : nat) Zero Suc dec
                                (fun x (y : nat) -> x = y)
                                (fun x y (z : bool) -> if x then y else z) g
             and g x : bool = N (x : nat) Zero Suc dec
                                (fun x (y : nat) -> x = y)
                                (fun x y (z : bool) -> if x then y else z) f
         fun () -> fitness [f;g] @@>

let run_experiment msg scheme i =
    printfn "Starting experiment %i" i
    let closure = Utils.closure 6 scheme
    let term_size = 18
    let max_term_size = 25
    let term_depth = 18
    let population_size = 500
    let generations = 500
    let bests = 25
    let mutation_prob = 0.10
    let error fit = 1.0 - fit
    let timeOut = 5000
    let seed = Guid.NewGuid().GetHashCode()
    printfn "Random seed: %i" seed
    let loadFile = None
    let saveFile = "pool.save"
    let par = false
    let data = GP_hol.get_gp_data par term_size max_term_size term_depth population_size
                                  generations bests mutation_prob error timeOut seed
                                  loadFile saveFile msg closure
    GP_hol.gp data

[<EntryPoint>]
let main argv =
(*    compare_lambda_terms_plot ("EvenOdd" + string 500) "Number of typed $\\\\lambda$-terms for EvenOdd" 40 constructor_style_evenodd_scheme destructor_style_evenodd_scheme
    execute "/usr/local/bin/gnuplot" "EvenOdd500LambdaTerms.plot" |> ignore*)

    let dests = [1 .. 20] |> PSeq.map (fun i -> i, run_experiment ("Experiment destructor_style_evenodd_scheme: " + string i) destructor_style_evenodd_scheme i)
                          |> PSeq.toList
                          |> List.sortBy fst
                          |> List.map snd
    let consts = [1 .. 20] |> PSeq.map (fun i -> i, run_experiment ("Experiment constructor_style_evenodd_scheme: " + string i) constructor_style_evenodd_scheme i)
                           |> PSeq.toList
                           |> List.sortBy fst
                           |> List.map snd
    let (eqs1, alleq1) = gp_statistics_to_equals 500 dests
    let (eqs2, alleq2) = gp_statistics_to_equals 500 consts
    printfn "gp_statistics_to_equals Destructive: (%i, %i)" eqs1 alleq1
    printfn "gp_statistics_to_equals Constructive: (%i, %i)" eqs2 alleq2
    gp_statistics_to_error_plot ("EvenOddDest" + string 500) "(b) Best-of-generation error for EvenOdd (destructive)" 500 dests
    gp_statistics_to_error_plot ("EvenOddConsts" + string 500) "(b) Best-of-generation error for EvenOdd (constructive)" 500 consts
    gp_statistics_to_cumulative_prob_plot ("EvenOdd" + string 500) "(a) Cumulative Probability of Success for EvenOdd" 500 consts dests
    // Latex code generation
    execute "/usr/local/bin/gnuplot" "EvenOddDest500Error.plot" |> ignore
    execute "/usr/local/bin/gnuplot" "EvenOddConsts500Error.plot" |> ignore
    execute "/usr/local/bin/gnuplot" "EvenOdd500Cumulative.plot" |> ignore
    0 // return an integer exit code
