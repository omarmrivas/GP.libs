// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open Utils
open GP_hol
open Gnuplot
open Experiments

type nat = Zero | Suc of nat

let rec to_nat i =
    if i > 0
    then to_nat (i-1) |> Suc
    else Zero

let rec from_nat = function
    | Zero -> 0
    | Suc x -> 1 + from_nat x

let in_out = [5, 3, 15
              1, 1, 1
              2, 2, 4
              2, 3, 6
              3, 2, 6
              3, 3, 9
              2, 4, 8
              4, 2, 8
              3, 4, 12
              4, 4, 16]
              |> List.map (fun (x,y,z) -> (to_nat x, to_nat y, to_nat z))

let fitness f = fitness_normal_2 (float << from_nat) f in_out

// Constructor-style sum scheme
let constructor_style_mul_scheme =
    <@@ fun M N ->
         let rec f x y = match x with
                         Zero -> (y : nat)
                       | Suc x -> M x y Zero Suc (f x)
             and g x y = match x with
                         Zero -> Zero
                       | Suc x -> N x y Zero Suc (f y) (g x)
         fun () -> fitness g @@>

// destructor of natural numbers
let dec = function
    | Zero -> Zero
    | Suc x -> x

// Destructor-style sum scheme
let destructor_style_mul_scheme =
    <@@ fun M N ->
         let rec f x y : nat = M (x : nat) (y : nat) Zero Suc dec
                                 (fun x (y : nat) -> x = y)
                                 (fun x y (z : nat) -> if x then y else z) (f (dec x))
             and g x y : nat = N (x : nat) (y : nat) Zero Suc dec
                                 (fun x (y : nat) -> x = y)
                                 (fun x y (z : nat) -> if x then y else z) (f y) (g (dec x))
         fun () -> fitness g @@>

(*let run_experiment msg scheme i =
    printfn "Starting experiment %i" i
    let closure = Utils.closure 6 scheme
    let term_size = 18
    let max_term_size = 25
    let term_depth = 18
    let population_size = 500
    let generations = 1000
    let bests = 25
    let mutation_prob = 0.75
    let error fit = 1.0 - fit
    let timeOut = 5000
    let seed = Guid.NewGuid().GetHashCode()
    printfn "Random seed: %i" seed
    let loadFile = None
    let saveFile = "pool.save"
    let data = GP_hol.get_gp_data term_size max_term_size term_depth population_size generations
                                  bests mutation_prob error timeOut seed
                                  loadFile saveFile msg closure
    GP_hol.gp data*)

[<EntryPoint>]
let main argv =
    //compare_lambda_terms "Mul" destructor_style_mul_scheme constructor_style_mul_scheme

    run_experiment 21 "Mul" destructor_style_mul_scheme constructor_style_mul_scheme

    (*let dests = [1 .. 20] |> List.map (GP_hol.gp << Experiments.get_gp_data "Experiment destructor_style_mul_scheme: " "destructor" destructor_style_mul_scheme)
    let consts = [1 .. 20] |> List.map (GP_hol.gp << Experiments.get_gp_data "Experiment constructor_style_mul_scheme: " "constructor" constructor_style_mul_scheme)
    let (eqs1, alleq1) = gp_statistics_to_equals 500 dests
    let (eqs2, alleq2) = gp_statistics_to_equals 500 consts
    printfn "gp_statistics_to_equals Destructive: (%i, %i)" eqs1 alleq1
    printfn "gp_statistics_to_equals Constructive: (%i, %i)" eqs2 alleq2
    gp_statistics_to_error_plot ("MulDest" + string 500) "(b) Best-of-generation error for Mul (destructive)" 500 dests
    gp_statistics_to_error_plot ("MulConsts" + string 500) "(b) Best-of-generation error for Mul (constructive)" 500 consts
    gp_statistics_to_cumulative_prob_plot ("Mul" + string 500) "(a) Cumulative Probability of Success for Mul" 500 consts dests
    // Latex code generation
    execute "/usr/local/bin/gnuplot" "MulDest500Error.plot" |> ignore
    execute "/usr/local/bin/gnuplot" "MulConsts500Error.plot" |> ignore
    execute "/usr/local/bin/gnuplot" "Mul500Cumulative.plot" |> ignore*)
    0 // return an integer exit code
        