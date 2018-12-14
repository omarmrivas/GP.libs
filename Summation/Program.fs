// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Utils
open Experiments

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

let fitness f = fitness_normal_1 float f in_out

// Constructor-style sum scheme
let constructor_style_sum_scheme =
    <@@ fun N ->
         let rec f x = match x with
                        [] -> 0
                      | x :: xs -> N x xs ((+) : int->int->int) f
         fun () -> fitness f @@>

// Destructor-style sum scheme
let destructor_style_sum_scheme =
    <@@ fun N ->
         let rec f xs = N xs (fun x y (z : int list) -> if x then y else z) 
                          (fun x (y : int list) -> x = y) ([] : int list)
                          ((+) : int->int->int) 0 
                          (fun (xs : int list) -> if List.isEmpty xs
                                                  then 0
                                                  else List.head xs)
                          (fun (xs : int list) -> if List.isEmpty xs
                                                  then []
                                                  else List.tail xs) f
         fun () -> fitness f @@>

(*let run_experiment msg scheme i =
    printfn "Starting experiment %i" i
    let closure = Utils.closure 10 scheme
    let term_size = 17
    let term_depth = 18
    let max_term_size = 25
    let population_size = 500
    let generations = 1000
    let bests = 25
    let mutation_prob = 0.10
    let error fit = 1.0 - fit
    let timeOut = 3000
    let seed = Guid.NewGuid().GetHashCode()
    printfn "Random seed: %i" seed
    let loadFile = None
    let saveFile = "pool.save"
    let par = false
    let data = GP_hol.get_gp_data par term_size max_term_size term_depth population_size 
                                  generations bests mutation_prob error timeOut seed
                                  loadFile saveFile msg closure
    GP_hol.gp data*)

[<EntryPoint>]
let main argv =
//    compare_lambda_terms "Sum" destructor_style_sum_scheme constructor_style_sum_scheme

    run_experiment 23 "Sum" destructor_style_sum_scheme constructor_style_sum_scheme

    (*compare_lambda_terms_plot ("Sum" + string 500) "Number of typed $\\\\lambda$-terms for Sum" 40 constructor_style_sum_scheme destructor_style_sum_scheme
    execute "/usr/local/bin/gnuplot" "Sum500LambdaTerms.plot" |> ignore*)

    (*let dests = [1 .. 20] |> List.map (fun i -> run_experiment ("Experiment destructor_style_sum_scheme: " + string i) destructor_style_sum_scheme i)
    let consts = [1 .. 20] |> List.map (fun i -> run_experiment ("Experiment constructor_style_sum_scheme: " + string i) constructor_style_sum_scheme i)
    let (eqs1, alleq1) = gp_statistics_to_equals 500 dests
    let (eqs2, alleq2) = gp_statistics_to_equals 500 consts
    printfn "gp_statistics_to_equals Destructive: (%i, %i)" eqs1 alleq1
    printfn "gp_statistics_to_equals Constructive: (%i, %i)" eqs2 alleq2
    gp_statistics_to_error_plot ("SumDest" + string 500) "(b) Best-of-generation error for Sum (destructive)" 500 dests
    gp_statistics_to_error_plot ("SumConsts" + string 500) "(b) Best-of-generation error for Sum (constructive)" 500 consts
    gp_statistics_to_cumulative_prob_plot ("Sum" + string 500) "(a) Cumulative Probability of Success for Sum" 500 consts dests
    // Latex code generation
    execute "/usr/local/bin/gnuplot" "SumDest500Error.plot" |> ignore
    execute "/usr/local/bin/gnuplot" "SumConsts500Error.plot" |> ignore
    execute "/usr/local/bin/gnuplot" "Sum500Cumulative.plot" |> ignore*)
    0 // return an integer exit code
    