module Experiments

open System
open System.IO

open FSharp.Collections.ParallelSeq

open GP_hol
open Gnuplot
open Utils

let get_gp_data term_size msg prefix scheme i =
    let msg = msg + string i
    printfn "Starting experiment %i" i
    let closure = Utils.closure 10 scheme
    let max_term_size = 45
    let delta_term = 3
    let population_size = 500
    let generations = 500
    let bests = 25
    let mutation_prob = 0.10
    let error fit = 1.0 - fit
    let timeOut = 5000
    let seed = Guid.NewGuid().GetHashCode()
    printfn "Random seed: %i" seed
    let loadFile = None
    let saveFile = prefix + "_pool" + string i + ".save"
    let par = false
    let memoization = false
    let data = GP_hol.get_gp_data memoization par term_size max_term_size delta_term
                                  population_size generations bests mutation_prob error 
                                  timeOut seed loadFile saveFile msg closure
    data

let change_string (str1:string) str2 file =
    let str = File.ReadAllText file
    File.WriteAllText(file, str.Replace(str1, str2))

let compare_lambda_terms name destructor_style_scheme constructor_style_scheme =
    compare_lambda_terms_plot true (name + string 500)
                              ("Number of typed $\\\\lambda$-terms for " + name) 
                              40
                              constructor_style_scheme
                              destructor_style_scheme
    execute "/usr/local/bin/gnuplot" (name + "500LambdaTerms.plot") |> ignore
    System.Threading.Thread.Sleep(2000)
    change_string (name + "500LambdaTerms") ("gnuplot/" + name + "500LambdaTerms") (name + "500LambdaTerms.tex")

let run_experiment term_size name destructor_style_scheme constructor_style_scheme =
    let dests = [1 .. 20] |> PSeq.map (gp << get_gp_data term_size 
                                                         "Experiment destructor style scheme: " 
                                                         "destructor" 
                                                         destructor_style_scheme)
                          |> PSeq.toList
    let consts = [1 .. 20] |> PSeq.map (gp << get_gp_data term_size
                                                          "Experiment constructor style scheme: " 
                                                          "constructor" constructor_style_scheme)
                           |> PSeq.toList
    let (eqs1, alleq1) = gp_statistics_to_equals 500 dests
    let (eqs2, alleq2) = gp_statistics_to_equals 500 consts
    let equals_dest = sprintf "gp_statistics_to_equals Destructive: (%i, %i)" eqs1 alleq1
    let equals_consts = sprintf "gp_statistics_to_equals Constructive: (%i, %i)" eqs2 alleq2
    gp_statistics_to_error_plot (name + "Dest" + string 500) ("(b) Best-of-generation error for "+name+" (destructive)") 500 dests
    gp_statistics_to_error_plot (name + "Consts" + string 500) ("(b) Best-of-generation error for "+name+" (constructive)") 500 consts
    gp_statistics_to_cumulative_prob_plot (name + string 500) ("(a) Cumulative Probability of Success for "+name) 500 consts dests
    // Latex code generation
    execute "/usr/local/bin/gnuplot" (name + "Dest500Error.plot") |> ignore
    execute "/usr/local/bin/gnuplot" (name + "Consts500Error.plot") |> ignore
    execute "/usr/local/bin/gnuplot" (name + "500Cumulative.plot") |> ignore
    System.Threading.Thread.Sleep(2000)
    change_string (name + "Dest500Error") ("gnuplot/" + name + "Dest500Error") (name + "Dest500Error.tex")
    change_string (name + "Consts500Error") ("gnuplot/" + name + "Consts500Error") (name + "Consts500Error.tex")
    change_string (name + "500Cumulative") ("gnuplot/" + name + "500Cumulative") (name + "500Cumulative.tex")

    let str_dest = dests
                   |> gp_solutions
                   |> List.sortBy (fun (str, i) -> str.Length :: List.map size i.genome)
                   |> List.map (str_of_individual << snd)
    let str_const = consts
                    |> gp_solutions
                    |> List.sortBy (fun (str, i) -> str.Length :: List.map size i.genome)
                    |> List.map (str_of_individual << snd)
    let lines = ["Equals dest: "
                 equals_dest
                 ""
                 "Equals consts: "
                 equals_consts
                 ""
                 "Solutions dest"
                ] @ str_dest @
                [""
                 "Solutions consts"]
                 @ str_const
    File.WriteAllLines("Result.txt", lines)
