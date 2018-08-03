module Gnuplot

open System.IO
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open GP_hol

let execute prog args =
    let proc = new System.Diagnostics.Process()
    let startInfo = new System.Diagnostics.ProcessStartInfo()
    startInfo.WindowStyle <- System.Diagnostics.ProcessWindowStyle.Hidden
    startInfo.FileName <- prog
    startInfo.Arguments <- args
    proc.StartInfo <- startInfo
    proc.Start()

let dest_result = function
    | Solved (sts, _) -> sts
    | Unsolved sts -> sts

let max_error (sts : gp_result list) =
    sts |> List.collect ((fun data -> data.statistics) << dest_result)
        |> List.maxBy (fun sts -> sts.average_error)
        |> (fun sts -> sts.average_error)

let max_generation (sts : gp_result list) =
    sts |> List.maxBy (List.length << (fun data -> data.statistics) << dest_result)
        |> (List.length << (fun data -> data.statistics) << dest_result)

let extract normalize = function
    | Solved (data, ind) -> match data.statistics with
                            | [] -> normalize (data.error ind.fitness)
                            | st :: _ -> normalize (data.error st.best_individual.fitness)
    | Unsolved data -> match data.statistics with
                       | [] -> failwith "Can not get statistics"
                       | st :: _ -> normalize (data.error st.best_individual.fitness)
let reduce = function
    | Solved (data, ind) -> match data.statistics with
                            | [] -> Solved (data, ind)
                            | _ :: sts -> Solved ({data with statistics = sts}, ind)
    | Unsolved data -> match data.statistics with
                       | [] -> failwith "Can not reduce statistics"
                       | _ :: sts -> Unsolved {data with statistics = sts}

let gp_statistics_to_error_plot file title generations (sts : gp_result list) =
    let merror = max_error sts
    let mgeneration = max_generation sts
    let normalize x = x / merror
    let datafile = file + "Error.data"
    let plotfile = file + "Error.plot"
    let experiments = List.length sts
    let header =["set terminal epslatex color"
                 "set output \"" + file + "Error.tex\""
                 "set format xy \"$%g$\""
                 "set title \"" + title + "\""
                 "set xrange [0.0:" + string mgeneration + "]"
                 "set yrange [0.0:1.0]"
                 "set xlabel \"Generation\""
                 "set ylabel \"Minimum error\""]
    let body = [3 .. experiments + 2]
                |> List.map (fun i -> "     \"" + datafile + "\" using 1:" + string i +
                                      " title \"\" with lines lt rgb \"#7F7F7F\", \\")
                |> (fun strs -> ("plot \"" + datafile + "\" using 1:2" +
                                 " title \"\" with lines lw 3 lt rgb \"#000000\", \\") :: strs)
    File.WriteAllLines(plotfile, header @ body)
    [0 .. generations - 1]
    |> List.fold (fun (R,sts) i -> let row = List.map (extract normalize) sts
                                   let avg = List.average row
                                   ((float i :: avg :: row) :: R, List.map reduce sts)) ([],sts)
    |> (List.rev << fst)
    |> List.map ((fun str -> str) << String.concat " " << List.map string)
    |> (fun lines -> File.WriteAllLines(datafile, lines))

let gp_statistics_to_cumulative_prob generations (sts : gp_result list) =
    let trials = float <| List.length sts
    [0 .. generations - 1]
           |> List.fold (fun (R, sts) _ ->
                let row = List.map (extract id) sts
                let cum = row |> List.fold (fun cum n -> if 0.0 = n
                                                         then cum + 1
                                                         else cum) 0
                              |> float
                              |> (fun x -> x / trials)
                (cum :: R, List.map reduce sts)) ([], sts)
           |> (List.rev << fst)

let gp_statistics_to_cumulative_prob_plot file title generations (sts1 : gp_result list) (sts2 : gp_result list) =
    let datafile = file + "Cumulative.data"
    let plotfile = file + "Cumulative.plot"
    let cums1 = gp_statistics_to_cumulative_prob generations sts1
    let cums2 = gp_statistics_to_cumulative_prob generations sts2
    let header =["set terminal epslatex color"
                 "set output \"" + file + "Cumulative.tex\""
                 "set format xy \"$%g$\""
                 "set title \"" + title + "\""
                 "set xrange [0.0:" + string generations + "]"
                 "set yrange [0.0:1.0]"
                 "set xlabel \"Generation\""
                 "set ylabel \"CPS\""]
    let body = ["plot \"" + datafile + "\" using 1:2" +
                " title \"Constructive\" with lines lw 3 lt rgb \"#0000FF\", \\";
                "     \"" + datafile + "\" using 1:3" +
                " title \"Destructive\" with lines lw 3 lt rgb \"#FF0000\""]
    File.WriteAllLines(plotfile, header @ body)
    [0 .. generations - 1]
    |> List.fold (fun (R, sts1, sts2) i ->
                    let row = [List.head sts1; List.head sts2]
                    ((float i :: row) :: R, List.tail sts1, List.tail sts2)) ([], cums1, cums2)
    |> (List.rev << (fun (x, _, _) -> x))
    |> List.map ((fun str -> str) << String.concat " " << List.map string)
    |> (fun lines -> File.WriteAllLines (datafile, lines))

let compare_lambda_terms_plot file title size (scheme1 : Expr) (scheme2 : Expr) =
    let datafile = file + "LambdaTerms.data"
    let plotfile = file + "LambdaTerms.plot"
    let cums1 = num_lambda_terms size scheme1
    let cums2 = num_lambda_terms size scheme2
    let header =["set terminal epslatex color"
                 "set output \"" + file + "LambdaTerms.tex\""
                 "set format y \"$10^{%T}$\""
                 "set title \"" + title + "\""
                 "set xrange [1.0:" + string size + "]"
                 "set logscale y"
                 "set xlabel \"Size\""
                 "set ylabel \"$\\\\lambda$-terms\""]
    let body = ["plot \"" + datafile + "\" using 1:2" +
                " title \"Constructive Style\" with lines lw 3 lt rgb \"#0000FF\", \\";
                "     \"" + datafile + "\" using 1:3" +
                " title \"Destructive Style\" with lines lw 3 lt rgb \"#FF0000\""]
    File.WriteAllLines(plotfile, header @ body)
//    [0 .. generations - 1]
    List.fold2 (fun R (i, v1) (_, v2) ->
                    let row = [v1; v2]
                    (((bigint : int -> bigint) i :: row) :: R)) [] cums1 cums2
    |> List.rev
    |> List.map ((fun str -> str) << String.concat " " << List.map string)
    |> (fun lines -> File.WriteAllLines (datafile, lines))


let gp_result_to_equals = function
    | Solved (data, _) -> List.sumBy (fun st -> st.equals) data.statistics
    | Unsolved data -> List.sumBy (fun st -> st.equals) data.statistics

let gp_statistics_to_equals population_size (sts : gp_result list) =
    let processed = function
        | Solved (data, _) -> population_size * List.length data.statistics
        | Unsolved data -> population_size * List.length data.statistics
    let eqs = List.sumBy gp_result_to_equals sts
    let all = List.sumBy processed sts
    (eqs, all)
