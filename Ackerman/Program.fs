// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open Utils
open GP_hol
open Gnuplot
open FSharp.Collections.ParallelSeq

open Utils
open MBrace.FsPickler
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns



type nat = Zero | Suc of nat

let rec to_nat i =
    if i > 0
    then to_nat (i-1) |> Suc
    else Zero

let rec from_nat = function
    | Zero -> 0
    | Suc x -> 1 + from_nat x

let in_out = [(0, 0, 1)
              (1, 0, 2)
              (0, 1, 2)
              (1, 1, 3)
              (1, 2, 4)
              (2, 1, 5)
              (2, 2, 7)
              (2, 0, 3)
              (0, 2, 3)]
              |> List.map (fun (x,y,z) -> 
                            (to_nat x,to_nat y,to_nat z))

let fitness f = fitness_normal_2 (float << from_nat) f in_out

// Constructor-style sum scheme
let constructor_style_ackerman_scheme =
    <@@ fun M N ->
         let rec f x y = match x with
                         Zero -> Suc y
                       | Suc x -> match y with
                                  Zero -> M x Zero Suc (f x)
                                | Suc y -> N x y Zero Suc (f x) (f (Suc x))
         fun () -> fitness f @@>

// destructor of natural numbers
let dec = function
    | Zero -> Zero
    | Suc x -> x

// Destructor-style sum scheme
let destructor_style_ackerman_scheme =
    <@@ fun M ->
         let rec f x y : nat = M (x : nat) (y : nat) Zero Suc dec
                                 (fun x (y : nat) -> x = y)
                                 (fun x y (z : nat) -> if x then y else z)
                                 (f (dec x)) (f x)
         fun () -> fitness f @@>

let run_experiment msg scheme i =
    printfn "Starting experiment %i" i
    let closure = Utils.closure 10 scheme
    let term_size = 21
    let max_term_size = 25
    let term_depth = 21
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
    let memoization = false
    let data = GP_hol.get_gp_data memoization par term_size max_term_size term_depth 
                                  population_size generations bests mutation_prob error 
                                  timeOut seed loadFile saveFile msg closure
    GP_hol.gp data

(*let algo =
    let rec f = fun af x y ->
        if af <= 0 
        then Zero
        else (match x with 
              | Suc(x) -> 
                  (match y with 
                   | Suc(y) ->
                       f (af - 1) x (f (af - 1) (Suc x) y)
                   | _ -> Suc(Suc( x )))
              | _ ->  Suc(y))
    in fun () -> fitness (f 7)*)

[<EntryPoint>]
let main argv =
(*    compare_lambda_terms_plot ("Ackerman" + string 500) "Number of typed $\\\\lambda$-terms for Ackerman" 40 constructor_style_ackerman_scheme destructor_style_ackerman_scheme
    execute "/usr/local/bin/gnuplot" "Ackerman500LambdaTerms.plot" |> ignore*)

    let scheme = destructor_style_ackerman_scheme
                 |> Utils.closure 10
    printfn "Fuck"
    let M = <@@ fun (x:nat) (xa:nat) (xb:nat) xc xd (xf:nat->nat->bool) xe xg (xh:nat->nat) -> xc (xe (xf xb x) xa (xg (xe (xf xb (xe (xf xb (xc xa)) xa xa)) xa (xe (xf (xe (xf x xa) xb x) (xd xa)) xa (xc xa))))) @@>
//    let N = <@@ fun (a:nat) (b:nat) (c:nat) (d:nat->nat) (e:nat->nat) (f:nat->nat) -> e (f b) @@>

    printfn "Depth: %i" (Utils.depth M)
    printfn "Size: %i" (Utils.size M)

    let scheme' = Expr.Applications(scheme, [[M]])
                    |> expand Map.empty

    printfn "%A" (Swensen.Unquote.Operators.decompile scheme')

    let f : unit -> float = (Swensen.Unquote.Operators.evalRaw scheme')

    // fun a b c d -> c (c a)
    // fun a b c d e f -> e (f b)

    printfn "f(): %A" (f ())


    (*let dests = [1 .. 20] |> PSeq.map (fun i -> run_experiment ("Experiment destructor_style_ackerman_scheme: " + string i) destructor_style_ackerman_scheme i)
                          |> PSeq.toList
    let consts = [1 .. 20] |> List.map (fun i -> run_experiment ("Experiment constructor_style_ackerman_scheme: " + string i) constructor_style_ackerman_scheme i)
                           |> PSeq.toList
    let (eqs1, alleq1) = gp_statistics_to_equals 500 dests
    let (eqs2, alleq2) = gp_statistics_to_equals 500 consts
    printfn "gp_statistics_to_equals Destructive: (%i, %i)" eqs1 alleq1
    printfn "gp_statistics_to_equals Constructive: (%i, %i)" eqs2 alleq2
    gp_statistics_to_error_plot ("AckermanDest" + string 500) "(f) Best-of-generation error for Ackerman (destructive)" 500 dests
    gp_statistics_to_error_plot ("AckermanConsts" + string 500) "(e) Best-of-generation error for Ackerman (constructive)" 500 consts
    gp_statistics_to_cumulative_prob_plot ("Ackerman" + string 500) "(c) Cumulative Probability of Success for Ackerman" 500 consts dests
    // Latex code generation
    execute "/usr/local/bin/gnuplot" "AckermanDest500Error.plot" |> ignore
    execute "/usr/local/bin/gnuplot" "AckermanConsts500Error.plot" |> ignore
    execute "/usr/local/bin/gnuplot" "Ackerman500Cumulative.plot" |> ignore*)
    0 // return an integer exit code
    