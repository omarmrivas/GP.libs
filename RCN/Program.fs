// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

open System.IO
open MathNet.Numerics

open DataTypes
open Approximation
open Graph
open Algebra
open GP_hol

let lower_bound =
    [4, 0
     5, 1
     6, 3
     7, 9
     8, 19
     9, 36
     10, 62
     11, 102
     12, 153
     13, 229
     14, 324
     15, 447
     16, 603
     17, 798
     18, 1029
     19, 1318
     20, 1657
     21, 2055
     22, 2528
     23, 3077
     24, 3699
     25, 4430
     26, 5250
     27, 6180
     28, 7233
     29, 8421
     30, 9726]

let fitness i f =
    try let (n, cn) = List.item i lower_bound
        match f n with
            Some g -> let cn' = g.crossing_number
                      let wc = List.length g.quadrilaterals
                      let x = double (wc - cn')
                      x * x
          | None -> 0.5
    with | :? System.AggregateException -> 0.0
         | :? System.OverflowException -> 0.0
         | genericException -> 0.0

let best_fitness i =
    let (n, cn) = List.item i lower_bound
    let wc = binomialCoefficient n 4
    let x = double (wc - cn)
    x * x

// Original scheme
let scheme k i =
    <@@ fun Mx My ->
         let rec x n : base_type =
                       if n <= 0
                       then one
                       else Mx (n-1) to_base zero one two three (pi k) (e k) iter test_zero
                               add sub div mul neg (sin k) (cos k) (sinh k) (cosh k) (tan k) (tanh k) (ln k) (exp k) (msqrt k) x y
         and y n : base_type =
                       if n <= 0
                       then -one
                       else My (n-1) to_base zero one two three (pi k) (e k) iter test_zero
                               add sub div mul neg (sin k) (cos k) (sinh k) (cosh k) (tan k) (tanh k) (ln k) (exp k) (msqrt k) x y
         let graph n = 
                [1 .. n]
                    |> List.map (fun i -> (x i, y i) : Vertex)
                    |> List.fold add_vertex (Some empty_graph)
         fun () -> fitness i graph @@>

[<EntryPoint>]
let main argv =
//    List.iter (fun i -> printfn "%f" (BigRational.ToDouble (Approximation.ln i (BigRational.FromInt(5))))) [1..10]
    let i = if Array.isEmpty argv
            then 0
            else let i = int argv.[0]
                 printfn "Starting with graph size = %i" i
                 i-4
    let closure = Utils.closure 6 (scheme 4 0)
    let term_size = 12
    let term_depth = 12
    let population_size = 200
    let generations = 10000
    let bests = 10
    let mutation_prob = 0.4
    let finish fit = best_fitness 0 = fit
    let timeOut = 120000 + 0 * 20000 // 2 minute + 20 seconds per n (size of graph)
    let seed = System.DateTime.Now.Millisecond
    printfn "Seed: %d" seed
    let loadFile = None
    let saveFile = "pool" + string 4 + ".save"
    let message = sprintf "Graph (size = %d)" 4
    let data = GP_hol.get_gp_data term_size term_depth population_size generations 
                                  bests mutation_prob finish timeOut seed
                                  loadFile saveFile message closure
    [i .. List.length lower_bound - 1]
        |> List.fold (fun data i ->
                        printfn "Using GP to obtain Graph (size = %d) with minimal RCN." (i+4)
                        let closure = Utils.closure (6+i) (scheme (4+i) i)
                        let finish fit = best_fitness i = fit
                        let timeOut = 1200000 + i * 200000 // 20 minute + 3.3 minutes per n (size of graph)
                        let (loadFile, saveFile) =
                                if File.Exists( "pool" + string (i+4) + ".save" )
                                then (Some ("pool" + string (i+4) + ".save"), 
                                      "pool" + string (i+4) + ".save")
                                else if i = 0
                                     then (None, "pool" + string (i+4) + ".save")
                                     else (Some ("pool" + string (i+3) + ".save"),
                                           "pool" + string (i+4) + ".save")
                        let message = sprintf "Graph (size = %d)" (i+4)
                        let data = {data with scheme = closure
                                              finish = finish
                                              timeout = timeOut
                                              load_file = loadFile
                                              save_file = saveFile
                                              message = message}
                        match GP_hol.gp data with
                            | Some (data, i) -> 
                                        printfn "************************"
                                        printfn "Solution: %s" (Swensen.Unquote.Operators.decompile i.norm)
                                        printfn "for: %s" message
                                        printfn "************************"
                                        data
                            | None -> failwith "oops") data
        |> ignore
    0 // return an integer exit code
        