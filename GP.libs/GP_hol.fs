module GP_hol

open System
open System.IO
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open MBrace.FsPickler
open Utils
open Serialize

type par_data = Random * (Type * (Type*bigint) list * int -> bigint)

type proto_individual =
    {
        genome: Expr list
        eta_norm : Expr
        norm: Expr
        fitness: unit -> float
    }

[<Serializable>]
type individual =
    {
        genome: Expr list
        eta_norm : Expr
        norm: Expr
        fitness: float
    }

[<Serializable>]
type gp_statistic =
    {
        best_individual : individual
        average_size : float list
        average_depth : float list
        average_fitness : float
        average_error : float
//        non_terminating : int
        equals : int
    }

type gp_data =
    {
        scheme : Expr
        vars : Var list
        term_size: int
        max_term_size : int
        delta_size: int
        term_sizes: int list
        population_size: int
        generations : int
        bests: int
        mutation_prob: float
        error: float -> float
        term_count: (int * bigint) [] list
        timeout : int
        par_data : par_data []
        par : bool
        load_file : string option
        save_file : string
        message: string
        statistics : gp_statistic list
        memoization : bool
        T : Map<string, individual>
    }

type gp_result =
    | Solved of gp_data * individual
    | Unsolved of gp_data

let gp_data_to_string data =
    sprintf "scheme: %s\n
             term_size: %d\n
             population_size: %d\n
             generations: %d\n
             bests: %d\n
             mutation_prob: %.3f\n
             timeout: %.1f sec\n
             load_file: %A\n
             save_file: %s\n"
                (Swensen.Unquote.Operators.decompile data.scheme)
                data.term_size
                data.population_size
                data.generations
                data.bests
                data.mutation_prob
                ((float data.timeout) / 1000.0)
                data.load_file
                data.save_file

let memoize fn =
//  let cache = new System.Collections.Concurrent.ConcurrentDictionary<_,_>()
  let cache = new System.Collections.Generic.Dictionary<_,_>()
  (fun x ->
    match cache.TryGetValue x with
    | true, v -> v
    | false, _ -> let v = fn (x)
                  cache.[x] <- v
                  v)

let statistics (data : gp_data) pool =
    let eq_detected = Array.sumBy (function (true, _) -> 1
                                          | (false, _) -> 0) pool
    let undetected = Array.filter (not << fst) pool
    let eq_undetected = Array.length undetected - 
                        Array.length (Array.groupBy (fun (_, i) -> Swensen.Unquote.Operators.decompile i.eta_norm) undetected)
    let best = snd <| Array.minBy (fun (_, i) -> (-i.fitness, List.map size i.genome)) pool
    let get_sizes best = List.mapi2(fun i e d -> 
                                        let d' = size e
                                        if d' + data.delta_size >= d && d' < data.max_term_size
                                        then printfn "Increasing max size for Chromosome %d from %d to %d" i d (d' + 1)
                                             d' + data.delta_size + 1
                                        else d) best.genome data.term_sizes
    let sizes = pool |> Array.sortBy (fun (_, i) -> (-i.fitness, List.map size i.genome))
                     |> Array.take data.bests
                     |> Array.filter (fun (_, i) -> i.fitness >= 0.90 * best.fitness)
                     |> Array.map (get_sizes << snd)
                     |> Array.fold (List.map2 max) data.term_sizes

    let statistic = 
        {
            best_individual = best
            average_fitness = Array.averageBy (fun (_, i) -> i.fitness) pool
            average_size = List.mapi (fun indx _ -> Array.averageBy (fun (_, i) -> (float << size) i.genome.[indx]) pool) 
                                     data.term_sizes
            average_depth = List.mapi (fun indx _ -> Array.averageBy (fun (_, i) -> (float << depth) i.genome.[indx]) pool) 
                                      data.term_sizes
            average_error = Array.averageBy (fun (_, i) -> data.error i.fitness) pool
            equals = eq_detected + eq_undetected
        }
    {data with term_sizes = sizes
               statistics = statistic :: data.statistics
               T = if data.memoization
                   then Array.fold (fun T (_, i) -> Map.add (Swensen.Unquote.Operators.decompile i.eta_norm) i T) data.T pool
                   else data.T},
    Array.map snd pool

let get_gp_data memoization par term_size max_term_size delta_size population_size generations bests mutation_prob error timeOut seed loadFile saveFile message scheme =
    let count_term = RandomTerms.count_term ()
    let count_terms A s = count_term (A, [], s)
    let tcount (var:Var) = 
            let args = (List.length << fst << strip_type) var.Type
            [| 1 .. args + term_size |]
                |> Array.map (fun i -> (i, count_terms var.Type i))
                |> Array.filter (fun (_, c) -> c > bigint.Zero)
                |> (fun L -> printfn "Var: %A, counts: %A" var.Name L
                             L)
    let dcount (var:Var) = term_size + (List.length << fst << strip_type) var.Type
    let rnd = Random(seed)
    let par_data = if par
                   then [| 1 .. population_size |]
//                        |> Array.map (fun i -> (Random(i + seed), RandomTerms.count_term))
                        |> Array.map (fun i -> (Random(rnd.Next() + i), memoize (fun (A,env,s) -> RandomTerms.count_term' A env s)))
                   else [| 1 .. population_size |]
                        |> Array.map (fun i -> (rnd, count_term))
    let vars = lambdas scheme
    {scheme = scheme
     vars = vars
     term_size = term_size
     max_term_size = max_term_size
     delta_size = delta_size
     term_sizes = List.map dcount vars
     generations = generations
     population_size = population_size
     bests = bests
     mutation_prob = mutation_prob
     error = error
     term_count = List.map tcount vars
     timeout = timeOut
     par_data = par_data
     par = par
     load_file = loadFile
     save_file = saveFile
     message = message
     statistics = []
     memoization = memoization
     T = Map.empty}

let mk_proto_individual (data : gp_data) lams : proto_individual =
    let eta_norm = Expr.Applications(data.scheme, List.map List.singleton lams)
                    |> expand Map.empty
    let norm = eta_norm
                |> Swensen.Unquote.Operators.reduceFully
                |> List.last
    //printfn "mk_proto_individual: %s" (Swensen.Unquote.Operators.decompile norm)
    {genome = lams
     eta_norm = eta_norm
     norm = norm
//     fitness = Swensen.Unquote.Operators.evalRaw norm}
     fitness = timeout data.timeout (fun () -> 0.0) (fun () -> Swensen.Unquote.Operators.evalRaw norm) ()}

let mk_individual (data : gp_data) (proto : proto_individual) : bool * individual =
    let key = Swensen.Unquote.Operators.decompile proto.eta_norm
    if Map.containsKey key data.T
    then (true, Map.find key data.T)
    else (false, 
          {genome = proto.genome
           // needs beta-eta contraction
           eta_norm = proto.eta_norm
           norm = proto.norm
           fitness = timeout data.timeout 0.0 proto.fitness ()})// |> tap (fun _ -> printfn "Done")

let serialize (data : gp_data) pool =
    let binarySerializer = FsPickler.CreateBinarySerializer()
    use fileStream = new FileStream(data.save_file, FileMode.Create)
    pool |> Array.map (fun i -> ((Array.map serialize_lambda << List.toArray) i.genome, i.fitness))
         |> (fun pool -> binarySerializer.Serialize(fileStream, (data.statistics,pool)))

let deserialize (data : gp_data) =
    match data.load_file with
      | Some load_file -> 
            printfn "Deserializing GP pool"
            let binarySerializer = FsPickler.CreateBinarySerializer()
            use fileStream = new FileStream(load_file, FileMode.Open)
            binarySerializer.Deserialize<(gp_statistic list * (term [] * float)[])>(fileStream)
                |> (fun (statistics, pool) -> 
                        (statistics,Array.map (fun (lams, fitness) -> 
                                let lams = (List.map (deserialize_lambda Map.empty) << Array.toList) lams
                                let eta_norm = Expr.Applications(data.scheme, List.map List.singleton lams)
                                                 |> expand Map.empty
                                let norm = eta_norm |> Swensen.Unquote.Operators.reduceFully
                                                    |> List.last
                                {genome = lams
                                 eta_norm = eta_norm
                                 norm = norm
                                 fitness = fitness}) pool))
      | None -> failwith "Can't deserialize GP pool. Load file not set."

let initial_population (data : gp_data) : gp_data * individual [] =
    if Option.isSome data.load_file &&
       File.Exists( Option.get data.load_file )
    then data |> deserialize
              |> (fun (sts, pool) ->
                    let data = {data with statistics = sts}
                    pool |> Array.map (fun i -> (false, i))
                         |> statistics data)
    else
     data.par_data
        |> Array.mapi (fun i x -> (i, x))
        |> pmap data.par (fun (i, (rnd,count_term)) ->
                (*let timer = new System.Diagnostics.Stopwatch()
                timer.Start()*)
                if i % 2 = 0
                then data.vars |> List.map2 (fun count var -> (count, var)) data.term_count
                               |> List.choose (fun (count, var : Var) -> 
                                 count |> weightRnd_bigint rnd
                                       |> RandomTerms.random_term (rnd,count_term) var.Type)
                               |> mk_proto_individual data
                               |> mk_individual data
                else data.vars |> List.map2 (fun count var -> (count, var)) data.term_count
                               |> List.choose (fun (count, var : Var) -> 
                                 count |> Array.map (fun (x, _) -> (x, 1))
                                       |> weightRnd_int rnd
                                       |> RandomTerms.random_term (rnd,count_term) var.Type)
                               |> mk_proto_individual data
                               |> mk_individual data)
        |> statistics data
//                          |> tap (fun _ -> printfn "Elapsed Time: %A sec" (timer.ElapsedMilliseconds / 1000L)))

let limit_size data i s t =
    if size t <= data.term_sizes.[i]
    then t
    else s

let mutation ((rnd,count_term) : par_data) (data : gp_data) t =
    let t = rename_expr "x" t
    let count_terms A s = count_term (A, [], s)
    let (_, ty, q) =
              t |> positions
                |> List.map (fun p -> (p,1))
                |> List.toArray
                |> weightRnd_int rnd
    let bounds = q |> bounds_at_position t
                   |> List.rev
    let target_typ = bounds |> List.map (fun var -> var.Type)
                            |> (fun typs -> typs ---> ty)
    let args = (List.length << fst << strip_type) target_typ
    let term_count = [|1 .. args + data.term_size|]
                        |> Array.map (fun i -> (i, count_terms target_typ i))
                        |> Array.filter (fun (_, c) -> c > bigint.Zero)
    let s = term_count |> weightRnd_bigint rnd
                       |> RandomTerms.random_term (rnd,count_term) target_typ
                       |> Option.get
                       |> rename_expr "y"
                       |> (fun lam -> Expr.Applications(lam, List.map (List.singleton << Expr.Var) bounds))
    t |> substitute (s, q)
      |> expand Map.empty

let Mutation ((rnd,count_term) : par_data) (data : gp_data) i =
    if rnd.NextDouble() < data.mutation_prob
    then let indx = rnd.Next (List.length i)
         let (prefix, x, suffix) = select_one indx i
         let x' = clone_expr x
         let x'' = x |> mutation (rnd,count_term) data
                     |> limit_size data indx x'
         prefix @ (x'' :: suffix)
    else i

let crossover (rnd : System.Random) (data : gp_data) s t =
    let s = rename_expr "x" s
    let t = rename_expr "y" t
    let ps = positions s
    let qs = positions t
    let valid_cross_points (_, (tao:System.Type), p) =
        let cod = bounds_at_position s p
        qs |> List.filter (fun (_, (tao':System.Type), _) -> tao.ToString() = tao'.ToString())
           |> List.choose (fun (t_q, _, _) -> 
                    let dom = frees t_q
                    match dom with
                        [] -> Some ((t_q, []), bigint.One)
                       | _ -> dom |> List.map (fun v -> (v, List.filter (fun (v':Var) -> v.Type.ToString() = v'.Type.ToString()) cod))
                                  |> List.map (fun (v, vs) -> ((v, vs), bigint(List.length vs)))
                                  |> (fun xs -> let count = List.fold (fun c (_,s) -> c*s) bigint.One xs
                                                if count > bigint.Zero
                                                then xs |> List.map fst
                                                        |> (fun xs -> Some ((t_q, xs), count))
                                                else None))
           |> (fun xs -> if List.isEmpty xs
                         then None
                         else Some ((p, xs), List.sumBy snd xs))
    ps |> List.choose valid_cross_points
       |> List.toArray
       // choose a p
       |> weightRnd_bigint rnd
       |> (fun (p, xs) -> (p, xs |> List.toArray
                                 |> weightRnd_bigint rnd))//choose a q
       |> (fun (p, (t_q, xs)) -> (p, (t_q, List.map (fun (v, vs) -> (v, vs |> List.map (fun v' -> (v',1))
                                                                           |> List.toArray
                                                                           |> weightRnd_int rnd
                                                                           |> Expr.Var)) xs))) // choose a sigma
       |> (fun (p, (t_q, sigma)) -> substitute (subst sigma t_q, p) s)
       |> expand Map.empty

let Crossover (rnd : System.Random) data i i' =
    let indx = rnd.Next (List.length i)
    let (prefix, s, suffix) = select_one indx i
    let (_, t, _) = select_one indx i'
    let s' = clone_expr s
    let x = t |> crossover rnd data s
              |> limit_size data indx s'
    prefix @ (x :: suffix)

let gp (data : gp_data) : gp_result =
    printfn "%s" (gp_data_to_string data)
    let rest_size = data.population_size - data.bests
    let make_new_ind (rnd : Random, count_term) =
        if rnd.NextDouble () < 0.5
        then data.vars |> List.map2 (fun count var -> (count, var)) data.term_count
                       |> List.choose (fun (count, var : Var) -> 
                         count |> weightRnd_bigint rnd
                               |> RandomTerms.random_term (rnd,count_term) var.Type)
        else data.vars |> List.map2 (fun count var -> (count, var)) data.term_count
                       |> List.choose (fun (count, var : Var) -> 
                         count |> Array.map (fun (x, _) -> (x, 1))
                               |> weightRnd_int rnd
                               |> RandomTerms.random_term (rnd,count_term) var.Type)
    let next_generation data pool =
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        let pool = Array.sortBy (fun i -> (-i.fitness, List.map size i.genome)) pool
        printfn "Best individual: %f" data.statistics.[0].best_individual.fitness
        printfn "Best individual error: %f" (data.error data.statistics.[0].best_individual.fitness)
        printfn "Best individual statistics:"
        List.iter (fun e -> printfn "size: %i, depth: %i" (size e) (depth e)) pool.[0].genome
        printfn "Average fitness: %f" data.statistics.[0].average_fitness
        printfn "Average size: "
        List.iteri (fun indx f -> printfn "Indx %i: %f" indx f) data.statistics.[0].average_size
        printfn "Average depth:"
        List.iteri (fun indx f -> printfn "Indx %i: %f" indx f) data.statistics.[0].average_depth
        printfn "term_sizes: %A" data.term_sizes
        printfn "Unquoted: %s" (Swensen.Unquote.Operators.decompile pool.[0].eta_norm)
        let bests = Array.take data.bests pool
        let pool' = Array.map (fun i -> (i, i.fitness)) pool
        let rest = data.par_data
                        |> Array.take rest_size
                        |> pmap data.par (fun (rnd,count_term) ->
                                let i1 = weightRnd_double rnd pool'
                                let i2 = weightRnd_double rnd pool'
                                let i = i2.genome |> Crossover rnd data i1.genome
                                                  |> Mutation (rnd,count_term) data
                                i |> mk_proto_individual data
                                  |> mk_individual data)
        printfn "Elapsed Time: %i sec" (timer.ElapsedMilliseconds / 1000L)
        printfn ""
        statistics data (Array.append (Array.map (fun i -> (false, i)) bests) rest)
    let rec loop i (data, pool) =
        printfn "%s" data.message
        printfn "Generation: %i" i
        serialize data pool
        if data.error data.statistics.[0].best_individual.fitness = 0.0
        then Solved ({data with statistics = List.rev data.statistics},
                     data.statistics.[0].best_individual)
        elif i < data.generations
        then loop (i + 1) (next_generation data pool)
        else Unsolved data
    printfn "Building initial population..."
    loop 1 (initial_population data)

let num_lambda_terms par size scheme =
    let count_term = RandomTerms.count_term ()
    let count_terms A s = count_term (A, [], s)
    let tcount (var:Var) = 
            [| 1 .. size |]
                |> pmap par (fun i -> (i, count_terms var.Type i))
//                |> Array.filter (fun (_, c) -> c > bigint.Zero)
                |> Array.sortBy fst
                |> (fun L -> printfn "Var: %A, counts: %A" var.Name L
                             L)
    let vars = lambdas scheme
    let term_count = List.map tcount vars
    [1 .. size]
    |> List.map (fun size -> List.map (Array.filter (fun (i,_) -> i <= size)) term_count)
    |> List.map (fun term_count -> List.map (Array.sumBy snd) term_count)
    |> List.map2 (fun size sums -> size, List.fold (fun x y -> x * y) bigint.One sums) [1 .. size]
