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

type gp_data =
    {scheme : Expr
     vars : Var list
     term_size: int
     population_size: int
     generations : int
     bests: int
     mutation_prob: float
     finish: float -> bool
     term_count: (int * bigint) [] list
     timeout : int
     par_data : par_data []
     load_file : string option
     save_file : string
     message: string}

type proto_individual =
    {genome: Expr list
     norm: Expr
     fitness: unit -> float}

[<Serializable>]
type individual =
    {genome: Expr list
     norm: Expr
     fitness: float}

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

let get_gp_data term_size population_size generations bests mutation_prob finish timeOut seed loadFile saveFile message scheme =
    let tcount (var:Var) = 
            let args = (List.length << fst << strip_type) var.Type
            [| 1 .. args + term_size |]
                |> Array.map (fun i -> (i, RandomTerms.count_terms var.Type i))
                |> Array.filter (fun (_, c) -> c > bigint.Zero)
                |> (fun L -> printfn "Var: %A, counts: %A" var.Name L
                             L)
    let par_data = [| 1 .. population_size |]
//                        |> Array.map (fun i -> (Random(i + seed), RandomTerms.count_term))
                        |> Array.map (fun i -> (Random(i + seed), memoize (fun (A,env,s) -> RandomTerms.count_term' A env s)))
    let vars = lambdas scheme
    {scheme = scheme
     vars = vars
     term_size = term_size
     generations = generations
     population_size = population_size
     bests = bests
     mutation_prob = mutation_prob
     finish = finish
     term_count = List.map tcount vars
     timeout = timeOut
     par_data = par_data
     load_file = loadFile
     save_file = saveFile
     message = message}

let mk_proto_individual (data : gp_data) lams : proto_individual =
    let norm = Expr.Applications(data.scheme, List.map List.singleton lams)
                |> expand Map.empty
    //printfn "mk_proto_individual: %s" (Swensen.Unquote.Operators.decompile norm)
    {genome = lams
     norm = norm
//     fitness = Swensen.Unquote.Operators.evalRaw norm}
     fitness = timeout data.timeout (fun () -> 0.0) (fun () -> Swensen.Unquote.Operators.evalRaw norm) ()}

let mk_individual (proto : proto_individual) : individual =
    //printfn "mk_individual"
    {genome = proto.genome
    // needs beta-eta contraction
     norm = proto.norm
     fitness = proto.fitness ()}// |> tap (fun _ -> printfn "Done")

let serialize (data : gp_data) pool =
    let binarySerializer = FsPickler.CreateBinarySerializer()
    use fileStream = new FileStream(data.save_file, FileMode.Create)
    pool |> Array.map (fun i -> ((Array.map serialize_lambda << List.toArray) i.genome, i.fitness))
         |> (fun pool -> binarySerializer.Serialize(fileStream, pool))

let deserialize (data : gp_data) =
    match data.load_file with
      | Some load_file -> 
            printfn "Deserializing GP pool"
            let binarySerializer = FsPickler.CreateBinarySerializer()
            use fileStream = new FileStream(load_file, FileMode.Open)
            binarySerializer.Deserialize<(term [] * float)[]>(fileStream)
                |> Array.map (fun (lams, fitness) -> 
                                let lams = (List.map (deserialize_lambda Map.empty) << Array.toList) lams
                                let norm = Expr.Applications(data.scheme, List.map List.singleton lams)
                                            |> expand Map.empty
                                {genome = lams
                                 norm = norm
                                 fitness = fitness})
      | None -> failwith "Can't deserialize GP pool. Load file not set."

let initial_population (data : gp_data) =
    if Option.isSome data.load_file &&
       File.Exists( Option.get data.load_file )
    then deserialize data
    else 
     data.par_data
        |> Array.mapi (fun i x -> (i, x))
        |> pmap (fun (i, (rnd,count_term)) ->
                (*let timer = new System.Diagnostics.Stopwatch()
                timer.Start()*)
                if i % 2 = 0
                then data.vars |> List.map2 (fun count var -> (count, var)) data.term_count
                               |> List.choose (fun (count, var : Var) -> 
                                 count |> weightRnd_bigint rnd
                                       |> RandomTerms.random_term (rnd,count_term) var.Type)
                               |> mk_proto_individual data
                               |> mk_individual
                else data.vars |> List.map2 (fun count var -> (count, var)) data.term_count
                               |> List.choose (fun (count, var : Var) -> 
                                 count |> Array.map (fun (x, _) -> (x, 1))
                                       |> weightRnd_int rnd
                                       |> RandomTerms.random_term (rnd,count_term) var.Type)
                               |> mk_proto_individual data
                               |> mk_individual)
//                          |> tap (fun _ -> printfn "Elapsed Time: %A sec" (timer.ElapsedMilliseconds / 1000L)))

let mutation ((rnd,count_term) : par_data) (data : gp_data) t =
    let t = rename_expr "x" t
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
                        |> Array.map (fun i -> (i, RandomTerms.count_terms target_typ i))
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
    then let (prefix, x, suffix) = select_one rnd (rnd.Next (List.length i)) i
         prefix @ (mutation (rnd,count_term) data x :: suffix)
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
    let (prefix, s, suffix) = select_one rnd indx i
    let (_, t, _) = select_one rnd indx i'
    let x = crossover rnd data s t
    prefix @ (x :: suffix)

let gp (data : gp_data) : individual option =
    printfn "%s" (gp_data_to_string data)
    let rest_size = data.population_size - data.bests
    let next_generation data pool =
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        let pool = Array.sortBy (fun i -> -i.fitness) pool
        printfn "Best individual: %f" pool.[0].fitness
        printfn "Unquoted: %s" (Swensen.Unquote.Operators.decompile pool.[0].norm)
        let bests = Array.take data.bests pool
        let pool' = Array.map (fun i -> (i, i.fitness)) pool
        let rest = data.par_data
                        |> Array.take rest_size
                        |> pmap (fun (rnd,count_term) ->
                                let i1 = weightRnd_double rnd pool'
                                let i2 = weightRnd_double rnd pool'
                                let i = i2.genome //|> tap (fun _ -> printfn "Starting crossover")
                                                  |> Crossover rnd data i1.genome
                                                  //|> tap (fun _ -> printfn "Starting Mutation")
                                                  |> Mutation (rnd,count_term) data
                                                  //|> tap (fun _ -> printfn "ready...")
                                i |> mk_proto_individual data
                                  |> mk_individual)
        printfn ""
        printfn "Elapsed Time: %i sec" (timer.ElapsedMilliseconds / 1000L)
        Array.append bests rest
    let rec loop i pool =
        printfn "%s" data.message
        printfn "Generation: %i" i
        serialize data pool
        match Array.tryFind (fun i -> data.finish i.fitness) pool with
            | Some i -> Some i
            | None -> if i < data.generations
                      then loop (i + 1) (next_generation data pool)
                      else None
    printfn "Building initial population..."
    data |> initial_population
         |> loop 1
