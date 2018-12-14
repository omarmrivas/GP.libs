module GP_hol

open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Utils

type par_data = Random * (Type * (Type*bigint) list * int -> bigint)

type proto_individual =
    {genome: Expr list
     eta_norm : Expr
     norm: Expr
     fitness: unit -> float}

[<Serializable>]
type individual =
    {genome: Expr list
     eta_norm : Expr
     norm: Expr
     fitness: float}

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

val get_gp_data :
            memoization     : bool ->
            par             : bool ->
            term_size       : int ->
            max_term_size   : int ->
            delta_size      : int ->
            population_size : int ->
            generations     : int ->
            bests           : int ->
            mutation_prob   : float ->
            error           : (float -> float) ->
            timeOut         : int ->
            seed            : int ->
            loadFile        : string option ->
            saveFile        : string ->
            message         : string ->
            scheme          : Expr
                           -> gp_data

val deserialize :
            data    : gp_data ->
            gp_statistic list * individual []

val str_of_statistic :
            sts     : gp_statistic
                   -> string

val gp : data  : gp_data
              -> gp_result

val str_of_individual : individual
                     -> string

val num_lambda_terms : par    : bool ->
                       size   : int ->
                       scheme : Expr ->
                       (int * Numerics.BigInteger) list
