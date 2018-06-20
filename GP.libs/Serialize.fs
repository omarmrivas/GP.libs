module Serialize

open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

[<Serializable>]
type typ = Fun of typ * typ
         | Typ of Type
         | Tup of typ []

[<Serializable>]
type term = Free of string * typ
          | Lam of string * typ * term
          | App of term * term

let rec to_typ (fstyp : System.Type) =
    if Reflection.FSharpType.IsFunction fstyp
    then let (ty1, ty2) = Reflection.FSharpType.GetFunctionElements fstyp
         Fun (to_typ ty1, to_typ ty2)
    elif Reflection.FSharpType.IsTuple fstyp
    then let typs = Reflection.FSharpType.GetTupleElements fstyp
         Tup (Array.map to_typ typs)
    else Typ fstyp

let rec from_typ = function
    | Fun (s, t) ->
        Reflection.FSharpType.MakeFunctionType (from_typ s, from_typ t)
    | Typ fstype -> fstype
    | Tup typs -> typs |> Array.map from_typ
                       |> Reflection.FSharpType.MakeTupleType

let rec serialize_lambda = function
    | Lambda (v, body) -> Lam (v.Name, to_typ v.Type, serialize_lambda body)
    | Application (s, t) -> App (serialize_lambda s, serialize_lambda t)
    | Var v -> Free (v.Name, to_typ v.Type)
    | _ -> failwith "Not lambda term"

let rec deserialize_lambda map = function
    | Lam (name, typ, body) -> let typ' = from_typ typ
                               let v = Var(name, typ')
                               Expr.Lambda (v, deserialize_lambda (Map.add (name + ":" + typ'.FullName) v map) body)
    | App (s, t) -> Expr.Application (deserialize_lambda map s, deserialize_lambda map t)
    | Free (name, typ) -> let typ' = from_typ typ
                          Expr.Var (Map.find (name + ":" + typ'.FullName) map)

