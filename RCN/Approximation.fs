module Approximation

open System
open MathNet.Numerics
open DataTypes

let zero = BigRational.FromInt(0)
let one = BigRational.FromInt(1)
let two = BigRational.FromInt(2)
let three = BigRational.FromInt(3)

let rec fact = function
    | 0 -> BigRational.FromInt(1)
    | k -> BigRational.FromInt(k) * fact (k - 1)

let msqrt i x =
    let x0 = x
    let rec babylonian i xn =
        if i <= 0
        then xn
        else babylonian (i-1) ((xn + x / xn) / two)
    babylonian i x0

let pi k = 
    Seq.initInfinite (fun k -> let n = fact (4 * k)*
                                       (BigRational.FromInt(1103) + 
                                        (BigRational.FromInt(26390) * BigRational.FromInt(k)))
                               let d = ((fact k) ** 4) * (BigRational.FromInt(396) ** (4*k))
                               n / d)
        |> Seq.take k
        |> Seq.sum
        |> (fun x -> BigRational.FromInt(9801) / (x * two * msqrt k two))

let e k = 
    Seq.initInfinite (fun k -> let n = one
                               let d = fact k
                               n / d)
        |> Seq.take k
        |> Seq.sum

let exp k x = 
    Seq.initInfinite (fun k -> let n = x ** k
                               let d = fact k
                               n / d)
        |> Seq.take k
        |> Seq.sum

let sin k x = 
    Seq.initInfinite (fun k -> let ex = 2 * k + 1
                               let n = if k % 2 = 0
                                       then x ** ex
                                       else -(x ** ex)
                               let d = fact ex
                               n / d)
        |> Seq.take k
        |> Seq.sum

let sinh k x = 
    Seq.initInfinite (fun k -> let ex = 2 * k + 1
                               let n = x ** ex
                               let d = fact ex
                               n / d)
        |> Seq.take k
        |> Seq.sum

let cos k x = 
    Seq.initInfinite (fun k -> let ex = 2 * k
                               let n = if k % 2 = 0
                                       then x ** ex
                                       else -(x ** ex)
                               let d = fact ex
                               n / d)
        |> Seq.take k
        |> Seq.sum

let cosh k x = 
    Seq.initInfinite (fun k -> let ex = 2 * k
                               let n = x ** ex
                               let d = fact ex
                               n / d)
        |> Seq.take k
        |> Seq.sum

let tan k x = 
    let den = cos k x
    if den.IsZero
    then one
    else sin k x / cos k x

let tanh k x = 
    let den = cosh k x
    if den.IsZero
    then one
    else sinh k x / cosh k x

let ln k x = 
    Seq.initInfinite (fun k -> let n = k+1
                               let ex = 2 * n - 1
                               let n = ((x-one) / (x+one)) ** ex
                               let d = BigRational.FromInt ex
                               n / d)
        |> Seq.take k
        |> Seq.sum
        |> (fun x -> two * x)

let to_base = BigRational.FromInt

(*
let pi = Math.PI
let e = Math.E

let sin  : base_type -> base_type = Math.Sin
let cos  : base_type -> base_type = Math.Cos
let sinh : base_type -> base_type = Math.Sinh
let cosh : base_type -> base_type = Math.Cosh
let log  : base_type -> base_type = Math.Log
let exp  : base_type -> base_type = Math.Exp
let msqrt: base_type -> base_type = Math.Sqrt
let mpow : base_type -> base_type -> base_type = fun x y -> Math.Pow(x,y)

let to_base = double : int -> base_type

let v_x (V (x, _, _)) = x
let v_y (V (_, y, _)) = y
let v_z (V (_, _, z)) = z*)

(*open System
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra

let two = BigRational.FromInt 2

let br_sqrt i x =
    let x0 = (BigRational.Parse << (fun x -> x + "N" )<< string << sqrt << BigRational.ToDouble) x
    let rec babylonian i xn =
        if i <= 0
        then xn
        else babylonian (i-1) ((xn + x / xn) / two)
    babylonian i x0*)
