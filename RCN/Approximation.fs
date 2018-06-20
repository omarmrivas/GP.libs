module Approximation

open System
open DataTypes

let zero = 0.0
let one = 1.0
let two = 2.0
let three = 3.0
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
let v_z (V (_, _, z)) = z

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
