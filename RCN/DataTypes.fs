module DataTypes

open System
open MathNet.Numerics

type base_type = BigRational

type vector2D = base_type * base_type

[<Serializable>]
type vector3D = V of base_type * base_type * base_type

[<Serializable>]
type matrix = M of vector3D * vector3D * vector3D

[<Serializable>]
type Vertex = vector2D

[<Serializable>]
type Edge = Vertex * Vertex

[<Serializable>]
type Quadrilateral = Vertex * Vertex * Vertex * Vertex

