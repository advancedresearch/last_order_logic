# Geometric Synthesis

This library makes it easier to prove things about geometry.

Types:
```lol
line_ty := I ~= I
square_ty := line_ty ~= line_ty
cube_ty := square_ty ~= square_ty
cube4_ty := cube_ty ~= cube_ty
triangle_ty := (I ~= I) ~= I
inv_triangle_ty := I ~= (I ~= I)
```

Constants:
```lol
li := 1 ~= 1
sq := li ~= li
cu := sq ~= sq
cu4 := cu ~= cu
li0 := 1 ~= 0
li1 := 0 ~= 1
tri := (1 ~= 1) ~= 1
```

Shapes:
```lol
line := \(p : line_ty) = all i : I { p ~ i } : un(1)
square := \(p : square_ty) = line(line(p))
cube := \(p : cube_ty) = line(square(p))
cube4 := \(p : cube4_ty) = square(square(p))
triangle := \(p : triangle_ty) = line(type(p ~ 0)) & type(p ~ 1)
```

Operations:
```lol
swap := \(p : line_ty) = type((p ~ 1) ~= (p ~ 0))
transpose := \(p : square_ty) =
    type(((p ~ (0, 0)) ~= (p ~ (1, 0))) ~=
         ((p ~ (0, 1)) ~= (p ~ (1, 1))))
cube_from_line := \(p : line_ty) = (p ~= p) ~= (p ~= p)
expand_line := \(a : line_ty) = \(b : I) = type((!li ~= a) ~ b)
rot_triangle_left := \(p : triangle_ty) =
    type(((p ~ (0, 1)) ~= (p ~ 1)) ~= (p ~ (0, 0)))
flip_triangle := \(p : triangle_ty) =
    type((p ~ (0, 0)) ~= ((p ~ (0, 1)) ~= (p ~ 1)))
flip_inv_triangle := \(p : inv_triangle_ty) =
    type(((p ~ 0) ~= (p ~ (1, 0))) ~= (p ~ (1, 1)))
```
