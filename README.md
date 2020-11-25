# Ray Tracer
3D renderer written in OCaml, based on *The Ray Tracer Challenge* by Jamis Buck.

## Getting started
projects/ contains programs that write ppm image files to images/.

`make silhouette`

Run tests with `make runtest`. Uses OUnit2.

## Library
in src
* canvas.ml: utilities for writing ppm files
* color.ml
* intersections.ml
* matrix.ml
* rays.ml
* reflection.ml: interaction between light and surface
* sphere.ml
* transformation.ml: matrices for rotation, etc.
* tuple.ml: tuples, which are sometimes treated as column matrices
