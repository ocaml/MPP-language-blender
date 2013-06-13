# frag

''frag'' is a tool to extract fragments of a file.

You may run  ./frag -help to see its documentation (implemented using Core.Std.Command).


### Implementation details
It is implemented in OCaml.
It uses Core (an OCaml library by Jane Street), Str (which will be eventually replaced by Re),
and bits of MPP.

frag used to be embedded in MPP, but is now a standalone program.

