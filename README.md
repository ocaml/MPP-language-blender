# MPP: a meta preprocessor that blends programming languages


MPP is a meta preprocessor.  It works by helping you combine different
programming  languages and tools  in order  to pre-process  the source
code of  your documents  or your programs.  In other words,  thanks to
MPP,  any  programming  language can  easily  become  a  preprocessing
language.

MPP is more  generic than (most) existing tools. It  is intended to be
used to  preprocess any ascii-based  files (strict supersets  of ascii
such as utf-8 are fine), using either the light language built in MPP,
and/or any  programming language of  your choice (currently,  OCaml is
the only  one supported, but supporting other  languages only consists
in writing a short description, so many languages will be supported in
the future).

MPP provides a built-in language,  which is minimal and designed to be
the glue  to combining  different languages. As  part of  its library,
some  builtins are provided.  They include  directives such  as input,
ifdef, ifndef, else, elseifdef, set, get, unset, unsetall, cmd.

MPP is implemented  in OCaml. The goal is to  have a preprocessor that
is easy to  extend, easy to customize and that  has a great potential.

# Examples

Variable not set:
```
$ mpp <<< '(( ifdef X hello world !))'

```
Variable set:
```
$ mpp -set X <<< '(( ifdef X hello world !))'
hello world !
```
Variable set and use its value only if it's set:
```
$ mpp -set X=42 <<< '{{ ifdef X (( get X ))}}'
42
```
Note that `{{` and `}}` are the default tokens to allow nesting,
and `((` and `))` are the default tokens that do not allow nesting.

For nesting to work deeper than 2 levels (in total), you need to name the opening and closing tokens:
```
$ mpp -set X=42 -set Y=23 <<< '{{A ifdef X {{B ifdef Y {{C get Y C}}B}}{{D else (( get X )) D}}A}}'
42
```

Note that you need to leave a space after the opening token because the first thing that follows is considered its name (unless it's followed by a space).
```
$ mpp -set X <<< '(( ifdef X hello world))'
hello world
$ mpp -set X <<< '(( ifdef X hello world ))'
hello world 
```
In the second example above, a space is printed after `world`.
If you don't want a space there, don't put a space before the closing token.
Same applies if you name the tokens:
```
$ mpp -set X <<< '((NAME ifdef X hello worldNAME))'
hello world
```

Names can be an arbitrary long set of characters, as long as it's not a regular space:
```
$ mpp -set X <<< '((<< ifdef X hello world<<))'
hello world
```
Note that you cannot use `<<` as the name for the opening and expect `>>` to be the closing name. The name for both closing and opening have to be identical (and there's no option to make then unidentical).


MPP get its power from being simple and also customizable. You can change opening and closing tokens so that it fits your needs:
```
$ mpp -set X -so '[[' -sc ']]' <<< '[[ ifdef X hello world !]]'
hello world !
```

If you're using MPP to preprocess OCaml files, you might want to use tokens that are "somewhat OCaml friendly", for instance:
```
$ cat example.ml
let () =
  let hello =
    (*# ifdef CAPS "HELLO WORLD"#*)(*# else "hello world"#*)
  in
  print_endline hello
$ mpp -so '(*#' -sc '#*)'  example.ml
let () =
  let hello =
    "hello world"
  in
  print_endline hello
$ mpp -set CAPS -so '(*#' -sc '#*)'  example.ml
let () =
  let hello =
    "HELLO WORLD"
  in
  print_endline hello
```

If MPP's builtins are not powerful enough, you may use your favorite language as a preprocessor:
```
$ cat example2.ml
let () =
  let hello =
    (*#X let _ =
      match Sys.getenv "CAPS" with
      | exception _ -> print_string "\"hello world\""
      | _ -> print_string "\"HELLO WORLD\""
      X#*)
  in
  print_endline hello
$ mpp -l ocaml -sos '(*#' -scs '#*)' example2.ml > tmp.ml
$ cat tmp.ml
 let _ = print_string "let () =\n  let hello =\n    "

# 3 "example2.ml"
 let _ =
      match Sys.getenv "CAPS" with
      | exception _ -> print_string "\"hello world\""
      | _ -> print_string "\"HELLO WORLD\""

# 7 "example2.ml"
 let _ = print_string "\n  in\n  print_endline hello\n"
$ ocaml tmp.ml
let () =
  let hello =
    "hello world"
  in
  print_endline hello
```

The list of supported "foreign preprocessor languages" is displayed by using the `-ll` option:
```
$ mpp -ll
List of foreign block languages available:
ocaml
bash
php
perl
```
If you need an additional language, feel free to open an issue!





# Licence

MPP is under the ISC licence.
http://www.isc.org/downloads/software-support-policy/isc-license/


# Issue reporting

https://github.com/ocaml/MPP-language-blender/issues
