# MPP: a simple preprocessor and a meta preprocessor

## (1/2) A simple preprocessor

MPP is a preprocessor, a little like
[CPP](https://gcc.gnu.org/onlinedocs/cpp/) or
[M4](https://www.gnu.org/software/m4/m4.html),
but simpler therefore generally less powerful.
For instance, you can't define your own functions.
(However, you can do it and much more if you use it as a meta preprocessor, cf. section [A meta preprocessor](#22-a-meta-preprocessor).)

As a simple preprocessor, you may define variables on the command line
and use conditional coding in your files.

### Understanding MPP using examples

Variable not set (doesn't output anything):
```
$ mpp <<< '(( ifdef X hello world !))'

```
Variable set:
```
$ mpp -set X <<< '(( ifdef X hello world !))'
hello world !
```

#### Nesting MPP instructions

Variable set and use its value only if it's set:
```
$ mpp -set X=42 <<< '{{ ifdef X (( get X ))}}'
42
```

Note that `{{` and `}}` are the default tokens to allow nesting,
and `((` and `))` are the default tokens that do not allow nesting.

##### Labeling MPP instructions

For nesting to work deeper than 2 levels (in total), you need to name the opening and closing tokens:
```
$ mpp -set X=42 -set Y=23 <<< '{{A ifdef X {{B ifdef Y {{C get Y C}}B}}{{D else (( get X )) D}}A}}'
42
```
After the opening token, the first thing that immediately follows is considered its label or name, unless it's followed by a space.


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
*Note that you cannot use `<<` as the name for the opening and expect `>>` to be the closing name. The name for both closing and opening have to be identical (and there's no option to make then unidentical).*

#### Customizing tokens defining MPP instructions

MPP get its power from being simple and also **customizable**. You may change opening and closing tokens so that it fits your needs:
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

You may use any sequence of characters except for the regular space (ASCII character 32), which means you may use `FOO` or `bar`, or 42, if it suits your needs.

##### Interference with the rest of your contents

In the following example, `BYE` is the closing token.
```
$ mpp -so 'HELLO' -sc 'BYE' <<< 'HELLO echo 42 BYE'
42
```
Now if the closing token has another meaning outside of being a closing token, you could get interference. Labeling your MPP instructions is how to easily overcome that potential issue.
```
$ mpp -so 'HELLO' -sc 'BYE' <<< 'HELLO! echo 42 BYEBYE !BYE'
42 BYEBYE
```
In the example above, `!` is the label.

In the example below, `42` is the label:
```
$ mpp -so 'HELLO' -sc 'BYE' <<< 'HELLO42 echo 42 BYEBYE 42BYE'
42 BYEBYE
```
Of course, it's advised that you avoid using such confusing customization!



## (2/2) A meta preprocessor

MPP is also a meta preprocessor. Everytime being a simple preprocessor
limits you in what you want to do, wishing you could easily use your
favorite language as a preprocessor, MPP may be the answer to your needs!

Say you are working on an OCaml project. And you wish you could use OCaml
as your preprocessor language. It can be difficult to set up an environment
where you may do that. You may use CamlP5 or PPX of course. But these are
not always lightweight enough to answer your needs. Also, you might want to
use Bash or PHP or Perl as your preprocessor language. Well, PHP is widely
used for preprocessing HTML pages, and it generally uses `<?php` and `?>`
as delimiters. These may be your delimiters of choice, but they could also
interfere in a bad way with your workflow.

MPP allows you to choose your delimiters. And to name them. And to nest them.
With PHP, you cannot  `<?php something <?php ?> something ?>`, can you?

If you chose to use OCaml as the processor language, you may want to
use an opening token starting with `(*` and closing token ending with `*)`,
such as `(*#` and `#*)`. An other idea is to use `begin!` and `!end`.
That's to avoid messing with syntax highlighters.

### Example using OCaml as the preprocessor language


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

The whole power of OCaml may be used. You may use modules, functors, objects, etc.

### How it (quite simply) works

If you use it in a powerful way, you need to understand how the preprocessing works.

The parts outside of MPP blocks are transformed into printing them using your preprocessing language.
Line `A` basically becomes `let _ = print_string "A\n"`.
And MPP instructions are just copy-pasted verbatim.

So, `mpp -l ocaml -sos-noloc '(*#' -scs '#*)'` applied to
```
A (*# B #*) C
```
outputs
```ocaml
 let _ = print_string "A "
B  let _ = print_string " C\n"
```

Note that this example uses `-sos-noloc` instead of `-sos`. These two options
are the same except `-sos-noloc` doesn't keep the original source file's locations.

### Lifting limitations: top-level instructions

Unlike PHP where you may write:
```
<?php
for ($i = 0; $i < 100 ; $i++) {
?>
HTML blah blah
<?php
}
?>
```
You may not write such code with `mpp -l ocaml`, because regular parts are transformed into top-level declarations.
But you may do an equivalent using OCaml modules:
```
(*# module M (Unit: sig end) = struct #*)
HTML blah blah
(*# end
let _ = for i = 0 to 100 do
  let module Tmp = M(struct end) in ()
done
    #*)
```
is transformed by `mpp -l ocaml -sos-noloc '(*#' -scs '#*)'` into
```
module M (Unit: sig end) = struct  let _ = print_string "\nHTML blah blah\n  "
end
let _ = for i = 0 to 100 do
  let module Tmp = M(struct end) in ()
done
     let _ = print_string "\n"
```


### Lifting limitations: nesting

It was mentioned earlier that you could nest MPP instructions. While it's natively available with the simple preprocessor features, it's a bit trickier with the meta preprocessor features.
It likely doesn't happen often that you want to do nesting for that, because it can easily become too complicated.
But it's possible if you really want to. You just need a little extra step.

See, if you do nesting as in the following example:
```
$ mpp -l ocaml -sos-noloc '(*#' -scs '#*)' <<< 'A (*#1 (*#2 B 2#*) 1#*) C'
 let _ = print_string "A "
 (*#2 B 2#*)  let _ = print_string " C\n"
```
the innermost part is not expanded.

To activate expansion up to level 2, run MPP twice!
```
$ mpp -l ocaml -sos-noloc '(*#' -scs '#*)' <<< 'A (*#1 (*#2 B 2#*) 1#*) C' | mpp -l ocaml -sos-noloc '(*#' -scs '#*)'
 let _ = print_string " let _ = print_string \"A \"\n "
 B  let _ = print_string "  let _ = print_string \" C\\n\"\n"
```
So, for expanding up to level N, run MPP n-times.
Note that for each time you run `mpp -l ocaml`, you'll need to run `ocaml` on the output as well to actually do the preprocessing.

A quite extreme use-case would be that you generate some MPP instructions inside some MPP instructions.

That being said, just because it's doable doesn't mean that you should do it.
Nesting MPP instructions might way too rapidly become a nightmare.



### Supported preprocessor languages

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


## Mixing the preprocessor and the meta preprocessor features

To some extend, yes you may mix them:
```
$ mpp -l ocaml -so '((' -sc '))' -sos-noloc '(*#' -scs '#*)' <<< '(( echo let _ = print_endline "hello")) (*# let _ = print_endline "world" #*)'
let _ = print_endline "hello" let _ = print_string " "
let _ = print_endline "world"  let _ = print_string "\n"
```

However, it is advised that you always use a well-chosen subset of MPP's features,
such that you do not end up with something too error-prone.

MPP is meant to be exactly the simple tool that you would otherwise miss.


# Licence

MPP is under the ISC licence.
http://www.isc.org/downloads/software-support-policy/isc-license/


# Issue reporting

https://github.com/ocaml/MPP-language-blender/issues
