MPP allows to use OCaml as a preprocessor language.

Here's an example:

```
Here are the numbers from 1 to 8.
{< let _ = for i = 1 to 8 do Printf.printf "%d\n" i done >}
```

If you give it to ```mpp -l ocaml | ocaml -stdin```, you will obtain this:
```
Here are the numbers from 1 to 8.
1
2
3
4
5
6
7
8
```

If you give it to ```mpp -l ocaml```, it will become something like this:
```ocaml
 let _ = print_string "Here are the numbers from 1 to 8.\n"
let _ = for i = 1 to 8 do Printf.printf "%d\n" i done
# 2 "/dev/stdin"
 let _ = print_string "\n\n"
```

So, basically, what you put between `{<` and `>}` is left untouched,
while everything else becomes OCaml instructions that simply print the
original content.

If you don't like the default tokens (`{<` and `>}`), you may redefine them, either on the command line (when you invoke MPP) or on the fly (within your to-be-preprocessed contents).

```mpp -sos '<<<' -scs '>>>'``` will use `<<<` instead of `{<`, and `>>>` instead of `>}`.
You can use any string you want (except the empty string, because, obviously, it wouldn't make sense at all).
On the fly, if you use MPP's default options, you may write `(( sos <<<))` and `(( scs >>>))` to do the same thing as above.
You may redefine on the fly any token, but beware: if what you ask is nonsense, don't expect any senseful output.
