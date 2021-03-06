# Compiling and Testing

Since I haven't done anything serious yet, I like to know how to compile all
this stuff together and skip all the project stuff that you'll get with `rebar`
or `lfetool`. I don't know why! I enjoy it, though.

This script will have command to compile and run unit tests.

```{name="usage"}
Available commands:
  compile: Compiles src/*.lfe to ebin.
  test: Compiles src/*.lfe and test/*.lfe to ebin, and runs eunit:test on
        everything from test/.
```

This definition of main gives me two commands and fallback on usage.

```{.lisp name="file:do"}
#!/usr/bin/env lfescript
;;! -pa ebin
;; This file was generated by compiling_and_testing.md.

(defun main
  ([(list "compile")]
    (compile))
  ([(list "test")]
    (test))
  ([_]
    (usage)))

(defun usage []
  (: lfe_io format "<<usage>>~n" ())
  )
```

Both compile and test will need `compile-files`. The `report` option lets me
see errors and `outdir` dumps the beams to `ebin`.

I tried `(: lists map compile-file files)` but got an `unbound symbol` error
for `compile-file`. I'm not sure how to refer to the function, so I'll just
wrap it in a lambda.

```{.lisp name="file:do"}
(defun compile-files [files]
  (: lists map (lambda [file] (compile-file file)) files))

(defun compile-file [filename]
  (: lfe_io format "Compiling: ~s.~n" (list filename))
  (let (((tuple 'ok module) (: lfe_comp file filename '(report #(outdir "ebin")))))
       module))
```

Then `compile` just compiles everything from `src`.

```{.lisp name="file:do"}
(defun compile []
  (compile-files (: filelib wildcard "src/*.lfe")))
```

Then `test` will compile both sets of files and run eunit for all the test
modules.

```{.lisp name="file:do"}
(defun test []
  (compile)
  (let* ((test-files (: filelib wildcard "test/*.lfe"))
         (compiled-modules (compile-files test-files)))

        (lfe_io:format "Test files: ~p ... Compiled modules: ~p~n" (list test-files compiled-modules))

        (: lists map (lambda [module]
                      (: code purge module)
                      (: code load_file module)
                      (: eunit test module))
                     compiled-modules)))
```
