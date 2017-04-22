# One; basic usage

## What's this?

Trying to design `one`.

## Input

### Running on the sequence-thing

```lisp
CL-USER> (for (alexandria:iota 10) print)
;=> (loop
;     :for e :in (alexandria:iota 10)
;     :do (print e))
(1 2 3 4 5 6 7 8 9 10)
```

### Running on the file specified pathname

```lisp
CL-USER> (for #P"seq-10.txt" /line print)
; => (with-open-file (in #P"seq-10.txt")
;      (loop
;        :for e := (read-line in nil :eof)
;        :do (print e)))
0
1
...
```
