# One; basic usage

## What's this?

Trying to design `one`.

## Input

### Input string

Prefix `/` means `per`. Functions which has the prefix split input into many elements.

```lisp
CL-USER> (for "path/to/lisp" (/split #\/) print)
;=> (loop
;     :for e :in (split-sequence:split-sequence #\/ "path/to/lisp")
;     :do (print e))
```

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

### Running on the standard input

```sh
$ seq 10 | ros -s one -e '(o:for - /line print)'
# => (loop
       :for e := (read-line *standard-input* nil :eof)
       :do (print e))
0
1
...
```

## Chaining

### Basic chaining

```lisp
CL-USER> (for '(0 1 2 3) 1+ 1+)
;=> (loop
;     :for e :in '(0 1 2 3)
;     :collect (1+ (1+ e)))
(2 3 4 5)
```

### Filter or folding

Prefix `>` means `filtering` or `folding`. Functions which has the prefix fold input into lesser elements.

```lisp
CL-USER> (for '(0 1 2 3) >oddp)
;=> (loop
;     :for e :in '(0 1 2 3)
;     :when (oddp e)
;     :collect e)
(1 3)
```


```lisp
CL-USER> (for "3,2,1,5,4" (/split #\,) (sort _ #'<))
(1 2 3 4 5)
```

## Combined operation

```lisp
CL-USER> (for "1,2,3,4,5" (/split #\,) read 1+ (>remove-if #'oddp) >+)
18
```
