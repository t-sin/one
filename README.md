# One - Input processing framework

[![Build Status](https://travis-ci.org/t-sin/one.svg?branch=master)](https://travis-ci.org/t-sin/one)
[![Coverage Status](https://coveralls.io/repos/github/t-sin/one/badge.svg?branch=master)](https://coveralls.io/github/t-sin/one?branch=master)

One provides a framework for processing input like stream, pathname and sequence.

> "'Woof', huh? The japanese word 'Woof' sounds like the English word 'one'... That's it! 'Ichi' means 'one' in Japanese! So let's name him 'Ichi'! An international name. Isn't that great!"
> --- Nobita, "The Kingdom of Ichi the Puppy - The Bonding", "Doraemon", Oct. 22, 2010 on air in Japan

## Installation

```
$ ros install t-sin/one
```

If you try *one* in REPL, load with quicklisp:

```lisp
> (ql:quickload :one)
```

If you use *one* as one-liner, it is useful that this template:

```sh
$ ros run -s one -e '(one:for ...)' -q
```

## Motivation

Sometime, I summarized CSV file with UNIX commands, like this:

```sh
$ cat data.csv
id1,1
id2,2
id3,3
$ cat data.csv | awk -F , '{sum+=$2}END{print sum}'
9
```

But I thought: awk is complex, I want to write it with Common Lisp. Then, I do:

```sh
$ cut -d ',' -f 2 data.csv | ros run -e '(print (loop for line = (read *standard-input* nil :eof) until (eq :eof line) sum line))' -q
9
```

*WTF? Maybe it's 'cause of the specters!!*

However, with this library that I wrote, that crazy one-liner turns into like this:

```sh
$ cut -d ',' -f 2 data.csv | ros run -s one -e '(one:for* - < one:read* +> + 0)' -q
```

*OMG! It's shockingly NICE! 😇*

*One* aimed to write shortly input processing with some features.

This is the reason which to use *one*.


## Basis

*One* provides three features in `one:for` macro:

1. less typing for `*standard-input*` (that is `-`)
2. loop absctraction over pathnames, streams and sequences
3. operator composition like pipe on shell or function composition

## Usage

We should tell two things to *one*: **input** and **operations** applied to input. Like pipe in UNIX shell, *one* passes and process the result of left process to right, and so on. One **operation** is placed with **connective**, it denotes a behavior; composition, reduce, scan on...

Generally, `one:for` should be used like this (with REGEX like notation for explain):

```lisp
(one:for <input> [<connective> <operation>]*)
```

### Input

`<input>` can take **pathname**, **stream** (includes `*standard-input*`) and **sequence**. Reading and looping on stream and sequence is hidden behind `one:for` macro, but we can specify how to read from stream or sequence. For details, see *Scanning on pathname, stream or sequence*.

### Operations

`<operation>` is a function that takes one argument. Basically, previous result is applied with operation then its result passed the next operation. Operations can be those:

- function
- symbol such that it is a function name (`#'` is automatically inserted)
- lambda expression

For the purpose of less typing, *one* provides reader macro `#/` for lambda expression. Example is like this:

```lisp
;; this is expanded: (lambda (input) (string= input "ichi"))
#/(string= _ "ichi")
```

Note that **the symbol `_` in `#/` is replaced with argument of function

### Connectives

Each *connectives* denotes a behavior. There are five connectives; `$` (composition), `<` (scanning), `>` (gathering), `+>` (folding) and `?` (filtering).

#### `$`: Operation Composition

Composition behavior connects previoues functiion to next function.

```lisp
(one:for <input> ... $ <operation> ...)
```

Example:

```lisp
> (one:for "ichi" $ #/(format nil "~a ni" _) $ print)
"ichi ni"
```

#### `<`: Scanning on pathname, stream or sequence

Scanning behavior reads for each element and applies operation on previous result, that can be pathname, stream or sequence.

```lisp
(one:for <input> ... < <next-fn> ...)
```

We can specify how to read element, as `<next-fn>`, for instance, `cdr` for lists, `one:read-line*` for stream. Note that `<next-fn>` for streams must be return `:eof` at EOF.

Example:

```lisp
> (one:for '(:one :two :tree) < cdr $ print)
:ONE
:TWO
:THREE
```

#### `>`: Gathering previous results

Gathering behavior buffers all results of previous operation call and passes it as a list.

```lisp
(one:for <input> ... > <operation> ...)
```

In this case, `<operation>` translates gathering result, like sorting. Note memory usage because of gathering stores all input.

Examples:

```lisp
> (one:for #P"file.txt" < one:read-line* > identity $ print)
("line2" "line1" "line3")

> (one:for #P"file.txt" < one:read-line* > #/(sort _ #'string<) $ print)
("line1" "line2" "line3")
```

#### `+>`: Folding previous results

Folding behavior is a special case of gathering operation like `reduce`. This use memory lesser than gathering.

```lisp
(one:for <input> ... +> <operation> [<init-value>] ...)
```

`<init-value>` is optional. By default `<init-value>` is nil.

In folding, `<operation>` must take two arguments.

Example:

```lisp
> (one:for '("line2" "line1" "line3") < cdr +> (lambda (i v) (format nil "~a ~a" i v)) "" $ print)
" line2 line1 line3"
```

#### `?`: Selection previous results

Selection behavior passes previous operation results through, if the result satisfies `predicate`.

```lisp
(one:for <input> ... ? <predicate> ...)
```

Example:

```lisp
> (one:for '("one" "two" "three" "twenty-one") < cdr ? (search "one" _) $ print)
"one"
"twenty-one"
```

### Examples

- `cat file.txt`

```sh
$ ros -s one -e '(one:for #P"file.txt" < one:read-line* $ print)' -q
```

- `cat file.txt | grep hoge`

```sh
$ ros -s one -e '(one:for #P"file.txt" < one:read-line* ? (search "hoge" _) $ print)' -q
```

- `cat file.csv | awk -F , '{s+=$2}END{print s}'`

```sh
# long...
$ ros run -s one -s split-sequence -e '(one:for* #P"file.csv" < one:read-line* $ (split-sequence:split-sequence #\, _) $ (nth 1 _) $ read-from-string +> + 0)' -q
```

## Author

- Shinichi TANAKA (shinichi.tanaka45@gmail.com)

### Special thanks

- Masatoshi SANO (https://gist.github.com/snmsts)
    - He showed me an idea as code snippet, how to process lazy, that give me lots of inspiration.
    - https://gist.github.com/snmsts/5abde1792c14c8a36e6c

## Copyright

Copyright (c) 2017 Shinichi TANAKA (shinichi.tanaka45@gmail.com)

## License

Licensed under the MIT License.
