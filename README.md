# One - Input processing framework

[![Build Status](https://travis-ci.org/t-sin/one.svg?branch=master)](https://travis-ci.org/t-sin/one)
[![Coverage Status](https://coveralls.io/repos/github/t-sin/one/badge.svg?branch=master)](https://coveralls.io/github/t-sin/one?branch=master)

One provides a framework for processing input like stream, pathname and sequence.


> 「ワンか……。ワンといえば英語の１のことだよね。んー、そうだ！　イチ！　イチにしよう。国際的な名まえだ。いいだろう？」
> --- のび太, 『子犬イチの国 キズナ編』, 『ドラえもん』, 2010.10.22放映


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

*One* aimed to write shortly input processing with some feature.

This is the reason which to use *one*.


## Basis

*One* provides three feature in `one:for` macro:

1. less typing for `*standard-input*` (that is `-`)
2. loop absctraction over pathnames, streams and sequences
3. operator composition like pipe on shell or function composition

## Usage

We should tell two things to *one*: **input** and **operations** applied to input. Like pipe in UNIX shell, *one* passes and process the result of left process, and so on. One **operation** is placed with **connective**, it denotes a behavior; composion, reduce, scan on...

Generally, `one:for` should be used like this (with REGEX like notation for description):

```lisp
(one:for <input> [<connective> <operation>]*)
```

### Input

`<input>` can take **pathname**, **stream** (includes `*standard-input*`) and **sequence**. Reading and looping on stream and sequence is hidden behind `one:for` macro, but we can specify how to read from stream or sequence. For details, see *Operation Composition*.

### Connectives

Each *connectives* denotes a behavior. There are five connectives; `$` (composition), `<` (scanning), `>` (gathering), `+>` (folding) and `?` (filtering).

#### `$`: Operation Composition
#### `<`: Scanning on pathname, stream or sequence
#### `>`: Gathering previous results
#### `+>`: Folding previous results
#### `?`: Selection previous results

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
    - He shew me an idea as code snippet, how to process lazy, that give me lots of inspiration.
    - https://gist.github.com/snmsts/5abde1792c14c8a36e6c

## Copyright

Copyright (c) 2017 Shinichi TANAKA (shinichi.tanaka45@gmail.com)

## License

Licensed under the MIT License.
