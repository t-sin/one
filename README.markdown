# One - Input reading for one-liner

[![Build Status](https://travis-ci.org/t-sin/one.svg?branch=master)](https://travis-ci.org/t-sin/one)
[![Circle CI](https://circleci.com/gh/t-sin/one.svg?style=svg)](https://circleci.com/gh/t-sin/one)

One provide each-line iteration to text file and standard-input.


> 「ワンか……。ワンといえば英語の１のことだよね。んー、そうだ！　イチ！　イチにしよう。国際的な名まえだ。いいだろう？」
> --- のび太, 『子犬イチの国 キズナ編』, 『ドラえもん』, 2010.10.22放映


## Usage

### Basis

One has two macros to iterate procedures on each line:

* `for`
* `forl`

`for` macro uses `cl:read` to read lines from input.
This means, in some case, the line delimited with space seems multiple lines.
It's useful when you want to read numbers.

`forl` macro uses `cl:read-line` to read lines from input.

`for` and `forl` return a value carried by evaluation of its body.
If these has no bodies, return a list consist of _lines_.

### Benefit

If you use one, you can write short code.
It's pretty good when write one-liner with some UNIX commands.

For example, you want to summate second columns of `nums.dat`, you must write code below, __in terminal__!!

    $ cat nums.dat | cut -d ',' -f 2 | ccl -Q -b -e '(print (loop for line = (read *standard-input* nil :eof) until (eq :eof line) sum line))'
    9

_Seriously!? Maybe it's because of the specters!!_

But wait, now.
If you use one and use implementation manager (e.g. [Roswell](https://github.com/snmsts/roswell), [CIM](https://github.com/KeenS/CIM)), this crazy code turns like this:

    $ cat nums.dat | cut -d ',' -f 2 | ros -s one -e "(print (reduce #'+ (one:for (line :stdin))))"
    9

Additionally, one has short nickname `o`.

_OMG! It's shockingly NICE!!_

This is the reason to use one.

### Examples

Calling `for`/`forl` macro with no body carries a list of lines.

Example 1.

    CL-USER> (one:for (line #P"/path/to/dat.txt"))
    => ("line-1" "line-2" "line-3")

Example 2.

    CL-USER> (one:for (line P#"/path/to/num.txt"))
    => (11 12 13)
    CL-USER> (reduce #'+ (one:for (line P#"/path/to/num.txt")))
    => 36
    CL-USER> (one:for (line P#"/path/to/num.txt")
               (format t "~a~%" line))
    11
    12
    13
    => (NIL NIL NIL)

Example 3.

These codes below return same list, but second code is faster than first.

    CL-USER> (mapcar #'1+ (one:for (line P#"/path/to/num.txt")))
    => (12 13 14)
    CL-USER> (one:for (line P#"/path/to/num.txt")
               (1+ line))
    => (12 13 14)


## Installation

`git clone` into quicklisp's path and type `(ql:quickload :one)` in your REPL.

## Author

* t-sin (shinichi.tanaka45@gmail.com)

## Copyright

Copyright (c) 2015 t-sin (shinichi.tanaka45@gmail.com)

## License

Licensed under the MIT License.
