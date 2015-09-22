# One - Input reading for one-liner

[![Build Status](https://travis-ci.org/t-sin/one.svg?branch=master)](https://travis-ci.org/t-sin/one)
[![Circle CI](https://circleci.com/gh/t-sin/one.svg?style=svg)](https://circleci.com/gh/t-sin/one)

One provide each-line iteration to text file and standard-input.


## Usage

### Basis
One has two macro to iterate each line:

* `for`
* `forl`

`for` macro uses `cl:read` to read lines from input.
This means, in some case, the line delimited with space seems multiple lines.
It's useful when you want to read numbers.

`forl` macro uses `cl:read-line` to read lines from input.

### Examples

Calling `for`/`forl` macro with no body carries a list of lines.

Example 1.

    CL-USER> (one:for (line #P"/path/to/dat.txt"))
    => '("line-1" "line-2" "line-3")

Example 2.

    CL-USER> (one:for (line P#"/path/to/num.txt"))
    => '(11 12 13)
    CL-USER> (reduce #'+ (one:for (line P#"/path/to/num.txt")))
    => 36
    CL-USER> (one:for (line P#"/path/to/num.txt")
               (format t "~a~%" line))
    11
    12
    13
    => nil


## Installation

## Author

* t-sin (shinichi.tanaka45@gmail.com)

## Copyright

Copyright (c) 2015 t-sin (shinichi.tanaka45@gmail.com)

## License

Licensed under the MIT License.
