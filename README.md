# One - Input processing framework

[![Build Status](https://travis-ci.org/t-sin/one.svg?branch=master)](https://travis-ci.org/t-sin/one)
[![Circle CI](https://circleci.com/gh/t-sin/one.svg?style=svg)](https://circleci.com/gh/t-sin/one)

One provides a framework for processing input like stream, pathname and sequence.


> 「ワンか……。ワンといえば英語の１のことだよね。んー、そうだ！　イチ！　イチにしよう。国際的な名まえだ。いいだろう？」
> --- のび太, 『子犬イチの国 キズナ編』, 『ドラえもん』, 2010.10.22放映


## Installation

```
$ ros install t-sin/one
```

## Usage

### Basis

TBD

### Benefit

TBD

### Examples

- `cat file.txt`

```sh
$ ros -s one -e '(one:for #P"file.txt" < one:read-line* $ print)' -q
```

- `cat file.txt | grep hoge`

```sh
$ ros -s one -e '(one:for #P"file.txt" < one:read-line* ? (search "hoge" _) $ print)' -q
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
