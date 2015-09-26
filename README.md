# codic.el [![travis badge][travis-badge]][travis-link] [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

[Codic](http://codic.jp/) for Emacs. [Codic](http://codic.jp/) is naming dictionary
service for Japanese programmers and system engineers. This service is useful for
naming class names, method names, variables name database column etc.

This package Emacs port of [Codic.vim](https://github.com/koron/codic-vim)


## ScreenCast

### Japanese

![codic-naming](image/codic-naming.gif)


### English

![codic-english](image/codic-english.gif)


## Installation

`codic-mode` is available on [MELPA][melpa-link] and [MELPA-STABLE][melpa-stable-link].

You can install `codic` with the following command.

<kbd>M-x package-install [RET] codic [RET]</kbd>


## Command

#### `M-x codic keyword`

Search keyword from Codic dictionary.


#### `M-x codic-translate`

Search keyword by Codic translate Web API.


## Customization

#### `codic-api-token`

API token of [codic.jp](https://codic.jp/docs/api). If this value is non-nil,
`M-x codic` uses Web API instead of local dictonaries.


[melpa-link]: https://melpa.org/#/codic
[melpa-stable-link]: https://stable.melpa.org/#/codic
[melpa-badge]: https://melpa.org/packages/codic-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/codic-badge.svg
[travis-badge]: https://travis-ci.org/syohex/emacs-codic.svg
[travis-link]: https://travis-ci.org/syohex/emacs-codic
