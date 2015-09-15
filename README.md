# Cyphejor

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](http://melpa.org/packages/cyphejor-badge.svg)](http://melpa.org/#/cyphejor)
[![Build Status](https://travis-ci.org/mrkkrp/cyphejor.svg?branch=master)](https://travis-ci.org/mrkkrp/cyphejor)

This package allows to shorten major mode names using user-defined rules.

## Installation

If you would like to install the package manually, download or clone it and
put on Emacs' `load-path`, then you can require it in your init file like
this:

```emacs-lisp
(require 'cyphejor)
```

It's available via MELPA, so you can just <kbd>M-x package-install RET
cyphejor RET</kbd>.

## Usage

First you need to set value of variable `cyphejor-rules`. This variable
contains rules that the package uses to generate new names for major modes
from their symbol names (values of `major-mode` variable).

Value of `cyphejor-rules` should be a list. Every element of the list must
be either a list:

```emacs-lisp
(string replacement &rest parameters)
```

where `string` is a “word” in major mode symbol name, `replacement` is
another string to be used instead, `parameters` is a list that may be empty
but may have the following keywords in it as well:

* `:prefix` — put it in the beginning of result string
* `:postfix` — put it in the end of result string

Apart from elements of the form described above the following keywords are
allowed (they influence the algorithm in general):

* `:downcase` — replace words that are not specified explicitly
  with their first letter downcased

* `:upcase` — replace words that are not specified explicitely with their
  first letter upcased

If nothing is specified, use word unchanged separating it from other words
with spaces if necessary.

Example of setup:

```emacs-lisp
(setq
 cyphejor-rules
 '(:upcase
   ("bookmark"    "→")
   ("buffer"      "β")
   ("diff"        "Δ")
   ("dired"       "δ")
   ("emacs"       "ε")
   ("fundamental" "Ⓕ")
   ("inferior"    "i" :prefix)
   ("interaction" "i" :prefix)
   ("interactive" "i" :prefix)
   ("lisp"        "λ" :postfix)
   ("menu"        "▤" :postfix)
   ("mode"        "")
   ("package"     "↓")
   ("python"      "π")
   ("shell"       "sh" :postfix)
   ("text"        "ξ")
   ("wdired"      "↯δ")))
```

Next, just enable `cyphejor-mode` in your configuration file:

```emacs-lisp
(cyphejor-mode 1)
```

## Customization

Access customization interface via <kbd>M-x customize-group cyphejor
RET</kbd>. Currently you can only edit `cyphejor-rules` variable and toggle
`cyphejor-mode` on and off.

## License

Copyright © 2015 Mark Karpov

Distributed under GNU GPL, version 3.
