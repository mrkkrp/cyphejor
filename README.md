# Cyphejor

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/cyphejor-badge.svg)](https://melpa.org/#/cyphejor)
[![CI](https://github.com/mrkkrp/cyphejor/actions/workflows/ci.yaml/badge.svg)](https://github.com/mrkkrp/cyphejor/actions/workflows/ci.yaml)

This package shortens major mode names by using a set of user-defined rules.

## Installation

The package is available via MELPA, so you can just type `M-x
package-install RET cyphejor RET`.

If you would like to install the package manually, download or clone it and
put on Emacs' `load-path`. Then you can require it in your init file like
this:

```emacs-lisp
(require 'cyphejor)
```

## Usage

First, you need to set the value of the variable `cyphejor-rules`. This
variable contains rules that are used to generate new names for major modes
from their symbol names, that is, values of the `major-mode` variable.

`cyphejor-rules` should be a list. Every element of the list should have the
following form:

```emacs-lisp
(string replacement &rest parameters)
```

where `string` is a word in the major mode's symbol name, `replacement` is a
string to be used instead of that word, `parameters` is a list that may
contain the following keywords:

* `:prefix`—put the component at the beginning of the resulting string
* `:postfix`—put the component at the end of resulting string

The following keywords influence the algorithm in general:

* `:downcase`—replace words that are not matched explicitly with their first
  letter downcased

* `:upcase`—replace words that are not matched explicitly with their first
  letter upcased

If nothing is specified, a word will be used unchanged, separated from other
words with spaces if necessary.

Example of a setup:

```emacs-lisp
(setq
 cyphejor-rules
 '(:upcase
   ("bookmark"    "→")
   ("buffer"      "β")
   ("diff"        "Δ")
   ("dired"       "δ")
   ("emacs"       "ε")
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

`cyphejor-mode` can be enabled like this:

```emacs-lisp
(cyphejor-mode 1)
```

## Customization

You can access the customization interface via `M-x customize-group cyphejor
RET`.

## License

Copyright © 2015–present Mark Karpov

Distributed under GNU GPL, version 3.
