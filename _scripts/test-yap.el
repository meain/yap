#!/bin/sh

echo "(require 'package)
(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)
(package-initialize)
(add-to-list 'load-path "\"$PWD"\")
(require 'yap)" >/tmp/yap-emacs-test.el
emacs -nw -q -l /tmp/yap-emacs-test.el
