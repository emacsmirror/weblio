;;; weblio.el --- Look up Japanese words on Weblio.jp  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Simon Zelazny

;; Author: Simon Zelazny
;; Version: 0.2
;; Package-Requires: ((request "0.3.3") (emacs "25.1"))
;; Keywords: langauges, i18n
;; URL: https://github.com/pzel/weblio.el

;;; Commentary:

;; This package provides two functions, weblio-lookup-region and
;; weblio-lookup-word.  They take the selected (or provided) text and attempt
;; to parse and display its definition from https://weblio.jp.
;; This package in not affiliated with weblio.jp.

;;; Code:
(require 'request)
(require 'dom)

(declare-function dom-by-class "dom")
(declare-function dom-by-tag "dom")
(declare-function request "request")

(defun weblio-lookup-region (start end)
  "Look up selected region in weblio.jp.
Display the results in a fresh buffer, *weblio*

Argument START start of region.
Argument END end of region."
  (interactive "r")
  (weblio-lookup-word (buffer-substring start end)))

(defun weblio-lookup-word (word)
  "Look up WORD in the weblio.jp dictionary."
  (interactive "sWeblio lookup: ")
  (request
    (concat "https://www.weblio.jp/content/" word)
    :parser (lambda () (libxml-parse-html-region (point) (point-max)))
    :error (cl-function
            (lambda (&key symbol-status &allow-other-keys)
              (error (format "failed to load %s with error: %s"
                             word symbol-status))))
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let*
                    ((konkat (lambda(list)
                               (mapcar (lambda(el)
                                         (apply #'concat el))
                                       list)))
                     (konkat-strings (lambda(nodes)
                                       (funcall konkat
                                                (mapcar #'dom-strings nodes))))
                     (response-body data)
                     (midashi (car (dom-by-class response-body "^kijiWrp$")))
                     (header (car (funcall konkat-strings
                                           (dom-by-tag midashi 'h2))))
                     (paragraphs (funcall konkat-strings
                                          (dom-by-tag midashi 'p)))
                     (entries (funcall konkat-strings
                                       (dom-by-class midashi "^kiji$"))))
                  (with-output-to-temp-buffer "*weblio*"
                    (princ (format "%s\n\n" header))
                    (if paragraphs
                        ;; "regular" entries are made up of <p> blocks. Display
                        ;; them
                        (mapcar (lambda(e)
                                  (princ (format "%s\n\n" e)))
                                paragraphs)
                      ;; "jistuyou jiten" entries don't have <p> marks, use
                      ;; bare div.kiji's
                      (mapcar (lambda(e)
                                (princ (format "%s\n\n" e)))
                              entries)))))))
  (message (format "Looking up %s ..." word)))

(provide 'weblio)
;;; weblio.el ends here
