;;; mpvi-subtitle.el --- Utils for subtitle/danmaku -*- lexical-binding: t -*-

;; Copyright (C) 2023 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; URL: https://github.com/lorniu/mpvi
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Convert danmaku file to ass format, which can be shown by mpv.
;;
;; To make it work, make sure `biliass' is installed (pip install biliass)
;; or config `mpvi-danmaku-converter' and `mpvi-danmaku-converter-args' to use
;; another conversion program.
;;
;; For example, download `danmaku2ass.py' to your local disk, and add config:
;;
;;  (setq mpvi-danmaku-converter (list "python3" "/path/to/danmaku2ass.py"))
;;

;;; Code:

(require 'cl-lib)

(declare-function mpvi-call-process "mpvi" t)
(declare-function mpvi-cache-directory "mpvi" t)

(defvar mpvi-danmaku-converter "biliass"
  "Command used to convert danmaku to ass format.
The default program is `biliass'. You can change to another one by config
this variable. You should also need to change `mpvi-danmaku-converter-args' to
fit the program. It also can be a list, for example:

  (list \"python3\" \"path-of-your-danmaku2ass.py\")

representing the local danmaku2ass.py file.")

(defvar mpvi-danmaku-converter-args
  (list "{{input}}"
        "--output"    "{{output}}"
        "--display-region-ratio" "1"
        "--block-keyword-patterns" ""
        "--size"      "1920x1080"
        "--font"      "sans-serif"
        "-dm"         "15.0"
        "-ds"         "10.0"
        "-fs"         "39.0"
        "-a"          "0.8")
  "Args template for the `mpvi-danmaku-converter'.
The first element is the program, others are args, where {{input}} and
{{output}} representing the placeholders of input and output file.")

(defun mpvi-convert-danmaku.xml (&optional danmaku-file confirm)
  "Convert DANMAKU-FILE to ass format and save in the same dir.
When CONFIRM, interactively prompt user with the arguments."
  (interactive)
  (unless danmaku-file
    (setq danmaku-file
          (read-file-name
           "Danmaku file: " (mpvi-cache-directory) nil t nil
           (lambda (f) (or (directory-name-p f) (string-match-p "\\.xml$" f))))))
  (when (or (equal (file-name-extension danmaku-file) "ass") (not (file-regular-p danmaku-file)))
    (user-error "Danmaku file '%s' not valid" danmaku-file))
  (let* ((input (file-truename danmaku-file))
         (dest (let ((f (file-name-sans-extension input)))
                 (concat (if (string-suffix-p ".danmaku" f) (file-name-sans-extension f) f) ".ass")))
         (args (if (or confirm current-prefix-arg)
                   (split-string-shell-command
                    (read-string "Danmaku converter args: "
                                 (string-join (mapcar #'shell-quote-argument mpvi-danmaku-converter-args) " ")))
                 mpvi-danmaku-converter-args))
         (final-args
          (cl-subst input "{{input}}"
                    (cl-subst dest "{{output}}" args :test #'equal) :test #'equal)))
    (with-temp-buffer
      (apply #'mpvi-call-process (append (ensure-list mpvi-danmaku-converter) final-args))
      (setq mpvi-danmaku-converter-args args)
      (if (file-exists-p dest)
          (prog1 dest
            (when (called-interactively-p 'any)
              (kill-new dest)
              (message "Convert done: %s" dest)))
        (user-error "Convert danmaku file to ass failed: %S" (string-trim (buffer-string)))))))

(provide 'mpvi-subtitle)

;;; mpvi-subtitle.el ends here
