;;; mpvi-bilibili.el --- Compat with Bilibili -*- lexical-binding: t -*-

;; Copyright (C) 2023 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; URL: https://github.com/lorniu/mpvi
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Load danmaku for bilibili.com.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'mpvi-sub)

(declare-function mpvi-extract-playlist "mpvi" t)
(declare-function mpvi-ytdlp-download-subtitle "mpvi" t)


;;; Bilibili

(defvar mpvi-bilibili-enable-danmaku t)

(defvar mpvi-bilibili-extra-opts `((lavfi . "\"fps=60\"")
                                   (sub-ass-force-margins . "\"yes\"")))

(defun mpvi-bilibili-add-begin-time-to-url (url timestart)
  "Add param TIMESTART to URL.
Then the opened URL in browser will begin from TIMESTART instead."
  (format "%s%st=%s" url (if (string-match-p "\\?" url) "&" "?") timestart))

(cl-defmethod mpvi-extract-url ((_ (eql :www.bilibili.com)) url &key urlonly)
  "Return mpv options with danmaku file as sub-file for bilibili URL.
If URLONLY is not nil, don't resolve danmaku file."
  (let (ret)
    (when (and mpvi-bilibili-enable-danmaku
               (not urlonly) (eq mpvi-mpv-subtitle-p t))
      (condition-case err
          ;; danmaku.xml -> danmaku.ass
          (let ((sub (mpvi-convert-danmaku (mpvi-ytdlp-download-subtitle url))))
            (setq ret (list :subfile sub :opts mpvi-bilibili-extra-opts)))
        (error (message "Bilibili load danmaku failed: %S" err))))
    ;; default
    (unless ret
      (setq ret (list :opts mpvi-bilibili-extra-opts)))
    ;; if this is a link with query string of p=NUM
    (when (string-match "^\\(.*\\)\\?p=\\([0-9]+\\)" url)
      (nconc ret `(:playlist-url ,(match-string 1 url) :playlist-index ,(string-to-number (match-string 2 url)))))
    ;; begin time
    (append ret `(:out-url-decorator ,#'mpvi-bilibili-add-begin-time-to-url))))

(cl-defmethod mpvi-extract-playlist ((_ (eql :www.bilibili.com)) url &rest args)
  "Extract playlist for bilibili URL. ARGS are extra arguments.
For bilibili, url with `?p=NUM' suffix is not a playlist link."
  (unless (string-match "^\\(.*\\)\\?p=\\([0-9]+\\)" url)
    (apply #'mpvi-extract-playlist nil url args)))

(provide 'mpvi-bilibili)

;;; mpvi-bilibili.el ends here
