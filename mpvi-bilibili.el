;;; mpvi-bilibili.el --- Extension for Bilibili -*- lexical-binding: t -*-

;; Copyright (C) 2023 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; URL: https://github.com/lorniu/mpvi
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Add danmaku support for bilibili.com.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'mpvi-subtitle)

(defvar mpvi-mpv-subtitle-p)

(declare-function mpvi-extract "mpvi" t)
(declare-function mpvi-ytdlp-download-subtitle "mpvi" t)


;;; Bilibili

(defvar mpvi-bilibili-enable-danmaku t)

(defvar mpvi-bilibili-extra-opts `((lavfi . "fps=60")
                                   (sub-ass-force-margins . "yes")))

(defun mpvi-bilibili-add-begin-time-to-url (url timestart)
  "Add param TIMESTART to URL.
Then the opened URL in browser will begin from TIMESTART instead."
  (format "%s%st=%s" url (if (string-match-p "\\?" url) "&" "?") timestart))

(cl-defmethod mpvi-extract ((_ (eql :bilibili.com)) url &key urlonly)
  "Parse URL for bilibili.
Return mpv options with danmaku file as sub-file.
If URLONLY is not nil, don't resolve danmaku file."
  (unless (string-match-p "live/" url)
    (let ((ret (mpvi-extract 'url url)))
      (if (plist-get :items ret) ret
        (when (and mpvi-bilibili-enable-danmaku (not urlonly) (eq mpvi-mpv-subtitle-p t))
          (condition-case err ; danmaku.xml -> danmaku.ass
              (let ((sub (mpvi-convert-danmaku (mpvi-ytdlp-download-subtitle url))))
                (setq ret (append ret (list :subfile sub))))
            (error (message "Bilibili load danmaku failed: %S" err))))
        ;; default
        (when mpvi-bilibili-extra-opts
          (setq ret (append ret (list :load-opts mpvi-bilibili-extra-opts))))
        ;; begin time
        (append ret `(:out-url-decorator ,#'mpvi-bilibili-add-begin-time-to-url))))))

(provide 'mpvi-bilibili)

;;; mpvi-bilibili.el ends here
