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
(declare-function mpvi-ytdl-download-subtitle "mpvi" t)


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
Provide danmaku as sub-file and other options.
If URLONLY is not nil, don't resolve danmaku file."
  (unless (string-match-p "live/" url)
    (let ((info (append (mpvi-extract 'url url)
                        (list :out-url-decorator #'mpvi-bilibili-add-begin-time-to-url
                              :logo "哔哩哔哩"))))
      (if-let* ((items (plist-get info :items)))
          (if (cl-every (lambda (c) (string-match-p "\\?p=[0-9]+" (plist-get c :path))) items)
              `(,@info :index 0) ; https://....?p=1
            info)
        (let ((options (copy-alist mpvi-bilibili-extra-opts)))
          (when (and mpvi-bilibili-enable-danmaku (not urlonly) (eq mpvi-mpv-subtitle-p t))
            (condition-case err ; danmaku.xml -> danmaku.ass
                (when-let* ((sub (mpvi-convert-danmaku.xml (mpvi-ytdl-download-subtitle url))))
                  (push `(sub-file . ,sub) options))
              (error (message "Bilibili load danmaku failed: %S" err))))
          (append info `(:load-opts ,options)))))))

(provide 'mpvi-bilibili)

;;; mpvi-bilibili.el ends here
