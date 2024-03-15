;;; mpvi-ps.el --- Media tool based on EMMS and MPV -*- lexical-binding: t -*-

;; Copyright (C) 2023 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; URL: https://github.com/lorniu/mpvi
;; SPDX-License-Identifier: MIT
;; Version: 1.1

;;; Commentary:

;; Platform specialized config.
;;
;; - Use 'danmaku2ass' to convert danmaku file to ass format
;; - Use 'seam' to resolve living url: https://github.com/Borber/seam
;;
;; For Arch user:
;;
;;   yay -S danmaku2ass-git seam-git
;;

;;; Code:

(require 'json)
(require 'cl-lib)

(defvar mpvi-cache-directory)

(declare-function mpvi-log "mpvi" t)
(declare-function mpvi-call-process "mpvi" t)
(declare-function mpvi-extract-playlist "mpvi" t)
(declare-function mpvi-ytdlp-download-subtitle "mpvi" t)


;;; Utils

(defvar mpvi-danmaku2ass "danmaku2ass"
  "Executable command or path of `danmaku2ass'.
It can be executable danmaku2ass command or path of danmaku2ass.py.")

(defvar mpvi-danmaku2ass-args "--protect 80  -ds 5.0  -dm 10.0  --font \"Lantinghei SC\"  --fontsize 37.0  --alpha 0.8  --size 960x768")

(defun mpvi-check-danmaku2ass ()
  "Check if `danmaku2ass' available."
  (or (executable-find mpvi-danmaku2ass)
      (and (string-suffix-p ".py" mpvi-danmaku2ass) (file-exists-p mpvi-danmaku2ass))
      (user-error "Please config `mpvi-danmaku2ass' for danmaku2ass command or path first")))

(defun mpvi-convert-danmaku2ass (danmaku-file &optional confirm)
  "Convert DANMAKU-FILE to ASS format.
If CONFIRM not nil then prompt user the options."
  (interactive (list (and (mpvi-check-danmaku2ass)
                          (read-file-name "Danmaku file: " mpvi-cache-directory nil t) t)))
  (mpvi-check-danmaku2ass)
  (unless (file-regular-p danmaku-file)
    (user-error "Danmaku file '%s' not valid" danmaku-file))
  (let* ((dest (concat (file-name-sans-extension danmaku-file) ".ass"))
         (file (file-truename danmaku-file))
         (options (concat "-o \"" dest "\"  " mpvi-danmaku2ass-args)))
    (when confirm
      (setq options (read-string "Confirm options for danmaku2ass: " options)))
    (setq options (split-string-and-unquote options))
    (with-temp-buffer
      (mpvi-log "Convert danmaku to ass format for %s" danmaku-file)
      (if (executable-find mpvi-danmaku2ass)
          (apply #'mpvi-call-process mpvi-danmaku2ass file options)
        (apply #'mpvi-call-process
               "python3" mpvi-danmaku2ass file options))
      (if (file-exists-p dest)
          (prog1 dest
            (when (called-interactively-p 'any)
              (kill-new dest)
              (message "Convert done: %s" dest)))
        (user-error "Convert danmaku file to ass failed: %S" (string-trim (buffer-string)))))))

(defun mpvi-extract-url-by-seam (platform rid)
  "Get real video url for UP with roomid RID by `seam'.
PLATFORM can be bili, douyu and so on, see `https://github.com/Borber/seam' for detail."
  (unless (executable-find "seam")
    (user-error "You should have `seam' in path to extract url (https://github.com/Borber/seam)"))
  (with-temp-buffer
    (mpvi-log "Get living url with seam for %s: %s" platform rid)
    (mpvi-call-process "seam" (format "%s" platform) rid)
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n")
    (backward-sexp)
    (let* ((json (ignore-errors (json-read)))
           (title (alist-get 'title json))
           (nodes (mapcar (lambda (n) (alist-get 'url n)) (alist-get 'nodes json))))
      (if (> (length nodes) 0)
          (let ((path (if (= (length nodes) 1) (car nodes)
                        (completing-read "Choose living source: " nodes nil t))))
            (prog1 (list path title) ; return path and title
              (mpvi-log "Live url: %s" path)))
        (user-error "Error when get url for %s/%s: %s" platform rid (string-trim (buffer-string)))))))


;;; Bilibili

(defvar mpvi-bilibili-enable-danmaku t)

(defvar mpvi-bilibili-extra-opts `((lavfi . "\"fps=60\"")
                                   (sub-ass-force-margins . "yes")))

(defun mpvi-bilibili-add-begin-time-to-url (url timestart)
  "Add param TIMESTART to URL.
Then the opened URL in browser will begin from TIMESTART instead."
  (format "%s%st=%s" url (if (string-match-p "\\?" url) "&" "?") timestart))

(cl-defmethod mpvi-extract-url ((_ (eql :www.bilibili.com)) url &key urlonly)
  "Return mpv options with danmaku file as sub-file for bilibili URL.
If URLONLY is not nil, don't resolve danmaku file."
  (let (ret)
    (when (and (not urlonly) mpvi-bilibili-enable-danmaku (mpvi-check-danmaku2ass))
      (condition-case err
          ;; danmaku.xml -> danmaku.ass
          (let ((sub (mpvi-convert-danmaku2ass (mpvi-ytdlp-download-subtitle url) current-prefix-arg)))
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


;;; Douyu Living

(cl-defmethod mpvi-extract-url ((_ (eql :www.douyu.com)) url &rest _)
  "Return the real video URL for douyu."
  (when (or (string-match "^https://www.douyu.com/\\([0-9]+\\)" url)
            (string-match "^https://www.douyu.com/topic/[[:alnum:]]+\\?rid=\\([0-9]+\\)" url))
    (let ((ret (mpvi-extract-url-by-seam 'douyu (match-string 1 url))))
      (list :url (car ret) :title (cadr ret) :logo "DouYu"))))

(cl-defmethod mpvi-extract-playlist ((_ (eql :www.douyu.com)) &rest _)
  "No need to check playlist for douyu link." nil)


;;; Douyin Living

(cl-defmethod mpvi-extract-url ((_ (eql :live.douyin.com)) url &rest _)
  "Return the real video URL for douyin living."
  (when (string-match "^https://live.douyin.com/\\([0-9]+\\)" url)
    (let ((ret (mpvi-extract-url-by-seam 'douyin (match-string 1 url))))
      (list :url (car ret) :title (cadr ret) :logo "DouYin"))))

(cl-defmethod mpvi-extract-playlist ((_ (eql :live.douyin.com)) &rest _)
  "No need to check playlist for douyin living link." nil)

(provide 'mpvi-ps)

;;; mpvi-ps.el ends here
