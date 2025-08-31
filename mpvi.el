;;; mpvi.el --- Watch video and take interactive video notes -*- lexical-binding: t -*-

;; Copyright (C) 2023 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; URL: https://github.com/lorniu/mpvi
;; Package-Requires: ((emacs "28.1") (emms "11"))
;; Keywords: convenience, docs, multimedia, application
;; SPDX-License-Identifier: MIT
;; Version: 1.3.0

;;; Commentary:
;;
;; Integrate MPV, EMMS, Org and other utilities with Emacs.
;;
;;  - Open and control MPV player directly in Emacs
;;  - Enable interactive video notes (timestamp link) for org mode
;;  - Download/convert video or audio conveniently in Emacs
;;  - Make EMMS support Windows
;;
;; Installation:
;;  - Install `emms' and `mpvi' from MELPA and load them
;;  - Install the dependencies: mpv (required), yt-dlp, ffmpeg, danmaku2ass and tesseract
;;
;; Usage:
;;  - Use `mpvi-open' to open a video or audio
;;  - Use `mpvi-control' or `mpvi-seek' to operate the playing video
;;  - Take interactive video notes with command `mpvi-insert'
;;
;; Miscellaneous:
;;  - You can control MPV that is opened by `emms'
;;  - You can open videos in webpage directly with MPV player through this

;;; Code:

(require 'ffap)
(require 'mpvi-emms)

(defgroup mpvi nil
  "Integrate MPV with Emacs."
  :group 'external
  :prefix 'mpvi-)

(defcustom mpvi-cache-directory (expand-file-name "mpvi/" (temporary-file-directory))
  "Used to save temporary files."
  :type 'directory)

(defcustom mpvi-ytdl-extra-options nil
  "The default extra options pass to `yt-dlp' and `mpv'."
  :type 'sexp)

(defcustom mpvi-ffmpeg-extra-options nil
  "The default Extra options pass to `ffmpeg'."
  :type 'sexp)

(defvar mpvi-favor-cmds
  '((cycle pause) ab-loop
    (keypress "T") (mouse 1 1)
    (set-property speed)
    (observe_property 1 mute) (unobserve_property 1)
    sub-add sub-reload sub-remove
    audio-reload video-reload
    playlist-prev playlist-next (playlist-play-index 0) playlist-clear (playlist-remove 0)
    seek revert-seek
    (screenshot "video")
    (screenshot-to-file "1.jpg" "video")
    (show-text "$") show-progress))

(defvar mpvi-favor-props
  '((keep-open) (ontop) (force-window) (force-seekable) (untimed)
    geometry autofit (auto-window-resize) (keepaspect) title screen
    (border) (title-bar) (show-in-taskbar) (osd-bar)
    script script-opts demuxer http-proxy sstep
    brightness contrast saturation gamma hue sharpen
    loop-file loop-playlist ab-loop-count (shuffle)
    playlist playlist-pos playlist-count playlist-path chapter-list
    speed pitch time-pos (pause) (play-direction "forward" "backward")
    audio audio-delay (audio-exclusive) audio-demuxer af ao
    (mute) volume volume-max volume-gain (audio-display nil "embedded-first" "external-first")
    video video-rotate video-crop video-zoom panscan (video-recenter)
    hwdec gpu-api gpu-context (deband) glsl-shader vf vo
    sub (sub-visibility) sub-delay sub-scale sub-pos sub-speed sub-fps (sub-scale-by-window)
    sub-font sub-font-size sub-blur sub-color (sub-ass-override "no" "yes" "scale" "force" "strip")
    sub-ass-style-overrides
    (screenshot-format "png" "jpg" "jpeg" "webp" "jxl" "avif") screenshot-template screenshot-directory
    (ytdl) ytdl-format ytdl-raw-options))

(defvar mpvi-cmd-history nil)

(defvar mpvi-prop-history nil)

(defvar mpvi-start-history nil)

(defvar mpvi-seek-actived nil)

(defvar mpvi-seek-paused nil)

(defvar mpvi-last-save-directory (expand-file-name "mpvi" emms-source-file-default-directory))

(defvar mpvi-annotation-face '(:inherit completions-annotations))

;; Silence compiler

(defvar org-mouse-map)
(defvar org-attach-method)

(declare-function org-link-make-string      "org.el" t t)
(declare-function org-link-set-parameters   "org.el" t t)
(declare-function org-open-at-point         "org.el" t t)
(declare-function org-insert-item           "org.el" t t)
(declare-function org-at-item-p             "org.el" t t)
(declare-function org-display-inline-images "org.el" t t)
(declare-function org-attach-attach         "org.el" t t)
(declare-function org-timer-secs-to-hms     "org.el" t t)
(declare-function org-timer-fix-incomplete  "org.el" t t)
(declare-function org-timer-hms-to-secs     "org.el" t t)
(declare-function org-element-context       "org-element.el" t t)
(declare-function org-element-property      "org-element.el" t t)

;; Helpers

(defvar mpvi-debug nil)

(defun mpvi-log (fmt &rest args)
  "Output log when `mpvi-debug' not nil.
FMT and ARGS are like arguments in `message'.
Turn on `emms-player-mpv-debug' too to show the mpv logs."
  (when mpvi-debug
    (apply #'message (concat "[mpvi] " fmt) args)))

(defun mpvi-cache-directory ()
  "Return the cache directory."
  (let ((dir (expand-file-name mpvi-cache-directory)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    dir))

(defun mpvi-url-p (path)
  "Return if PATH is an URL."
  (member (url-type (url-generic-parse-url path)) '("http" "https")))

(defun mpvi-read-path (prompt default)
  "Read a file path using minibuffer.
PROMPT specifies the prompt string. DEFAULT is used when picking only a
directory from minibuffer."
  (let* ((default-directory (or mpvi-last-save-directory default-directory))
         (target (read-file-name prompt))
         (dirp (or (directory-name-p target) (file-directory-p target)))
         (defdir (lambda (dir)
                   (expand-file-name
                    (if (file-name-absolute-p default)
                        (file-name-nondirectory default)
                      default)
                    dir))))
    (if (zerop (length target))
        (funcall defdir default-directory)
      (if dirp
          (funcall defdir target)
        (expand-file-name target)))))

(defun mpvi-ffap-guesser ()
  "Return proper url or file at current point."
  (let* ((mark-active nil)
         (guess (or (when (derived-mode-p 'org-mode)
                      (let ((elem (org-element-context)))
                        (when (equal 'link (car elem))
                          (setq elem (cadr elem))
                          (pcase (plist-get elem :type)
                            ("mpv" (car (mpvi-parse-link (plist-get elem :path))))
                            ((or "http" "https") (plist-get elem :raw-link))))))
                    (ffap-url-at-point)
                    (ffap-file-at-point))))
    (when (and guess (not (mpvi-url-p guess)))
      (if (file-exists-p guess)
          (when (file-directory-p guess)
            (setq guess (file-name-as-directory guess)))
        (setq guess nil)))
    guess))

(defun mpvi-read-file-or-url (&optional prompt regex map)
  "Read a file name or URL from minibuffer.
Optional PROMPT specifies the prompt string, REGEX used to filter the files,
and MAP is used to define extra keys for current minibuffer."
  (minibuffer-with-setup-hook
      (lambda ()
        (when map
          (use-local-map (make-composed-keymap (list (current-local-map) map)))))
    (let ((file-name-history mpvi-start-history))
      (unwind-protect
          (catch 'ffap-prompter
            (let ((prompt (or prompt "File or url: "))
                  (guess (prog1 (mpvi-ffap-guesser) (ffap-highlight)))
                  (pred (if regex (lambda (item)
                                    (or (file-directory-p item) (string-match-p regex item)))))
                  (elem (cons ffap-url-regexp #'ffap--url-file-handler)))
              (unwind-protect
                  (progn
                    (push elem file-name-handler-alist)
                    (if (ffap-url-p guess)
                        (funcall #'read-file-name-default prompt guess guess nil nil pred)
                      (unless guess
                        (setq guess default-directory))
                      (unless (ffap-file-remote-p guess)
                        (setq guess (abbreviate-file-name (expand-file-name guess))))
                      (read-file-name prompt
                                      (file-name-directory guess) nil nil
                                      (file-name-nondirectory guess)
                                      pred)))
                (setq file-name-handler-alist (delq elem file-name-handler-alist)))))
        (setq mpvi-start-history file-name-history)
        (ffap-highlight t)))))

(defun mpvi-call-process (program &rest args)
  "Helper for `call-process', PROGRAM and ARGS are the same."
  (mpvi-log ">>> %S" (cons program args))
  (apply #'call-process program nil t nil args))

(defun mpvi-time-to-secs (time &optional total)
  "Convert TIME to seconds format.
When there is \\='%' in time, return percent seconds from TOTAL."
  (require 'org-timer)
  (cond ((or (null time) (numberp time)) time)
        ((or (not (stringp time)) (not (string-match-p "^-?[0-9:.%]+$" time)))
         (user-error "This is not a valid time: %s" time))
        ((cl-find ?: time)
         (+ (org-timer-hms-to-secs (org-timer-fix-incomplete time))
            (if-let* ((p (cl-search "." time))) (string-to-number (cl-subseq time p)) 0)))
        ((cl-find ?% time)
         (if (null total)
             (user-error "Percent time need TOTAL non nil")
           (/ (* total (string-to-number (substring time 0 (- (length time) 1)))) 100.0)))
        (t (string-to-number time))))

(defun mpvi-secs-to-hms (secs &optional full truncate)
  "Convert SECS to h:mm:ss.xx format.
If FULL is nil, remove '0:' prefix. If TRUNCATE is non-nil, remove frac suffix."
  (require 'org-timer)
  (let* ((frac (cadr (split-string (number-to-string secs) "\\.")))
         (ts (concat (org-timer-secs-to-hms (truncate secs)) (if frac ".") frac)))
    (when (and (not full) (string-prefix-p "0:" ts))
      (setq ts (cl-subseq ts 2)))
    (if truncate (car (split-string ts "\\.")) ts)))

(defun mpvi-secs-to-string (secs &optional groupp)
  "Truncate SECS and format to string, keep at most 2 float digits.
When GROUPP not nil then try to insert commas to string for better reading."
  (let ((ret (number-to-string
              (if (integerp secs) secs
                (/ (truncate (* 100 secs)) (float 100))))))
    (when groupp
      (while (string-match "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)" ret)
        (setq ret (concat (match-string 1 ret) "," (match-string 2 ret)))))
    ret))

(defun mpvi-latest-org-buffer ()
  "Return the last visited visible org mode buffer."
  (cl-find-if (lambda (buf)
                (with-current-buffer buf
                  (derived-mode-p 'org-mode)))
              (mapcar #'window-buffer (window-list))))

(defun mpvi-mpv-version ()
  "Return the current mpv version as a cons cell."
  (with-temp-buffer
    (process-file emms-player-mpv-command-name nil t nil "--version")
    (goto-char (point-min))
    (save-match-data
      (if (re-search-forward "v\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9+]\\)" (line-end-position) t)
          (list (string-to-number (match-string 1))
                (string-to-number (match-string 2))
                (string-to-number (match-string 3)))
        (user-error "No mpv version found")))))

(defun mpvi-compare-mpv-version (comparefn version)
  "Compare current mpv verion with the special VERSION through COMPAREFN.
VERSION should be a list, like \\='(0 38 0) representing version 0.38.0."
  (let ((current (mpvi-mpv-version)))
    (funcall comparefn
             (+ (* (car current) 1000000) (* (cadr current) 1000) (caddr current))
             (+ (* (car version) 1000000) (* (cadr version) 1000) (caddr version)))))

(defvar-local mpvi-transient-buffer-status nil)

(cl-defun mpvi-transient-buffer (buffer &key title tips content keymap onload wconfig)
  "Pop to BUFFER for transient task.
TITLE and TIPS is shown with header line.
CONTENT is the initial text of the buffer.
KEYMAP is a key map used as the local map.
ONLOAD is a function to execute after buffer ready.
WCONFIG is a list as window popup action."
  (declare (indent 1))
  (with-current-buffer (if (bufferp buffer) buffer (get-buffer-create buffer))
    (let* ((inhibit-read-only t)
           (tip (if tips (mapconcat #'identity tips "  ")))
           (hline (list (propertize " " 'display '(space :width 0.6)) title
                        '(:eval (if mpvi-transient-buffer-status (concat "   " mpvi-transient-buffer-status)))
                        (if tip (propertize " " 'display `((space :align-to (- right ,(length tip)))))) tip)))
      (erase-buffer)
      (setq header-line-format hline)
      (insert content)
      (if keymap (use-local-map keymap))
      (if onload (funcall onload))
      (set-buffer-modified-p nil)
      (pop-to-buffer (current-buffer) wconfig))))


;;; MPV communication

(defcustom mpvi-mpv-autofit nil
  "Autofit of MPV player, that is, initial window size."
  :type '(choice string (const nil)))

(defcustom mpvi-mpv-geometry nil
  "Geometry of MPV player, that is, initial window position."
  :type '(choice string (const nil)))

(defcustom mpvi-mpv-ontop-p t
  "Whether MPV window should be on top."
  :type '(choice (const t) (const :json-false) (const nil)))

(defcustom mpvi-mpv-mute-p nil
  "Whether MPV should mute on start."
  :type '(choice (const t) (const :json-false) (const nil)))

(defcustom mpvi-mpv-border-p t
  "Whether MPV window should show the border."
  :type '(choice (const t) (const :json-false) (const nil)))

(defcustom mpvi-mpv-title-bar-p t
  "Whether MPV window should show the title bar."
  :type '(choice (const t) (const :json-false) (const nil)))

(defcustom mpvi-mpv-subtitle-p t
  "Whether MPV should show the sub title."
  :type '(choice (const t) (const :json-false) (const nil)))

(defcustom mpvi-init-cmds nil
  "Command list to run after MPV process initialized.
See `emms-player-mpv-cmd' for syntax."
  :type '(repeat sexp))

(defcustom mpvi-start-hook nil
  "A function to run after a video is started."
  :type 'function)

(defvar mpvi-current-playing nil)

(defvar mpvi-url-info-cache nil)

(defun mpvi-start (path &optional type beg end)
  "Start MPV player for PATH according to TYPE.

TYPE is a flag:
 - default \\='nil for play directly
 - \\='emms for add to playlist of EMMS
 - \\='append/append-play... for add to playlist of MPV

BEG and END for ab-loop play."
  (if (mpvi-url-p path)
      (unless (executable-find "yt-dlp")
        (user-error "You should have 'yt-dlp' installed to play remote url"))
    (setq path (expand-file-name path)))
  (unless beg (setq beg 0))
  (let* ((current (and (emms-player-mpv-proc-playing-p) (mpvi-cmd `(get_property path))))
         (addp (memq type '(append insert-next append-play insert-next-play playlist)))
         (emmsp (emms-playlist-current-selected-track))
         (try-seek (lambda (path)
                     (when (and path (equal path current)
                                (not addp) (if emmsp (eq type 'emms) (not (eq type 'emms)))
                                (mpvi-seekable))
                       ;; I want to make it loop N times then pause when beg and end is provided.
                       ;; But it seems this is not an easy thing for current MPV.
                       ;; Give up and use ab-loop instead, it will loop forever until manually pause.
                       ;; https://github.com/mpv-player/mpv/issues/13860
                       (mpvi-set 'ab-loop-a (if end beg "no"))
                       (mpvi-set 'ab-loop-b (or end "no"))
                       (mpvi-cmd `(seek ,beg absolute))
                       (mpvi-set 'pause 'no)
                       t))))
    (unless (funcall try-seek path)
      (unless (or addp (eq type 'emms))
        (message "Waiting %s..." path))
      (when (and current (not addp))
        (ignore-errors (mpvi-pause t)))
      (when (and emmsp (not (eq type 'emms)))
        (with-current-emms-playlist
          (emms-stop)
          (setq emms-playlist-selected-marker nil)))
      (let* ((lowp    (ignore-errors (mpvi-compare-mpv-version #'< '(0 38 0))))
             (info    (mpvi-extract nil path))
             (items   (plist-get info :items)) ; item1, item2, ...
             (choices (plist-get info :choices)) ; fmt1, fmt2, ...
             (index   (plist-get info :index))
             (target  (plist-get info :path))
             (entries (or items choices)))
        (cond (addp
               (if target
                   (setq entries `((:path ,target)) index 0)
                 (if (null index) (setq index 0))
                 (if choices (setq entries (list (nth index entries))))))
              (items
               (when (or (null index) (null (nth index items)))
                 (let* ((cs (cl-loop with idx = (or (when-let* ((p (mpvi-get 'path)))
                                                      (cl-position-if (lambda (c) (equal (plist-get c :path) p)) items))
                                                    0)
                                     for c in items
                                     for i from 1
                                     for p1 = (plist-get c :path)
                                     for t1 = (plist-get c :title)
                                     for s = (propertize (format "%d:%s:%s" i p1 (or t1 ""))
                                                         'display (concat (propertize (format "%s " i) 'face 'font-lock-comment-face)
                                                                          (or t1 p1)))
                                     collect (list s (1- i) p1 t1) into rs
                                     finally return (append (cl-subseq rs idx) (cl-subseq rs 0 idx))))
                        (ct (lambda (input pred action)
                              (if (eq action 'metadata)
                                  `(metadata (display-sort-function . ,#'identity)
                                             (annotation-function
                                              . ,(lambda (c)
                                                   (when-let* ((cand (assoc c cs)) (u1 (caddr cand)) (t1 (cadddr cand)))
                                                     (concat (make-string (max 2 (- 60 (string-width t1))) ? )
                                                             (substring-no-properties u1))))))
                                (complete-with-action action cs input pred))))
                        (r1 (progn (select-frame-set-input-focus (selected-frame))
                                   (completing-read (format "Item of [%s]: "
                                                            (or (plist-get info :title)
                                                                (plist-get info :up)))
                                                    ct nil t))))
                   (setq index (cadr (assoc r1 cs)))))
               (setf (nth index entries)
                     (append (mpvi-extract nil (plist-get (nth index entries) :path))
                             (nth index entries))))
              (choices
               (when (or (null index) (null (nth index choices)))
                 (setq index 0)))
              (target
               (setq entries (list info) index 0)))
        (setq entries
              (mapcar (lambda (c) (if (stringp c) (list :path c) c)) entries))
        (unless (funcall try-seek (plist-get (nth index entries) :path))
          (let* ((ytdl-opt (when mpvi-ytdl-extra-options
                             (mapconcat #'identity
                                        (cl-loop with options = (cl-copy-list mpvi-ytdl-extra-options)
                                                 while options
                                                 for key-arg = (pop options)
                                                 for key = (replace-regexp-in-string "^-+" "" (format "%s" key-arg))
                                                 for next-arg = (car options)
                                                 if (and next-arg (not (and (stringp next-arg) (string-prefix-p "-" next-arg))))
                                                 collect (format "%s=%s" key (pop options))
                                                 else collect key)
                                        ",")))
                 (play-cmd `(batch
                             ,@(unless current
                                 `(((set_property speed 1))
                                   ,@(if ytdl-opt `(((set_property ytdl-raw-options ,ytdl-opt))))
                                   ((set_property ontop ,(or mpvi-mpv-ontop-p :json-false)))
                                   ((set_property mute ,(or mpvi-mpv-mute-p :json-false)))
                                   ((set_property border ,(or mpvi-mpv-border-p :json-false)))
                                   ((set_property title-bar ,(or mpvi-mpv-title-bar-p :json-false)))
                                   ((set_property sub-visibility ,(or mpvi-mpv-subtitle-p :json-false)))
                                   ,@(if mpvi-mpv-autofit `(((set_property autofit ,mpvi-mpv-autofit))))
                                   ,@(if mpvi-mpv-geometry `(((set_property geometry ,mpvi-mpv-geometry))))
                                   ,@mpvi-init-cmds))
                             ,@(unless addp
                                 `(((set_property keep-open ,(if (eq type 'emms) 'no 'yes)))
                                   ((set_property pause yes))))
                             ,@(cl-loop with load-opts = (plist-get info :load-opts)
                                        with join-opts = (lambda (list)
                                                           (when list
                                                             (mapconcat
                                                              (lambda (x)
                                                                (if-let* ((x2 (cdr x))
                                                                          (val (thread-last (format "%s" x2) (string-replace "\"" "`"))))
                                                                    (format "%s=%S" (car x) val)
                                                                  (format "%s" (car x))))
                                                              (delq nil list) ",")))
                                        for item in entries
                                        for i from 0
                                        for playp = (and (not addp) (zerop i))
                                        for flag = (if addp
                                                       (if (zerop i)
                                                           (if current-prefix-arg 'insert-next-play 'append-play)
                                                         (if current-prefix-arg 'insert-next 'append))
                                                     (if (zerop i) 'replace 'append))
                                        for opts = (append (if playp
                                                               (append `((start . ,beg))
                                                                       (when end `((ab-loop-a . ,beg) (ab-loop-b . ,end))))
                                                             (when-let* ((title (plist-get item :title)))
                                                               `((force-media-title . ,title))))
                                                           load-opts)
                                        for opts-str = (funcall join-opts opts)
                                        ;; Since mpv 0.38.0, an insertion index argument is added as the third argument
                                        ;; https://mpv.io/manual/master/#command-interface, loadfile
                                        for cmd =
                                        `((loadfile ,(plist-get item :path)
                                                    ,(intern-soft flag)
                                                    ,@(if (and opts-str (not lowp)) (list -1))
                                                    ,@(if opts-str (list opts-str)))
                                          . ,(if playp
                                                 (lambda (_data err)
                                                   (if err (user-error "Load video failed (%S)" err))
                                                   (if mpvi-start-hook (funcall mpvi-start-hook))
                                                   (if-let* ((start-func (plist-get (car entries) :start-func)))
                                                       (funcall start-func)
                                                     (message "%s" (if-let* ((title (plist-get (car entries) :title)))
                                                                       (concat (if-let* ((logo (or (plist-get (car entries) :logo)
                                                                                                   (plist-get info :logo))))
                                                                                   (format "/%s:" logo))
                                                                               (propertize title 'face 'font-lock-keyword-face))
                                                                     ""))))
                                               #'ignore))
                                        collect cmd)
                             ,@(unless addp
                                 `(((playlist-play-index ,index))
                                   ((set_property pause no))))))
                 (launcher (lambda ()
                             (setq mpvi-current-playing `(:origin ,path ,@info))
                             (mpvi-async-cmd play-cmd
                               (lambda (_mpv-data mpv-error)
                                 (when (eq mpv-error 'connection-error)
                                   (mpvi-async-cmd play-cmd)))))))
            (mpvi-log "load-commands: %S" play-cmd)
            (if (and (eq type 'emms) emms-player-mpv-ipc-stop-command)
                (setq emms-player-mpv-ipc-stop-command launcher)
              (funcall launcher))))))))

(cl-defgeneric mpvi-extract (type path &rest args)
  "Extract PATH according TYPE.

Return a plist as the PATH info:
- :item/title/subfile for the real url, display media title and sub-file
- :load-opts for extra options for `loadfile'
- :started for function executed after loaded
- :out-url-decorator for function to decorate url when open in external program
- :items if the PATH is a playlist

TYPE should be keyword as :host format, for example :youtube.com,
if it's nil then this method will be a dispatcher."
  (if-let* ((cate (mpvi-path-category path))
            (info (when (not (equal cate type))
                    (apply #'mpvi-extract cate path args))))
      info
    (if (string-prefix-p "http" path)
        (apply #'mpvi-extract 'url path args)
      (setq path (expand-file-name path))
      (unless (file-readable-p path)
        (user-error "Unkown path `%s'" path))
      (if (file-directory-p path)
          (apply #'mpvi-extract 'dir path args)
        (apply #'mpvi-extract 'file path args)))))

(cl-defmethod mpvi-extract ((_ (eql 'url)) url &rest _)
  "Extract metadata for URL."
  (or (cdr (assoc url mpvi-url-info-cache))
      (let* ((json (mpvi-ytdl-dump-url url))
             (info (list :title (alist-get 'title json)
                         :up (or (alist-get 'uploader json)
                                 (alist-get 'series json)
                                 (alist-get 'id json)))))
        (if (equal "playlist" (alist-get '_type json))
            (setq info
                  `(,@info :items
                           ,(cl-loop for item across (alist-get 'entries json)
                                     for url = (alist-get 'url item)
                                     for title = (alist-get 'title item)
                                     for up = (or (alist-get 'uploader item)
                                                  (alist-get 'series item))
                                     for duration = (alist-get 'duration item)
                                     collect (append (list :path url)
                                                     (if title (list :title title))
                                                     (if up (list :up up))
                                                     (if duration (list :duration duration))))))
          (setq info `(,@info :path ,url)))
        (push (cons url info) mpvi-url-info-cache)
        info)))

(cl-defmethod mpvi-extract ((_ (eql 'file)) path &rest _)
  "Extract PATH for plain file."
  (list :path path))

(cl-defmethod mpvi-extract ((_ (eql 'dir)) path &rest _)
  "Exract PATH for directory."
  (let ((files (directory-files-recursively path (mpvi-emms-video-regex))))
    (list :items files)))

(defun mpvi-path-category (path)
  "Return a symbol as category for PATH."
  (when (string-prefix-p "http" path)
    (let* ((host (url-host (url-generic-parse-url path)))
           (pairs (split-string host "\\.")))
      (intern (concat ":" (car (last (butlast pairs))) "." (car (last pairs)))))))

(defun mpvi-async-cmd (cmd &optional handler)
  "Alias of `emms-player-mpv-cmd', that is, call CMD with HANDLER."
  (declare (indent 1))
  (emms-player-mpv-cmd cmd handler))

;;;###autoload
(defun mpvi-cmd (cmd &optional handler)
  "Execute CMD synchronously for MPV player.
HANDLER will be called if it is not nil when execute success.
This can be called interactively."
  (interactive (list
                (let* ((_ (mpvi-check-live))
                       (cmds (mapcar (lambda (c)
                                       (string-join
                                        (cl-loop for i from 0 for r in (ensure-list c)
                                                 for s = (format "%S" r)
                                                 if (= i 0) collect (propertize s 'face 'bold)
                                                 else collect s)
                                        " "))
                                     mpvi-favor-cmds))
                       (cand (completing-read "Exec MPV Command: " cmds
                                              nil nil nil 'mpvi-cmd-history (car mpvi-cmd-history))))
                  (car (read-from-string (concat "(" cand ")"))))))
  (save-match-data
    (when (and (emms-player-mpv-proc-playing-p)
               (process-live-p emms-player-mpv-proc))
      (let ((result nil) (error nil) (done nil))
        (mpvi-async-cmd (ensure-list cmd)
          (lambda (data err)
            (if handler (funcall handler data err))
            (setq result data error err done t)))
        (with-timeout (5 (user-error "MPV command timed out after 5 seconds"))
          (while (and (not done) (process-live-p emms-player-mpv-proc))
            (accept-process-output emms-player-mpv-proc 0.01)))
        (if done
            (if error
                (user-error "Run `%S' failed: %s" cmd error)
              (if (called-interactively-p 'any)
                  (message "%S  →  %S." cmd result)
                result))
          (user-error "MPV process terminated unexpectedly"))))))

;;;###autoload
(defun mpvi-set (property val)
  "Set PROPERTY to VAL for MPV player."
  (interactive (let* ((_ (mpvi-check-live))
                      (prop (intern
                             (completing-read
                              "Change Property: "
                              (lambda (input pred action)
                                (if (eq action 'metadata)
                                    `(metadata (display-sort-function . ,#'identity))
                                  (complete-with-action action mpvi-favor-props input pred)))
                              nil nil nil 'mpvi-prop-history (car mpvi-prop-history))))
                      (old (mpvi-get prop))
                      (value (if-let* ((item (assoc prop mpvi-favor-props))
                                       (vals (remove old (or (cdr item) '(t nil)))))
                                 (car (read-from-string
                                       (completing-read (format "Change `%s' from `%s' to: " prop old)
                                                        (mapcar (lambda (c) (format "%s" c)) vals)
                                                        nil t)))
                               (read-from-minibuffer "Value: " (format "%s" old) nil t nil old))))
                 (list prop (or value :json-false))))
  (mpvi-cmd `(set_property ,property ,val))
  (if (called-interactively-p 'any)
      (message "Value of prop `%s' changed to: %s" property val)
    val))

;;;###autoload
(defun mpvi-get (property-or-properties &optional raw)
  "Get property value of PROPERTY-OR-PROPERTIES from MPV player.
Unless RAW, transform :json-false to nil."
  (interactive (list
                (progn
                  (mpvi-check-live)
                  (intern
                   (completing-read
                    "Property: "
                    (lambda (input pred action)
                      (if (eq action 'metadata)
                          `(metadata (display-sort-function . ,#'identity))
                        (complete-with-action action mpvi-favor-props input pred)))
                    nil nil nil 'mpvi-prop-history (car mpvi-prop-history))))))
  (if raw (mpvi-check-live))
  (cl-loop for prop in (ensure-list property-or-properties)
           for value = (mpvi-cmd `(get_property ,prop))
           for tv = (unless (and (not raw) (eq value :json-false)) value)
           if (called-interactively-p 'any)
           do (message "Property `%s' → %s"  prop tv)
           if (atom property-or-properties) return tv
           else collect tv))

(defun mpvi-check-live ()
  "Check if MPV is runing."
  (unless (emms-player-mpv-proc-playing-p)
    (user-error "No living MPV found"))
  (unless (mpvi-compare-mpv-version #'> '(0 16 999))
    (user-error "You should update MPV to support ipc connect")))

(defun mpvi-origin-path (&optional path)
  "Reverse of `mpvi-extract', return the origin url for PATH.
When PATH is nil then return the path of current playing video."
  (unless path
    (mpvi-check-live)
    (setq path (mpvi-cmd `(get_property path))))
  (or (plist-get mpvi-current-playing :origin) path))

(cl-defun mpvi-pause (&optional (how nil supplied))
  "Set or toggle pause state for MPV player.
When HOW is SUPPLIED, explictly turn pause on or off.
Otherwise, toggle pause state."
  (interactive)
  (mpvi-check-live)
  (mpvi-cmd (if supplied
                (let ((how1 (pcase how ('t 'yes) ('nil 'no) (_ how))))
                  `(set pause ,how1))
              `(cycle pause)))
  (when mpvi-seek-actived
    (when current-prefix-arg ; with prefix, remain the state after exit minibuffer
      (setq mpvi-seek-paused (if (mpvi-get 'pause) 'yes 'no)))
    (mpvi-revert-seek)))

(defun mpvi-toggle-fullscreen ()
  "Toggle fullscreen for MPV player."
  (interactive)
  (mpvi-check-live)
  (mpvi-async-cmd `(cycle fullscreen)
    (lambda (&rest _)
      (if mpvi-seek-actived (throw 'mpvi-seek nil)))))

(defun mpvi-toggle-ontop ()
  "Toggle display on top for MPV player."
  (interactive)
  (mpvi-check-live)
  (mpvi-async-cmd `(cycle ontop)
    (lambda (&rest _)
      (setq mpvi-mpv-ontop-p (mpvi-get 'ontop))
      (message "On Top: %s" (if mpvi-mpv-ontop-p "enable" "canceled")))))

(defun mpvi-toggle-mute ()
  "Toggle mute for MPV player."
  (interactive)
  (mpvi-check-live)
  (mpvi-async-cmd `(cycle mute)
    (lambda (&rest _)
      (setq mpvi-mpv-mute-p (mpvi-get 'mute))
      (message "Mute: %s" (if mpvi-mpv-mute-p "On" "Off")))))

(defun mpvi-toggle-video ()
  "Toggle video for MPV player.
When video is enabled, force window display."
  (interactive)
  (mpvi-check-live)
  (let ((video (mpvi-get 'video))
        (window (mpvi-get 'force-window)))
    (mpvi-set 'force-window
              (if (and (null video) (null window)) 'yes 'no))
    (mpvi-async-cmd `(cycle video)
      (lambda (&rest _)
        (message "Video: %s" (mpvi-get 'video))))))

(defun mpvi-toggle-border ()
  "Toggle border for MPV player."
  (interactive)
  (mpvi-check-live)
  (mpvi-async-cmd `(cycle border)
    (lambda (&rest _)
      (setq mpvi-mpv-border-p (mpvi-get 'border)))))

(defun mpvi-toggle-title-bar ()
  "Toggle title bar for MPV player."
  (interactive)
  (mpvi-check-live)
  (mpvi-async-cmd `(cycle title-bar)
    (lambda (&rest _)
      (setq mpvi-mpv-title-bar-p (mpvi-get 'title-bar)))))

(defun mpvi-toggle-subtitle ()
  "Toggle display subtitle for MPV player."
  (interactive)
  (mpvi-check-live)
  (mpvi-async-cmd `(cycle sub-visibility)
    (lambda (&rest _)
      (setq mpvi-mpv-subtitle-p (mpvi-get 'sub-visibility)))))

(defun mpvi-load-subtitle (subfile)
  "Load or reload SUBFILE for current playing video."
  (interactive (list (progn
                       (mpvi-check-live)
                       (read-file-name
                        "Danmaku file: " (mpvi-cache-directory) nil t
                        (ignore-errors
                          (file-name-nondirectory
                           (file-name-sans-extension (aref (mpvi-get 'sub-files) 0))))))))
  (mpvi-check-live)
  (cl-assert (file-regular-p subfile))
  (when (string-suffix-p ".danmaku.xml" subfile) ; bilibili
    (setq subfile (mpvi-convert-danmaku.xml subfile 'confirm)))
  (ignore-errors (mpvi-async-cmd `(sub-remove)))
  (mpvi-async-cmd `(sub-add ,subfile))
  (let ((msg "Sub file loaded!"))
    (if mpvi-seek-actived (throw 'mpvi-seek msg)
      (message msg))))

(defun mpvi-delay-subtitle (n)
  "Delay subtitle for N seconds."
  (interactive (list (progn
                       (mpvi-check-live)
                       (read-number "Subtitle to delay: " (or (mpvi-get 'sub-delay) 0)))))
  (mpvi-check-live)
  (mpvi-set 'sub-delay n))

(defun mpvi-capture-subtitle ()
  "Copy subtitle text of current playing video."
  (interactive)
  (mpvi-check-live)
  (let ((msg "Copied to kill ring, yank to the place you want."))
    (if-let* ((sub (mpvi-get 'sub-text)))
        (kill-new sub)
      (setq msg "No sub text found"))
    (if mpvi-seek-actived (throw 'mpvi-seek msg)
      (message msg))))

(defun mpvi-ab-loop ()
  "Cycle ab-loop for MPV player."
  (interactive)
  (mpvi-cmd `(ab-loop)))

(defun mpvi-seekable (&optional arg)
  "Whether current video is seekable.
Alert user when not seekable when ARG not nil."
  (mpvi-check-live)
  (let ((seekable (mpvi-get 'seekable)))
    (if (and arg (not seekable))
        (user-error "Current video is not seekable, do nothing")
      seekable)))

(defun mpvi-volume (&optional val)
  "Tune volume base on VAL for MPV player.
VAL is nil for reset to 100. If it is a number, set volume directly.
If it is a string, with prefix +/- for relative value, and can be:

  2, +2, -2, 2x, -2x

That is:

  To 2, add 2, sub 2, add 2 times, sub 2 times."
  (interactive (list (progn
                       (mpvi-check-live)
                       (let ((str (read-string (format "Set volume to (20,±20,2x ∈ [0, 100+]. current %s): "
                                                       (mpvi-get 'volume)))))
                         (if (string-blank-p str) 100 str)))))
  (mpvi-check-live)
  (unless val (setq val 100))
  (let (relative multiply)
    (when (stringp val)
      (cond ((string-match-p "^[+-]?[0-9.]+$" val) ; 2, +2, -2
             (when (memq (aref val 0) '(?+ ?-)) ; string with +- prefix: relative
               (setq relative t))
             (setq val (string-to-number val))) ; 2x, +2x, -2x
            ((string-match-p "^[+-]?[0-9.]+[xX]$" val)
             (setq multiply t)
             (setq val (string-to-number val)))
            (t (user-error "Wrong input speed: %s" val))))
    (if multiply
        (mpvi-cmd `(multiply volume ,val))
      (if relative (setq val (if relative (+ (mpvi-get 'volume) val) val)))
      (if (< val 0) (setq val 0))
      (mpvi-set 'volume val)))
  (when (called-interactively-p 'any)
    (message "Volume changed to %s" (mpvi-get 'volume))))

(defun mpvi-speed (&optional val)
  "Tune speed base on VAL for MPV player.
VAL is nil for reset to 1. If it is a number, tune to such speed directly.
If it is a string, with prefix +/- for relative value, and can be:

  2, +2, -2, 2x, -2x

That is:

  To 2, add 2, sub 2, add 2 times, sub 2 times."
  (interactive (list (progn
                       (mpvi-check-live)
                       (let ((str (read-string (format "Set speed to (2,±2,2x ∈ [0.1, 100]. current %s): "
                                                       (mpvi-get 'speed)))))
                         (if (string-blank-p str) 1 str)))))
  (mpvi-check-live)
  (unless val (setq val 1))
  (let (relative multiply)
    (when (stringp val)
      (cond ((string-match-p "^[+-]?[0-9.]+$" val) ; 2, +2, -2
             (when (memq (aref val 0) '(?+ ?-)) ; string with +- prefix: relative
               (setq relative t))
             (setq val (string-to-number val))) ; 2x, +2x, -2x
            ((string-match-p "^[+-]?[0-9.]+[xX]$" val)
             (setq multiply t)
             (setq val (string-to-number val))
             (if (< val 0) (setq val (/ -1.0 val))))
            (t (user-error "Wrong input speed: %s" val))))
    (if multiply
        (mpvi-cmd `(multiply speed ,val))
      (mpvi-set 'speed (if relative (+ (mpvi-get 'speed) val) val))))
  (when (called-interactively-p 'any)
    (message "Speed changed to %s" (mpvi-get 'speed))))

(defun mpvi-time (val)
  "Jump to new playback time based on VAL for MPV player.
VAL can be a number or string. If it is a number, seek to absolute position.
If it is a string, with prefix +/- for relative value, and can be:

  6, 6s, 6f, 6%, 6:06

That is:

  6 seconds, 6 seconds, 6 frames, percent, datetime."
  (interactive (list (progn
                       (mpvi-seekable 'assert)
                       (let ((str (read-string (format "Jump (max: %.1fs, e.g.: [+-]6,6s,6f,6%%,6:06): "
                                                       (mpvi-get 'duration)))))
                         (unless (string-blank-p str) str)))))
  (mpvi-seekable 'assert)
  (when val
    (if (and (stringp val) (string-match "^[+-]?[0-9]+fs?$" val)) ; Nfs for relative frames
        (let ((paused (mpvi-get 'pause)))
          (unwind-protect
              (mpvi-cmd `(frame_step ,(string-to-number val) seek))
            (mpvi-set 'pause (or paused :json-false))))
      (let (relative)
        (when (stringp val)
          (when (memq (aref val 0) '(?+ ?-)) ; string with +- prefix: relative position
            (setq relative t))
          (setq val
                (cond ((string-match-p "^[+-]?[0-9]\\{0,2\\}\\.?[0-9]*%$" val) ; percent%
                       (* (/ (string-to-number (cl-subseq val 0 -1)) 100.0) (mpvi-get 'duration)))
                      ((string-match-p "^[+-]?[0-9.]+s?$" val) ; +32323s
                       (string-to-number val))
                      ((string-match "^\\+?\\(-?[0-9]+:[0-9:.]+\\)$" val) ; 2:23
                       (mpvi-time-to-secs (match-string 1 val)))
                      (t (user-error "Error input pos: %s" val)))))
        (let ((total (mpvi-get 'duration)))
          (when relative
            (setq val (+ val
                         (if (and (> (recursion-depth) 0)
                                  (or (zerop val) (mpvi-get 'pause)))
                             (let ((str (string-trim (minibuffer-contents))))
                               (or (mpvi-time-to-secs str total)
                                   (user-error "Not valid time input")))
                           (mpvi-get 'time-pos)))))
          (if (< val 0) (setq val 0)
            (if (> val total) (setq val total)))
          (mpvi-set 'time-pos val))))))

(defun mpvi-geofit ()
  "Change the initial window size and position."
  (interactive)
  (let ((fit (mpvi-get 'autofit))
        (geo (mpvi-get 'geometry)))
    (mpvi-set 'autofit (read-string "Autofit ([W[xH]]): " fit nil fit))
    (mpvi-set 'geometry (read-string "Geometry ([W[xH]][+-x+-y][/WS], x:y): " geo nil geo))))


;;; Utils Integrated

(defvar mpvi-screenshot-function #'mpvi-screenshot)

(defvar mpvi-ocr-function #'mpvi-ocr)

(defvar mpvi-local-video-handler #'mpvi-ffmpeg-exec)

(defvar mpvi-remote-video-handler #'mpvi-ytdl-exec)

(defvar mpvi-build-link-function #'mpvi-build-mpv-link)

;; screenshot

(defcustom mpvi-clipboard-command
  (cond ((memq system-type '(cygwin windows-nt))
         "powershell -Command \"Add-Type -AssemblyName System.Windows.Forms; [Windows.Forms.Clipboard]::SetImage($([System.Drawing.Image]::Fromfile(\\\"%s\\\")))\"")
        ((eq system-type 'darwin)
         "osascript -e 'set the clipboard to (read (POSIX file \"%s\") as picture)'")
        ((executable-find "xclip")
         ;; A hangs issue:
         ;; https://www.reddit.com/r/emacs/comments/da9h10/why_does_shellcommand_hang_using_xclip_filter_to/
         "xclip -selection clipboard -t image/png -filter < \"%s\" &>/dev/null"))
  "Command used copy the image data to clipboard."
  :type 'sexp)

(defun mpvi-image-to-clipboard (image-file)
  "Save IMAGE-FILE data to system clipboard."
  (if (and mpvi-clipboard-command (file-exists-p image-file))
      (let ((command (format mpvi-clipboard-command (shell-quote-argument image-file))))
        (mpvi-log "Copy image to clipboard: %s" command)
        (shell-command command))
    (user-error "Nothing to do with coping image file")))

;;;###autoload
(defun mpvi-screenshot (video &optional target arg)
  "Screenshot VIDEO and save to TARGET.

VIDEO is a file or url. If it is t then for the playing MPV instance.

If screenshot for a file or url, ARG should be a number for the playback time,
otherwise, ARG should be `video', `subtitles' or `window'.

If TARGET is t, save to clipboard. If it is nil, save to a temporary directory.
If it is \\=interact, prompt to user for the target. It also can be a directory,
file name or full path."
  (interactive (if (and (not current-prefix-arg) (emms-player-mpv-proc-playing-p))
                   (list t nil nil)
                 (list (mpvi-read-file-or-url "Video to screenshot: " (mpvi-emms-video-regex)) nil
                       (read-number "Frame at time second: " 0))))
  (if (stringp target) (setq target (expand-file-name target)))
  (let* ((file (cond ((and (stringp target) (file-regular-p target))
                      target)
                     ((memq target '(interact interactive))
                      (let ((f (mpvi-read-path "Screenshot save to: " (format-time-string "mpv-%F-%X.png"))))
                        (make-directory (file-name-directory f) t) f))
                     (t (expand-file-name (format-time-string "IMG-%s.png")
                                          (if (stringp target) target (mpvi-cache-directory))))))
         (callback (lambda () ; if filename is t save to clipboard, otherwise return temp file name
                     (if (eq target t)
                         (unwind-protect
                             (prog1 (mpvi-image-to-clipboard file)
                               (let ((msg "Screenshot data is copied to clipboard."))
                                 (if mpvi-seek-actived (throw 'mpvi-seek msg) (message msg))))
                           (delete-file file))
                       (prog1 file
                         (kill-new file)
                         (let ((msg (format "Saved to `%s'." file)))
                           (if mpvi-seek-actived (throw 'mpvi-seek msg) (message msg))))))))
    (if (eq video t)
        ;; screenshot for playing instance
        (let ((flags (list "video" "subtitles" "window")))
          (mpvi-check-live)
          (unless (or (null arg) (stringp arg))
            (setq arg (completing-read "Type of screenshot: " flags nil t)))
          (unless (member arg flags) (setq arg "video"))
          (mpvi-cmd `(screenshot-to-file ,file ,arg))
          (funcall callback))
      ;; screenshot for video file or url
      (unless video
        (setq video (mpvi-read-file-or-url "Video: " (mpvi-emms-video-regex))))
      (unless (mpvi-url-p video)
        (setq video (expand-file-name video))
        (unless (file-regular-p video) (user-error "File not exists")))
      (with-temp-buffer
        (if (zerop (call-process "mpv" nil nil nil video
                                 "--no-terminal" "--no-audio" "--vo=image" "--frames=1"
                                 (format "--start=%s" (or arg 0))
                                 "-o" file))
            file
          (user-error "Screenshot failed: %s" (string-trim (buffer-string)))))
      (funcall callback))))

;; ocr

(defcustom mpvi-ocr-cmd (list "tesseract" "{{input}}" "stdout" "-l" "chi_sim")
  "Command list to use for OCR video frame.
Default use `tesseract' as the OCR program. You can change this variable for
another program or modify the args only. The {{input}} representing the source
image."
  :type 'sexp)

(defun mpvi-ocr (file)
  "Run OCR command on the screenshot FILE."
  (with-temp-buffer
    (if (zerop (apply #'mpvi-call-process (cl-subst file "{{input}}" mpvi-ocr-cmd :test #'equal)))
        (buffer-string)
      (user-error "OCR failed: %s" (string-trim (buffer-string))))))

;;;###autoload
(defun mpvi-capture-ocr (video)
  "Copy OCR text of current VIDEO screenshot."
  (interactive (list (if (and (not current-prefix-arg) (emms-player-mpv-proc-playing-p))
                         (mpvi-screenshot t)
                       (read-file-name "Image to OCR: " nil nil t))))
  (let ((ret (funcall mpvi-ocr-function video)))
    (kill-new ret))
  (let ((msg "OCR done and copied into kill ring, please yank it."))
    (if mpvi-seek-actived (throw 'mpvi-seek msg) (message msg))))

;; ffmpeg

(defcustom mpvi-ffmpeg-gif-filter "fps=10,crop=iw:ih:0:0,scale=320:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse"
  "Filter used when use `ffmpeg' to convert to gif file."
  :type 'string)

(defvar mpvi-ffmpeg-option-templates
  `(("-ss 0"         . "seeks (in/out)")
    ("-to 5"         . "end position (in/out)")
    ("-t 5 "         . "duration (in/out)")
    ("-fs"           . "limit file size (out)")
    ("-q 2"          . "quality scale (out/s)")
    ("-an/vn/sn/dn"  . "audio/video/sub/data none")
    ("-c"            . "codec (in/out/s)")
    "-c copy"
    "-c:v libx264"
    "-c:v libx265"
    "-c:v copy"
    "-c:a aac"
    "-c:a mp3"
    "-c:a copy"
    ("-r 30"         . "frame rate (in/out/s)")
    ("-s 1920x1080"  . "frame size (in/out/s)")
    ("-b:v 2M"       . "bitrate")
    ("-b:a 192k"     . "bitrate")
    ("-ar 44100"     . "audio rate")
    ("-ac 2"         . "audio channels")
    ("-c:v libx264 -crf 28 -c:a copy" . "for compress")
    ("-vn -c:a copy -b:a 192k" . "pick only the audio")
    ("-minrate 964K -maxrate 3856K -bufsize 2000K" . "rating")
    ("-ss 00:00:05 -vframes 1 -q:v 2" . "screenshot")
    "-loop"
    "-map 0"
    "-map 0:v"
    "-map 0:a"
    "-map 0:s"
    ("-filter/vf/af/filter_complex" . "filter graph, -filter:v, -filter:a (out/s)")
    ("-af volume=30dB" . "Volume")
    ("-vf transpose=2"  . "Transpose")
    ("-vf scale=480:-1" . "Scale")
    ("-vf scale=iw/2:ih/2" . "Scale")
    ("-vf crop=600:400:0:0" . "Crop")
    ("-vf rotate=10*PI/180:fillcolor=black" . "Rotate")
    ("-vf hflip/vflip" . "Flip")
    ("-vf \"drawtext=text='hello':fontsize=36:x=10:y=H-th-10\"" . "Text")
    ("-vf eq=brightness=0.1:contrast=1.2:saturation=1.3" . "Eq")
    ("-vf format=gray" . "Turn to black/white mode")
    ("-vf boxblur=5:1" . "Blur")
    ("-vf reverse -af areverse" . "Reverse")
    (,(format "-vf %s" mpvi-ffmpeg-gif-filter) . "Gen Gif")
    ("-i watermark.png -filter_complex \"overlay=W-w-10:10\"" . "Watermark")
    ("-i watermark.png -filter_complex \"overlay=x='if(gte(t,2), -w+(t-2)*50, NAN)':y=10\"" . "Watermark"))
  "Every item is an option string or (cons option description).")

(defvar mpvi-ffmpeg-load-hook nil)

(defun mpvi-ffmpeg-exec (file &optional target beg end)
  "Convert local video FILE from BEG to END using ffmpeg, output to TARGET.
This can be used to cut/resize/reformat and so on."
  (cl-assert (file-regular-p file))
  (unless (executable-find "ffmpeg")
    (user-error "Program `ffmpeg' not found"))
  (let* ((beg (if (numberp beg) (format " -ss %s" beg)))
         (end (if (numberp end) (format " -to %s" end)))
         (target (expand-file-name
                  (or target (format-time-string "mpv-video-%s.mp4"))
                  mpvi-last-save-directory))
         (extra (mapconcat (lambda (c) (if (eq (aref c 0) ?-) c (format "%S" c)))
                           (append (if (member (file-name-extension target) '("gif" "webp"))
                                       (list "-vf" mpvi-ffmpeg-gif-filter "-loop" "0")
                                     (list "-c" "copy"))
                                   mpvi-ytdl-extra-options)
                           " "))
         (buffer "*mpvi-ffmpeg-exec*")
         (help-tips (propertize "<global_opts> <in_opts> -i in.file <out_opts> out.file" 'face 'font-lock-comment-face)))
    (cl-labels ((bs ()
                  (buffer-substring-no-properties (point-min) (point-max)))
                (ensure-dest ()
                  (let ((dest (save-excursion
                                (goto-char (point-max))
                                (cl-loop while (re-search-backward "\"" nil t)
                                         when (get-pos-property (point) 'read-only)
                                         return (when (looking-at "\"\\([^\"]+\\)\"")
                                                  (match-string 1))))))
                    (unless dest
                      (state "error" 'warning)
                      (user-error "Not valid output file"))
                    (when (file-exists-p dest)
                      (state "error" 'warning)
                      (user-error "Output file `%s' already exist" dest))
                    (make-directory (file-name-directory dest) t)
                    (setq mpvi-last-save-directory (file-name-directory target))
                    dest))
                (convert ()
                  (interactive)
                  (state "converting...")
                  (let ((dest (ensure-dest))
                        (cmd (split-string-and-unquote (string-replace "\n" " " (concat "ffmpeg " (bs))))))
                    (mpvi-log "Convert %s" file)
                    (condition-case err
                        (with-temp-buffer
                          (apply #'mpvi-call-process cmd)
                          (goto-char (point-min))
                          (if (save-excursion (re-search-forward "^\\(ERROR\\|Error\\) " nil t))
                              (progn
                                (state "error" 'warning)
                                (message "ffmpeg execution failed: %s" (string-trim (bs))))
                            (quit)
                            (kill-new dest)
                            (message "Save to %s done." (propertize dest 'face 'font-lock-keyword-face))))
                      (error (state "error" 'warning) (message "%s" err)))))
                (link ()
                  (interactive)
                  (let ((cmd (string-replace "\n" " " (concat "ffmpeg " (bs)))))
                    (ensure-dest)
                    (quit)
                    (kill-new cmd)
                    (message "Saved %s to kill ring." (propertize cmd 'face 'font-lock-keyword-face))))
                (snippet ()
                  (interactive)
                  (insert (mpvi--read-cmd-option "Insert ffmpeg option: " mpvi-ffmpeg-option-templates)))
                (quit ()
                  (interactive)
                  (state help-tips)
                  (kill-buffer-and-window))
                (state (s &optional face)
                  (with-current-buffer (get-buffer buffer)
                    (setq mpvi-transient-buffer-status
                          (if s (propertize s 'face (or face 'font-lock-string-face))))
                    (force-mode-line-update)
                    (redisplay))))
      (mpvi-transient-buffer buffer
        :title "ffmpeg"
        :tips (list (cl-loop for (c . d) in '(("C-c C-c" . "Convert")
                                              ("C-c C-w" . "Link")
                                              ("C-c C-i" . "Snippet")
                                              ("C-c C-k" . "Quit"))
                             concat (concat " " (propertize c 'face 'font-lock-keyword-face) " " d)))
        :content (concat "-loglevel error\n"
                         (propertize (format "-i %S" (expand-file-name file))
                                     'read-only t 'front-sticky t 'face 'underline)
                         "\n"
                         (if beg (concat beg "\n"))
                         (if end (concat end "\n"))
                         (unless (string-blank-p extra) (concat extra "\n"))
                         (let* ((p (format "%S" target)) (n (length p)))
                           (add-text-properties 0 1 '(read-only t front-sticky t) p)
                           (add-text-properties 0 n '(face underline) p)
                           p))
        :keymap (let ((map (make-sparse-keymap)))
                  (define-key map (kbd "C-c C-c") #'convert)
                  (define-key map (kbd "C-c C-w") #'link)
                  (define-key map (kbd "C-c C-i") #'snippet)
                  (define-key map (kbd "C-c C-k") #'quit)
                  map)
        :onload (lambda ()
                  (hl-line-mode 1)
                  (state help-tips)
                  (run-hooks 'mpvi-ffmpeg-load-hook))
        :wconfig '((display-buffer-in-direction) (direction . bottom))))))

(defun mpvi--read-cmd-option (prompt options)
  "Read with PROMPT for one option from OPTIONS."
  (let* ((os (cl-remove-if-not #'consp options))
         (len (min 50 (if os (+ 2 (apply #'max (mapcar (lambda (c) (length (car c))) os))) 20))))
    (completing-read
     prompt
     (lambda (input pred action)
       (if (eq action 'metadata)
           `(metadata (display-sort-function . ,#'identity)
                      (annotation-function . ,(lambda (c)
                                                (when-let* ((item (assoc c options)))
                                                  (concat (make-string (max 2 (- len (length c))) ? )
                                                          (cdr item))))))
         (complete-with-action action options input pred))))))

;; yt-dlp

(defvar mpvi-ytdl-default-format "bv*+ba/b"
  "Default download video format.

See `man yt-dlp' for the syntax (such as `bv' as `bestvideo', `ba' as
`bestaudio', `+' as `and', `/' as `or', `b' as `best').

A complex example:
  bv*[ext=mp4][vcodec^=avc]+ba[ext=m4a]/b[ext=mp4]/bv*+ba/b

Meaning of `bv*+ba/b' is:
  merge the best video with best audio-only, or return the best merged.")

(defvar mpvi-ytdl-option-templates
  '("-t mp3|mp4"
    "-f bv*+ba/b"
    "-o %(title)s.%(ext)s"
    ("-o %(playlist_index)s - %(title)s.%(ext)s" . "output template for playlist")
    ("--match-filters like_count>?100" . "filter for playlist")
    "--continue"
    "--concurrent-fragments 2"
    "--proxy http://127.0.0.1:1080"
    ("--downloader aria2c" . "external downloader used instead of native")
    "--downloader-args ffmpeg:-ss 0"
    ("--downloader ffmpeg --downloader-args \"ffmpeg:-ss 0 -to 5\"" . "postprocess with ffmpeg")
    "--cookies-from-browser edge"
    "--write-description"
    ("--write-subs" . "also download subtitle")
    "--convert-subs ass|srt|none"
    "--embed-subs"
    "--embed-thumbnail"
    "--embed-metadata"
    "--embed-chapters")
  "Every item is an option string or (cons option description).")

(defvar mpvi-ytdl-load-hook nil)

(defun mpvi-ytdl-dump-url (url)
  "Return metadata of video URL."
  (unless (executable-find "yt-dlp")
    (user-error "Program `yt-dlp' should be installed"))
  (with-temp-buffer
    (condition-case err
        (progn
          (mpvi-log "Request matadata for %s" url)
          (apply #'mpvi-call-process "yt-dlp" url "--dump-single-json" "--flat-playlist" "--no-warnings" mpvi-ytdl-extra-options)
          (goto-char (point-min))
          (json-read))
      (error (user-error "Error when get metadata for %s: %s" url err)))))

(defun mpvi-ytdl-output-field (url field)
  "Get FIELD information for video URL.
FIELD can be id/title/urls/description/format/thumbnail/formats_table and so on."
  (unless (executable-find "yt-dlp")
    (user-error "Program 'yt-dlp' should be installed"))
  (with-temp-buffer
    (mpvi-log "yt-dlp output template for %s of %s" field url)
    (apply #'mpvi-call-process "yt-dlp" url "--print" field mpvi-ytdl-extra-options)
    (goto-char (point-min))
    (if (re-search-forward "^yt-dlp: error:.*$" nil t)
        (user-error "Error to get `yt-dlp' template/%s: %s" field (match-string 0))
      (string-trim (buffer-string)))))

(defun mpvi-ytdl-pick-formats (url)
  "Completing read the video formats for URL through `yt-dlp'."
  (unless (executable-find "yt-dlp")
    (user-error "Program 'yt-dlp' should be installed"))
  (with-temp-buffer
    (apply #'mpvi-call-process "yt-dlp" url "--list-formats" "--quiet" mpvi-ytdl-extra-options)
    (unless (and (goto-char (point-min))
                 (re-search-forward "^ID" nil t)
                 (ignore-errors (forward-line 2)))
      (user-error "Nothing found: %s" (string-trim (buffer-string))))
    (let* ((items (cl-loop with lines = (split-string (string-trim (buffer-substring (point) (point-max))) "\n")
                           for item in lines for i from 1
                           for item-list = (split-string item " +")
                           collect (list (propertize (car item-list) 'face 'font-lock-keyword-face) item item-list))))
      (completing-read-multiple
       (format "Format (Multiple, like '12,76' or 'bv,ba'. Default: %s): " mpvi-ytdl-default-format)
       (lambda (input pred action)
         (if (eq action 'metadata)
             `(metadata
               (display-sort-function . ,#'identity)
               (annotation-function . ,(lambda (c) (concat "  " (cl-subseq (cadr (assoc c items)) (length c))))))
           (complete-with-action action items input pred)))
       nil nil nil nil mpvi-ytdl-default-format))))

(defun mpvi-ytdl-exec (url &optional target beg end)
  "Download and clip video for URL to TARGET. Use BEG and END for trim."
  (cl-assert (mpvi-url-p url))
  (unless (and (executable-find "yt-dlp") (executable-find "ffmpeg"))
    (user-error "Programs `yt-dlp' and `ffmpeg' should be installed"))
  (let* ((meta (mpvi-ytdl-dump-url url))
         (playlistp (alist-get 'entries meta))
         (fmts (if playlistp (list mpvi-ytdl-default-format) (mpvi-ytdl-pick-formats url)))
         (title (alist-get 'title meta))
         (up (or (alist-get 'uploader meta) (alist-get 'series meta)))
         (beg (if (numberp beg) (format " -ss %s" beg)))
         (end (if (numberp end) (format " -to %s" end)))
         (extra (mapconcat (lambda (c) (format " %s" c)) mpvi-ytdl-extra-options " "))
         (buffer "*mpvi-ytdl-exec*")
         dir file)
    (when target
      (if playlistp
          (setq dir target)
        (setq dir (file-name-directory target)
              file (file-name-nondirectory target))))
    (unless dir
      (setq dir mpvi-last-save-directory))
    (unless file
      (setq file (concat (if playlistp "%(playlist)s/%(playlist_index)s - ")
                         "%(title)s_" (string-replace "/" "-or-" (string-join fmts "+"))
                         ".%(ext)s")))
    (cl-labels ((bs ()
                  (buffer-substring-no-properties (point-min) (point-max)))
                (down ()
                  (interactive)
                  (state "downloading...")
                  (setq mpvi-last-save-directory dir) ; record the dir
                  (let ((cmd (split-string-and-unquote (string-replace "\n" " " (bs)))))
                    (mpvi-log "Download/Clip %s" url)
                    (condition-case err
                        (with-temp-buffer
                          (apply #'mpvi-call-process cmd)
                          (goto-char (point-min))
                          (if (save-excursion (re-search-forward "has already been downloaded" nil t))
                              (state "output file is already exist, rename is required!" 'warning)
                            (if (save-excursion (re-search-forward "^ERROR" nil t))
                                (progn
                                  (state "error" 'warning)
                                  (message "Download and clip with yt-dlp/ffmpeg failed: %s" (string-trim (bs))))
                              (let ((dest (or (and (re-search-forward "Destination: \\([^ ]+\\)$" nil t)
                                                   (match-string 1))
                                              (expand-file-name file dir))))
                                (quit)
                                (kill-new dest)
                                (message "Save to %s done." (propertize dest 'face 'font-lock-keyword-face))))))
                      (error (state "error" 'warning) (message "%s" err)))))
                (link ()
                  (interactive)
                  (let ((cmd (string-replace "\n" " " (bs))))
                    (quit)
                    (kill-new cmd)
                    (message "Saved %s to kill ring." (propertize cmd 'face 'font-lock-keyword-face))))
                (snippet ()
                  (interactive)
                  (insert (mpvi--read-cmd-option "Insert yt-dlp option: " mpvi-ytdl-option-templates)))
                (quit ()
                  (interactive)
                  (state nil)
                  (kill-buffer-and-window))
                (state (s &optional face)
                  (with-current-buffer (get-buffer buffer)
                    (setq mpvi-transient-buffer-status
                          (if s (propertize s 'face (or face 'font-lock-string-face))))
                    (force-mode-line-update)
                    (redisplay))))
      (mpvi-transient-buffer buffer
        :title (concat "[" up "] " title (if playlistp " (playlist)"))
        :tips (list (cl-loop for (c . d) in '(("C-c C-c" . "Down")
                                              ("C-c C-w" . "Link")
                                              ("C-c C-i" . "Snippet")
                                              ("C-c C-k" . "Quit"))
                             concat (concat " " (propertize c 'face 'font-lock-keyword-face) " " d)))
        :content (concat (propertize (concat "yt-dlp " url) 'face 'font-lock-constant-face 'read-only t 'front-sticky t) "\n"
                         (format "-f %S\n" (string-join fmts "+"))
                         (when (or beg end)
                           (concat "--downloader ffmpeg --downloader-args \"ffmpeg:" beg end "\"\n"))
                         (unless (string-blank-p extra) (concat extra "\n"))
                         (format "-P %S\n" dir)
                         (format "-o %S" file))
        :keymap (let ((map (make-sparse-keymap)))
                  (define-key map (kbd "C-c C-c") #'down)
                  (define-key map (kbd "C-c C-w") #'link)
                  (define-key map (kbd "C-c C-i") #'snippet)
                  (define-key map (kbd "C-c C-k") #'quit)
                  map)
        :onload (lambda ()
                  (hl-line-mode 1)
                  (state nil)
                  (run-hooks 'mpvi-ytdl-load-hook))
        :wconfig '((display-buffer-in-direction) (direction . bottom))))))

(defun mpvi-ytdl-download-subtitle (url &optional prefix)
  "Download subtitle for URL and save as file named begin with PREFIX."
  (unless (executable-find "yt-dlp")
    (user-error "Program `yt-dlp' should be installed"))
  (with-temp-buffer
    (mpvi-log "Downloading subtitle for %s" url)
    (apply #'mpvi-call-process
           "yt-dlp" url "--write-subs" "--skip-download"
           "-o" (or prefix (expand-file-name "SUB-%(fulltitle)s-%(id)s" (mpvi-cache-directory)))
           mpvi-ytdl-extra-options)
    (goto-char (point-min))
    (if (re-search-forward "Destination:\\(.*\\)$" nil t)
        (string-trim (match-string 1))
      (user-error "Error when download subtitle: %s" (string-trim (buffer-string))))))

;; Timestamp Org Link

(defcustom mpvi-attach-link-attrs "#+attr_html: :width 600"
  "Attrs insert above a inserted attach image.
The :width can make image cannot display too large in org mode."
  :type 'string)

(defun mpvi-parse-link (link)
  "Extract path, beg, end from LINK."
  (if (string-match "^\\([^#]+\\)\\(?:#\\([0-9:.]+\\)?\\(?:-\\([0-9:.]+\\)?\\)?\\)$" link)
      (let ((path (match-string 1 link))
            (beg (match-string 2 link))
            (end (match-string 3 link)))
        (list path (mpvi-time-to-secs beg) (mpvi-time-to-secs end)))
    (user-error "Link is not valid")))

(defun mpvi-parse-link-at-point ()
  "Return the mpv link object at point."
  (unless (derived-mode-p 'org-mode)
    (user-error "You must parse MPV link in org mode"))
  (let* ((link (org-element-context))
         (node (cadr link)))
    (when (equal "mpv" (plist-get node :type))
      (let ((meta (mpvi-parse-link (plist-get node :path)))
            (begin (org-element-property :begin link))
            (end (save-excursion (goto-char (org-element-property :end link)) (skip-chars-backward " \t") (point))))
        `(:path ,(car meta) :vbeg ,(cadr meta) :vend ,(caddr meta) :begin ,begin :end ,end ,@node)))))

(defun mpvi-build-mpv-link (path &optional beg end desc)
  "Build mpv link with timestamp that used in org buffer.
PATH is local video file or remote url. BEG and END is the position number.
DESC is optional, used to describe the current timestamp link."
  (concat
   (org-link-make-string
    (concat
     "mpv:" path (if (or beg end) "#")
     (if beg (number-to-string beg))
     (if end "-")
     (if end (number-to-string end)))
    (concat
     "▶ "
     (if beg (mpvi-secs-to-hms beg nil t))
     (if end " → ")
     (if end (mpvi-secs-to-hms end nil t))))
   (if desc (concat " " desc))))

(defun mpvi-insert-attach-link (file)
  "Save image FILE to org file using `org-attach'."
  (require 'org-attach)
  ;; attach it
  (let ((org-attach-method 'mv)) (org-attach-attach file))
  ;; insert the attrs
  (when mpvi-attach-link-attrs
    (insert (string-trim mpvi-attach-link-attrs) "\n"))
  ;; insert the link
  (insert (org-link-make-string (concat "attachment:" (file-name-nondirectory file))))
  ;; show it
  (org-display-inline-images))

(cl-defmacro mpvi-with-current-mpv-link ((var &optional path errmsg) &rest form)
  "Run FORM when there is a mpv PATH at point that is playing.
Bind the link object to VAR for convenience. Alert user with ERRMSG when
there is a different path at point."
  (declare (indent 1))
  `(progn
     (mpvi-check-live)
     (let ((,var (mpvi-parse-link-at-point)))
       (when (and ,var (not (equal (plist-get ,var :path)
                                   ,(or path `(mpvi-origin-path)))))
         (user-error ,(or errmsg "Current link is not the actived one, do nothing")))
       ,@form)))

;;;###autoload
(defun mpvi-insert (&optional endp)
  "Insert or update timestamp link in org mode buffer.
When ENDP, update end time of the link, and remove end time if seek return nil."
  (interactive "P")
  (mpvi-seekable 'assert)
  (let* ((paused (prog1 (mpvi-get 'pause) (mpvi-pause t)))
         (ori-buf (current-buffer))
         (work-buf (or (mpvi-latest-org-buffer)
                       (user-error "No target org-mode buffer found, abort")))
         (in-place (eq ori-buf work-buf))
         (path (mpvi-origin-path))
         beg end pos desc link)
    (with-current-buffer work-buf
      (if-let* ((node (mpvi-parse-link-at-point)))
          ;; when node is found, try to update the timestamps
          (progn
            (unless (equal (plist-get node :path) path)
              (user-error "Current timestamp link is not for the current playing video, can't update"))
            (setq beg (plist-get node :vbeg)
                  end (plist-get node :vend)
                  pos (if in-place ; seek explictly only when edit a node
                          (mpvi-seek
                           (mpvi-get 'time-pos)
                           (format "%s (%s ∈ [%d, %d]; mm:ss,n,n%%): "
                                   (if (or end endp) (if endp "Loop-B" "Loop-A") "Time")
                                   (if endp end beg)
                                   (if endp beg 0)
                                   (if (or endp (null end)) (mpvi-get 'duration) end)))
                        (mpvi-get 'time-pos)))
            (when (and endp (null pos))
              (setq pos -1)) ; for remove loop-B
            (unless (numberp pos)
              (user-error "Wrong seek position"))
            (delete-region (plist-get node :begin) (plist-get node :end)))
        ;; when call the command directly, and A-B Loop is on, use the range
        (when in-place
          (pcase-let* ((`(,a ,b) (mpvi-get '(ab-loop-a ab-loop-b))))
            (unless (or (equal a "no") (equal b "no"))
              (setq beg a end b)))))
      (unless pos (setq desc (string-trim (read-string "Description: "))))
      (setq link (funcall mpvi-build-link-function
                          path
                          (if endp
                              (or beg (mpvi-get 'time-pos))
                            (or pos (mpvi-get 'time-pos)))
                          (if endp (unless (equal pos -1) (or pos end)) end)
                          (if (> (length desc) 0) desc)))
      (unless pos
        (end-of-line)
        (if (org-at-item-p) (org-insert-item) (insert "\n")))
      (insert link)
      (set-window-point (get-buffer-window) (point))
      (mpvi-pause paused))))

;;;###autoload
(defun mpvi-insert-screenshot ()
  "Screenshot and insert as attach link."
  (interactive nil org-mode)
  (with-current-buffer (mpvi-latest-org-buffer)
    (unless (derived-mode-p 'org-mode)
      (user-error "This is not org-mode, should not insert org link"))
    (unless (looking-back "[\n\r] *" (1- (line-beginning-position)))
      (end-of-line) (insert "\n"))
    (mpvi-insert-attach-link (mpvi-screenshot t)))
  (let ((msg "Capture and insert done."))
    (if mpvi-seek-actived (throw 'mpvi-seek msg) (message msg))))


;;; Interactive Commands

;;;###autoload
(defun mpvi-play (&optional playlist)
  "Play video with MPV player.
Interactively choose a file. Prefer the file or URL at point.
If PLAYLIST, only add the video into playlist of MPV player."
  (interactive)
  (let* ((prompt (if playlist "Add to playlist (file or url) " "Play video (file or url): "))
         (path (mpvi-read-file-or-url prompt (mpvi-emms-video-regex))))
    (unless (and (> (length path) 0) (or (mpvi-url-p path) (file-exists-p path)))
      (user-error "Not valid file or url"))
    (setq path (if (mpvi-url-p path) path (expand-file-name path)))
    (mpvi-start path (if playlist 'playlist))))

;;;###autoload
(defun mpvi-add-playlist ()
  "Add PATH to playlist of MPV."
  (interactive)
  (mpvi-play 'playlist))

;;;###autoload
(defun mpvi-add-emms (path)
  "Add PATH to playlist of EMMS."
  (interactive (list (mpvi-read-file-or-url "Add to EMMS (file or url): " (mpvi-emms-video-regex))))
  (unless (and (> (length path) 0) (or (mpvi-url-p path) (file-exists-p path)))
    (user-error "Not valid file or URL: %s" path))
  (cl-loop with info = (mpvi-extract nil path)
           with entries = (if (plist-get info :choices)
                              (list (list path (plist-get info :title) (plist-get info :up)))
                            (if-let* ((items (plist-get info :items)))
                                (cl-loop for item in items
                                         collect (list (plist-get item :path) (plist-get item :title) (plist-get item :up)))
                              (if (plist-get info :path)
                                  (list (list (plist-get info :path) (plist-get info :title) (plist-get info :up)))
                                (user-error "Unkown source: %s" path))))
           for i from 0
           for (filename title artist) in entries
           for track = (emms-track 'mpvi filename)
           if (and (not current-prefix-arg) ; fetch title and artist if necessary
                   (null title)
                   (string-prefix-p "http" filename))
           do (let* ((meta (ignore-errors (mpvi-extract nil filename)))
                     (t1 (plist-get meta :title))
                     (a1 (plist-get meta :up)))
                (if t1 (setq title t1))
                (if a1 (setq artist a1)))
           if title do (emms-track-set track 'info-title title)
           if artist do (emms-track-set track 'info-artist artist)
           do (with-current-emms-playlist
                (unless (= (line-beginning-position) (point))
                  (forward-line))
                (when (zerop i)
                  (switch-to-buffer (current-buffer)))
                (emms-playlist-insert-track track)
                (redisplay))
           finally (message "Add to EMMS success (total %d)." (length entries))))

;;;###autoload
(defun mpvi-next ()
  "Switch in playlist for EMMS or MPV player."
  (interactive)
  (if-let* ((current (and (not current-prefix-arg) (emms-playlist-current-selected-track))))
      ;; switch track of EMMS
      (with-current-emms-playlist
        (let ((track-indices nil))
          (save-excursion
            (goto-char (point-min))
            (emms-walk-tracks
	          (setq track-indices (append track-indices
                                          (let* ((track (emms-playlist-track-at (point)))
                                                 (desc (emms-track-description track))
                                                 (name (emms-track-name track)))
                                            (list (list (propertize name 'display desc) (point) desc)))))))
          (let* ((pos (marker-position (with-current-emms-playlist emms-playlist-selected-marker)))
                 (idx (cl-position-if (lambda (c) (equal (cadr c) pos)) track-indices))
                 (items (append (cl-subseq track-indices idx) (cl-subseq track-indices 0 idx)))
                 (item (completing-read "Play from EMMS: "
                                        (lambda (input pred action)
                                          (if (eq action 'metadata)
                                              `(metadata
                                                (display-sort-function . ,#'identity)
                                                (annotation-function
                                                 . ,(lambda (c)
                                                      (let ((ci (assoc c track-indices)))
                                                        (unless (string-match-p "/" (caddr ci))
                                                          (concat (make-string (max 2 (- 50 (string-width (caddr ci)))) ? )
                                                                  (substring-no-properties (car ci))))))))
                                            (complete-with-action action items input pred)))
                                        nil t)))
            (emms-playlist-select (cadr (assoc item track-indices)))
            (emms-player-start (emms-playlist-current-selected-track)))))
    ;; switch playlist of MPV player
    (let* ((playlist (mpvi-get 'playlist))
           (items (cl-loop with idx = 0
                           for i from 1 for p across playlist
                           for file = (alist-get 'filename p)
                           for title = (or (alist-get 'title p)
                                           (unless (string-prefix-p "http" file)
                                             (file-name-sans-extension (file-name-nondirectory file))))
                           for s = (propertize (format "%d:%s:%s" i file (or title ""))
                                               'display (concat (propertize (format "%s " i) 'face 'font-lock-comment-face)
                                                                (or title file)))
                           if (alist-get 'current p) do (setq idx (1- i))
                           collect (list s file title) into rs
                           finally (return (append (cl-subseq rs idx) (cl-subseq rs 0 idx)))))
           (cand (completing-read "Play from playlist: "
                                  (lambda (input pred action)
                                    (if (eq action 'metadata)
                                        `(metadata
                                          (display-sort-function . ,#'identity)
                                          (annotation-function
                                           . ,(lambda (c)
                                                (let ((ci (assoc c items)))
                                                  (when (caddr ci)
                                                    (concat (make-string (max 2 (- 60 (string-width (caddr ci)))) ? )
                                                            (substring-no-properties (cadr ci))))))))
                                      (complete-with-action action items input pred)))
                                  nil t))
           (index (or (cl-position-if
                       (lambda (c) (equal (alist-get 'filename c) (cadr (assoc cand items))))
                       playlist)
                      (user-error "Invalid index for current playlist"))))
      (mpvi-async-cmd `(batch
                        ((playlist-play-index ,index)
                         . ,(lambda (_ err)
                              (if err (user-error "%s" err)
                                (message "Switch to `%s'" (mpvi-get 'path)))))
                        ((set_property pause no)))))))

;;;###autoload
(defun mpvi-export (path &optional target beg end)
  "Cut or convert video for PATH from BEG to END, save to TARGET."
  (interactive
   (let (node path)
     (cond ((and (not current-prefix-arg)
                 (setq node (ignore-errors (mpvi-parse-link-at-point))))
            (setq path (plist-get node :path))
            (if (or (mpvi-url-p path) (file-exists-p path))
                (list path
                      (unless (mpvi-url-p path) (mpvi-read-path "Save to: " path))
                      (plist-get node :vbeg) (plist-get node :vend))
              (user-error "File not found: %s" path)))
           ((and (not current-prefix-arg)
                 (setq path (ignore-errors (mpvi-get 'path))))
            (list path))
           (t
            (setq path (mpvi-read-file-or-url "Clip video (file or url): " (mpvi-emms-video-regex)))
            (list path (unless (mpvi-url-p path) (mpvi-read-path "Save to: " path)))))))
  (funcall (if (mpvi-url-p path) mpvi-remote-video-handler mpvi-local-video-handler)
           path target beg end)
  (if mpvi-seek-actived (throw 'mpvi-seek "Export success.")))

;;;###autoload
(defun mpvi-browse (path)
  "Open current playing video PATH with system program."
  (interactive (list (if (and (not current-prefix-arg) (emms-player-mpv-proc-playing-p))
                         (mpvi-origin-path)
                       (mpvi-read-file-or-url "File or url: " (mpvi-emms-video-regex)))))
  (unless (and (stringp path) (mpvi-url-p path))
    (user-error "This should be a remote video"))
  (if (or (not mpvi-seek-actived)
          (y-or-n-p (format "Open '%s' externally?" path)))
      (let ((msg "Open in system program done."))
        ;; add begin time for url if necessary
        (when-let* ((fn (plist-get mpvi-current-playing :out-url-decorator)))
          (setq path (funcall fn path (mpvi-get 'time-pos))))
        (browse-url path)
        (if mpvi-seek-actived
            (throw 'mpvi-seek msg)
          (mpvi-pause t)
          (message msg)))
    (message "")))

;;; Control Panel

(defvar mpvi-control-buffer "*mpvi-control*")

(defvar mpvi-control-display-action
  `((display-buffer-below-selected)
    (window-height . 7)))

(defvar mpvi-control--timer nil)

(defvar mpvi-control--last-data nil)

(defvar mpvi-control--last-update-time nil)

(defvar mpvi-control--refreshing nil)

(defmacro mpvi-fn1 (form &optional revert-p)
  "Helper to build interactive command with FORM.
When REVERT-P is t, try to revert the input for seek."
  `(lambda ()
     (interactive)
     (prog1 ,form ,(if revert-p `(mpvi-revert-seek)))))

(defvar mpvi-control-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'mpvi-pause)
    (define-key map (kbd "m")   #'mpvi-toggle-mute)
    (define-key map (kbd "M")   #'mpvi-toggle-video)
    (define-key map (kbd "f")   #'mpvi-toggle-fullscreen)
    (define-key map (kbd "T")   #'mpvi-toggle-ontop)
    (define-key map (kbd "i")   (lambda ()
                                  (interactive)
                                  (if (and (minibufferp) (not (string-match-p "^Seek" (minibuffer-prompt))))
                                      (call-interactively #'exit-minibuffer)
                                    (call-interactively #'mpvi-insert))))
    (define-key map (kbd "I")   (lambda ()
                                  (interactive)
                                  (if (and (minibufferp) (not (string-match-p "^Seek" (minibuffer-prompt))))
                                      (call-interactively #'exit-minibuffer)
                                    (mpvi-insert t))))
    (define-key map (kbd "C-i") #'mpvi-insert-screenshot)
    (define-key map (kbd "n")   (mpvi-fn1 (mpvi-time "+1s") t))
    (define-key map (kbd "p")   (mpvi-fn1 (mpvi-time "-1s") t))
    (define-key map (kbd "N")   (mpvi-fn1 (mpvi-time "+5s") t))
    (define-key map (kbd "P")   (mpvi-fn1 (mpvi-time "-5s") t))
    (define-key map (kbd "M-n") (mpvi-fn1 (mpvi-time "+1%") t))
    (define-key map (kbd "M-p") (mpvi-fn1 (mpvi-time "-1%") t))
    (define-key map (kbd "C-n") (mpvi-fn1 (mpvi-time "+1s") t))
    (define-key map (kbd "C-p") (mpvi-fn1 (mpvi-time "-1s") t))
    (define-key map (kbd ".")   (mpvi-fn1 (mpvi-time "+1f") t))
    (define-key map (kbd ",")   (mpvi-fn1 (mpvi-time "-1f") t))
    (define-key map (kbd ">")   (mpvi-fn1 (mpvi-time "+1f") t))
    (define-key map (kbd "<")   (mpvi-fn1 (mpvi-time "-1f") t))
    (define-key map (kbd "C-l") (mpvi-fn1 (mpvi-time "-0s") t))
    (define-key map [left]      (mpvi-fn1 (mpvi-time "-5s") t))
    (define-key map [right]     (mpvi-fn1 (mpvi-time "+5s") t))
    (define-key map (kbd "0")   (mpvi-fn1 (mpvi-volume "+10") t))
    (define-key map (kbd "9")   (mpvi-fn1 (mpvi-volume "-10") t))
    (define-key map [up]        (mpvi-fn1 (mpvi-volume "+10") t))
    (define-key map [down]      (mpvi-fn1 (mpvi-volume "-10") t))
    (define-key map (kbd "M-<") (mpvi-fn1 (mpvi-revert-seek 0)))
    (define-key map (kbd "l")   (mpvi-fn1 (mpvi-speed)))
    (define-key map (kbd "j")   (mpvi-fn1 (mpvi-speed "-1.1x")))
    (define-key map (kbd "k")   (mpvi-fn1 (mpvi-speed "+1.1x")))
    (define-key map (kbd "J")   (mpvi-fn1 (mpvi-speed "-0.5")))
    (define-key map (kbd "K")   (mpvi-fn1 (mpvi-speed "+0.5")))
    (define-key map (kbd "[")   (mpvi-fn1 (mpvi-speed "0.9x")))
    (define-key map (kbd "]")   (mpvi-fn1 (mpvi-speed "1.1x")))
    (define-key map (kbd "{")   (mpvi-fn1 (mpvi-speed "0.5x")))
    (define-key map (kbd "}")   (mpvi-fn1 (mpvi-speed "2.0x")))
    (define-key map [backspace] (mpvi-fn1 (mpvi-speed)))
    (define-key map (kbd "s")   (mpvi-fn1 (mpvi-screenshot t 'interact)))
    (define-key map (kbd "C-s") (mpvi-fn1 (mpvi-screenshot t t)))
    (define-key map (kbd "`")   #'mpvi-cmd)
    (define-key map (kbd "x")   #'mpvi-set)
    (define-key map (kbd "c")   #'mpvi-next)
    (define-key map (kbd "v")   #'mpvi-toggle-subtitle)
    (define-key map (kbd "L")   #'mpvi-ab-loop)
    (define-key map (kbd "e")   #'mpvi-export)
    (define-key map (kbd "o")   #'mpvi-browse)
    (define-key map (kbd "r")   #'mpvi-capture-ocr)
    (define-key map (kbd "t")   #'mpvi-capture-subtitle)
    (define-key map (kbd "z")   #'mpvi-delay-subtitle)
    (define-key map (kbd "C-t") #'mpvi-load-subtitle)
    (define-key map (kbd "g")   #'mpvi-seek)
    (define-key map (kbd "/")   #'mpvi-seek)
    (define-key map (kbd "w")   #'mpvi-geofit)
    (define-key map (kbd "q")   #'mpvi-control-quit)
    (define-key map (kbd "C-q") (lambda () (interactive) (emms-stop) (mpvi-control-quit)))
    (define-key map (kbd "h")   #'mpvi-control-tips)
    map)
  "Keymap for `mpvi-control-mode'.")

(define-derived-mode mpvi-control-mode special-mode "MPVi-Control"
  "Major mode for MPV control panel."
  :interactive nil
  (setq buffer-read-only t cursor-type nil)
  (add-hook 'window-configuration-change-hook #'mpvi-control-ob-playback-time)
  (add-hook 'kill-buffer-hook
            (lambda () (remove-hook 'window-configuration-change-hook #'mpvi-control-ob-playback-time))
            nil t)
  (use-local-map mpvi-control-map))

(defun mpvi-control-ob-playback-time ()
  "Observe playback time only when control panel is visible."
  (when (emms-player-mpv-proc-playing-p)
    (if (and (buffer-live-p (get-buffer mpvi-control-buffer))
             (get-buffer-window mpvi-control-buffer))
        (progn
          (mpvi-async-cmd '(observe_property 10086 playback-time))
          (mpvi-async-cmd '(observe_property 10087 duration)))
      (mpvi-async-cmd '(unobserve_property 10086))
      (mpvi-async-cmd '(unobserve_property 10087)))))

(defun mpvi-control-tips ()
  "Command tips for current seek."
  (interactive)
  (let ((tips '(("SPC m M T f w L" . "Pause/Mute/Video/Ontop/Fullscreen/Window/AB-Loop")
                ("n p N P M-n M-p C-l < >" . "Forward/Backward")
                ("/ g" . "Interactively Seek")
                ("9 0 up down" . "Tune volume")
                ("j k l [ ] { } Backspace" . "Tune Speed")
                ("s C-s" . "Screenshot")
                ("v z t C-t" . "Subtitle")
                ("r o c e" . "OCR/Browser/Playlist/Export")
                ("` x" . "Interactively Cmd/SetProp")
                ("i I C-i" . "Video Notes (timestamp link/screenshot)"))))
    (prog1 tips
      (let ((s (mapconcat (lambda (tip)
                            (concat (propertize (car tip) 'face 'bold) ": " (cdr tip)))
                          tips "\n")))
        (message "------------") (message s)))))

(cl-defgeneric mpvi-control-render-header (&optional path media-title warning ww)
  "Setup playback header for the control panel.
PATH, MEDIA-TITLE are status of MPV player.
WARNING for error message.
WW is window width of current buffer."
  (let* ((title (if warning
                    (propertize warning 'face 'warning)
                  (if (stringp media-title)
                      (propertize media-title 'help-echo path)
                    path)))
         (ww (or ww (window-width (get-buffer-window (current-buffer)))))
         (tip (if (> ww 90) (concat "press " (propertize "h" 'face 'warning) " for help")))
         (header (concat (propertize " " 'display '(space :width 0.6)) title
                         (if tip (propertize " " 'display `((space :align-to (- right ,(length tip)))))) tip)))
    (setq header-line-format header)
    (force-mode-line-update)
    (redisplay t)))

(cl-defgeneric mpvi-control-render-body (&key playback-time duration speed volume mute video pause ab-loop-a ab-loop-b ww &allow-other-keys)
  "Insert playback status for the control panel.
PLAYBACK-TIME, DURATION, SPEED, VOLUME, MUTE, VIDEO, PAUSE, AB-LOOP-A and
AB-LOOP-B are status of MPV player. WW is window width."
  (let* ((width (min (max 30 (- ww 15)) 60))
         (percent (min 1 (/ playback-time duration)))
         (elapsed (if (> duration 0) (round (* width percent)) 0))
         (remained (- width elapsed))
         (bar (concat (apply #'propertize
                             (format "%s [%s%s] %s"
                                     (mpvi-secs-to-hms playback-time nil t)
                                     (make-string (max 1 elapsed) (if pause ?▬ ?■))
                                     (make-string (max 0 remained) ?-)
                                     (mpvi-secs-to-hms (- duration playback-time) nil t))
                             'pointer 'hand
                             (when (eq (window-buffer) (current-buffer))
                               '(face font-lock-keyword-face)))))
         (prop (lambda (k v r)
                 (concat "[" k "]" (if v " ")
                         (if v (propertize (format "%s" v) 'face 'font-lock-variable-use-face)) r)))
         (rest (concat (funcall prop
                                (if pause "⏸" (if video "Video" "Audio"))
                                (format "%.1f/%.1f, %.1f%%" playback-time duration (* 100 percent)) " ")
                       (funcall prop (if mute (propertize "Volume" 'face '(:strike-through t)) "Volume") volume " ")
                       (funcall prop "Speed" speed " ")
                       (if-let* ((loop (let ((a ab-loop-a) (b ab-loop-b))
                                         (if (equal a "no") (setq a nil))
                                         (if (equal b "no") (setq b nil))
                                         (when (or a b) (cons a b)))))
                           (funcall prop "Loop" loop " ")))))
    (erase-buffer)
    (insert (propertize " " 'display (concat "\n" bar "\n\n" rest)))))

(defun mpvi-control-refresh (&optional alist)
  "Refresh the control panel with current playback info.
If ALIST exists, only refresh the property data in it."
  (when (and (null mpvi-control--refreshing)
             (or (> (- (float-time) (or mpvi-control--last-update-time 0)) 0.1)
                 (not (and (= (length alist) 1) (eq (caar alist) 'playback-time)))))
    (setq mpvi-control--refreshing t)
    (condition-case err
        (if-let* ((buf (get-buffer mpvi-control-buffer))
                  (win (and buf (buffer-live-p buf) (get-buffer-window buf))))
            (if (emms-player-mpv-proc-playing-p)
                (with-current-buffer buf
                  (let* ((f1 (lambda (k) (if-let* ((v (assoc k alist))) (cdr v)
                                           (or (alist-get k mpvi-control--last-data)
                                               (mpvi-get k t)))))
                         (fm (lambda (ks)
                               (cl-loop with dirty = nil
                                        for k in ks
                                        for v = (cond
                                                 ((eq k 'ww) (window-width win))
                                                 ((eq k 'focus) (eq (selected-window) win))
                                                 ((eq k 'warning) (alist-get 'warning alist))
                                                 (t (funcall f1 k)))
                                        do (unless (equal v (alist-get k mpvi-control--last-data))
                                             (setf (alist-get k mpvi-control--last-data) v)
                                             (setq dirty t))
                                        collect v into vs
                                        finally (return (if dirty vs))))))
                    (when-let* ((vals (funcall fm '(path media-title warning ww))))
                      (apply #'mpvi-control-render-header vals))
                    (when-let* ((keys '(playback-time duration speed volume mute video pause ab-loop-a ab-loop-b ww focus))
                                (vals (funcall fm keys))
                                (ps (cl-loop for k in keys for v in vals
                                             collect (intern (format ":%s" k))
                                             collect (unless (eq v :json-false) v))))
                      (let ((inhibit-read-only t))
                        (apply #'mpvi-control-render-body ps)
                        (set-buffer-modified-p nil)))
                    (setq mpvi-control--last-update-time (float-time))
                    (setq mpvi-control--refreshing nil)))
              (with-current-buffer buf
                (unless (string-match-p "Error" header-line-format)
                  (mpvi-control-render-header nil nil "Waiting..."))
                (setq mpvi-control--last-data nil)
                (setq mpvi-control--refreshing nil)))
          (mpvi-control-quit))
      ((debug error)
       (mpvi-control-quit (format "Error: %s" (cdr err)))))))

;;;###autoload
(defun mpvi-control ()
  "Popup a panel to control MPV player."
  (interactive)
  (if (and (buffer-live-p (get-buffer mpvi-control-buffer))
           (get-buffer-window mpvi-control-buffer))
      (mpvi-control-quit)
    (mpvi-check-live)
    (unless (buffer-live-p (get-buffer mpvi-control-buffer))
      (with-current-buffer (get-buffer-create mpvi-control-buffer)
        (setq mpvi-control--last-data nil)
        (mpvi-control-mode)))
    (when (buffer-live-p (get-buffer mpvi-control-buffer))
      (pop-to-buffer mpvi-control-buffer mpvi-control-display-action)
      (redisplay t)
      (mpvi-control-refresh))))

(defun mpvi-control-quit (&optional warning)
  "Exit the control panel or raise WARNING."
  (interactive)
  (setq mpvi-control--last-data nil)
  (setq mpvi-control--refreshing nil)
  (let ((buf (get-buffer mpvi-control-buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (if warning
            (mpvi-control-refresh `((warning . ,warning)))
          (when-let* ((win (get-buffer-window buf)))
            (delete-window win))
          (kill-buffer buf))))))

(cl-defmethod mpvi-event :around (type data)
  "Enhance event handlers for event TYPE and its DATA."
  (ignore-errors
    (when (and (eq type 'end-file) (equal (alist-get 'reason data) "quit"))
      (setq emms-player-mpv-ipc-stop-command nil)
      (sleep-for 0.3)) ; wait for a moment, avoid restart error
    (when (memq type '(video-reconfig playback-restart file-loaded end-file))
      (setq mpvi-control--last-data nil))
    (when (eq type 'start-file)
      (mapc #'emms-player-mpv-observe-property
            '(speed volume ab-loop-a ab-loop-b mute video pause ontop geometry autofit)))
    (when (eq type 'property-change)
      (let ((val (alist-get 'data data)))
        (pcase (intern (alist-get 'name data))
          ('mute (setq mpvi-mpv-mute-p val))
          ('ontop (setq mpvi-mpv-ontop-p val))
          ('autofit (setq mpvi-mpv-autofit val))
          ('geometry (setq mpvi-mpv-geometry val)))))
    (when (and (buffer-live-p (get-buffer mpvi-control-buffer))
               (get-buffer-window mpvi-control-buffer))
      (cond ((eq type 'property-change)
             (mpvi-control-refresh `((,(intern (alist-get 'name data)) . ,(alist-get 'data data)))))
            ((memq type '(video-reconfig playback-restart))
             (mpvi-control-refresh)))))
  (cl-call-next-method type data))

;;; Interactive Seek

(defvar mpvi-seek-overlay nil)

(defvar mpvi-seek-refresh-timer nil)

(defvar mpvi-seek-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (make-composed-keymap (list minibuffer-mode-map mpvi-control-map)))
    (mapc (lambda (k) (define-key map k nil))
          (list [left] [right] [up] [down] [backspace] "0" "9" "." ","))
    (define-key map (kbd "q")   #'abort-minibuffers)
    (define-key map (kbd "C-q") #'abort-minibuffers)
    map))

(defun mpvi-seek-refresh ()
  "Show information of the current playing in minibuffer."
  (when (minibufferp)
    (ignore-errors (cancel-timer mpvi-seek-refresh-timer))
    (if mpvi-seek-overlay (delete-overlay mpvi-seek-overlay))
    (let ((vf (lambda (s) (if s (propertize (format "%s" s) 'face mpvi-annotation-face))))
          (sf (lambda (s) (propertize " " 'display `(space :align-to (- right-fringe ,(1+ (length s))))))) ; space
          (ov (make-overlay (point-max) (point-max) nil t t)))
      (overlay-put ov 'intangible t)
      (setq mpvi-seek-overlay ov)
      (if (mpvi-seekable)
          (condition-case nil
              (pcase-let* ((`(,loop ,pause ,time-pos ,duration ,percent-pos ,speed)
                            (mpvi-get '(loop pause time-pos duration percent-pos speed)))
                           (loop (if loop (funcall vf "[Looping] ")))
                           (pause (if pause (funcall vf "[Paused] ")))
                           (time (funcall vf (mpvi-secs-to-hms time-pos nil t)))
                           (total (funcall vf (mpvi-secs-to-hms duration nil t)))
                           (percent (funcall vf (format "%.1f%%" percent-pos)))
                           (speed (funcall vf (format "%.2f" speed)))
                           (concated (concat loop (if loop " ") pause (if pause " ")
                                             time "/" total "  " percent "  Speed: " speed))
                           (space (funcall sf concated)))
                (overlay-put ov 'before-string (propertize (concat space concated) 'cursor t))
                (setq mpvi-seek-refresh-timer (run-with-timer 0.5 nil #'mpvi-seek-refresh)))
            (error nil))
        (let* ((title (funcall vf (concat "        >> " (string-trim (or (mpvi-get 'media-title) "")))))
               (state (funcall vf (if (mpvi-get 'pause) "Paused")))
               (space (funcall sf state)))
          (delete-minibuffer-contents)
          (insert "0")
          (overlay-put ov 'before-string (propertize (concat title space state) 'cursor t)))))))

;;;###autoload
(defun mpvi-seek (&optional pos prompt)
  "Interactively seek POS for current playing video.
PROMPT is used if non-nil for `minibuffer-prompt'."
  (interactive)
  (unless mpvi-seek-actived
    (mpvi-seekable 'assert)
    (let ((paused (mpvi-get 'pause))
          (target-depth nil))
      (unwind-protect
          (let* ((enable-recursive-minibuffers t)
                 (ret (catch 'mpvi-seek
                        (minibuffer-with-setup-hook
                            (lambda ()
                              (unless target-depth ; Notice: setup-hook can affect minibuffers in any depth
                                (setq target-depth (minibuffer-depth)))
                              (when (eq (minibuffer-depth) target-depth)
                                (setq mpvi-seek-actived t)
                                (setq mpvi-seek-paused nil)
                                (mpvi-pause t)
                                (ignore-errors (cancel-timer mpvi-seek-refresh-timer))
                                (let ((control-panel (get-buffer mpvi-control-buffer)))
                                  (unless (and (buffer-live-p control-panel) (get-buffer-window control-panel))
                                    (add-hook 'post-command-hook
                                              (lambda ()
                                                (unless (memq this-command '(self-insert-command))
                                                  (mpvi-seek-refresh)))
                                              nil t)
                                    (setq mpvi-seek-refresh-timer (run-with-timer 1 nil #'mpvi-seek-refresh))))
                                (add-hook 'after-change-functions
                                          (lambda (start end _)
                                            (let ((text (minibuffer-contents)))
                                              (unless (or (string-match-p "^[0-9]+\\.?[0-9]*$" text) ; 23.3
                                                          (string-match-p "^[0-9]\\{1,2\\}\\(\\.[0-9]*\\)?%$" text) ; 23%
                                                          (string-match-p ; 1:23:32
                                                           "^\\([0-9]+:\\)?\\([0-9]\\{1,2\\}\\):\\([0-9]\\{1,2\\}\\)?$" text))
                                                (delete-region start end))))
                                          nil t)
                                (add-hook 'minibuffer-exit-hook
                                          (lambda ()
                                            (when (and mpvi-seek-actived (eq (minibuffer-depth) target-depth))
                                              (ignore-errors (cancel-timer mpvi-seek-refresh-timer))
                                              (setq mpvi-seek-refresh-timer nil)
                                              (setq mpvi-seek-actived nil))))))
                          (ignore-errors ; case for empty string
                            (read-from-minibuffer
                             (or prompt (if (mpvi-seekable)
                                            (format "Seek ([0,%d] mm:ss,n,n%%): " (mpvi-get 'duration))
                                          "MPV Controller: "))
                             (format "%.1f" (or pos (mpvi-get 'time-pos)))
                             mpvi-seek-map t 'mpvi-seek-hist))))))
            (pcase ret
              ('nil (ignore))
              ((pred stringp) (message "%s" ret))
              ((pred numberp) (mpvi-set 'time-pos ret))
              ((pred symbolp) (mpvi-set 'time-pos (mpvi-time-to-secs (format "%s" ret) (mpvi-get 'duration))))))
        (mpvi-pause (or mpvi-seek-paused paused))))))

(defun mpvi-revert-seek (&optional num)
  "Insert current time-pos to minibuffer.
If NUM is not nil, go back that position first."
  (interactive)
  (when (and mpvi-seek-actived (minibufferp))
    (when (and num (mpvi-seekable))
      (mpvi-set 'time-pos num))
    (delete-minibuffer-contents)
    (insert (mpvi-secs-to-string (mpvi-get 'time-pos)))))

;;; Org Link

(defvar mpvi-org-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ", ,")   #'org-open-at-point)
    (define-key map (kbd ", s")   #'mpvi-seek)
    (define-key map (kbd ", a")   #'mpvi-insert)
    (define-key map (kbd ", b")   (lambda () (interactive) (mpvi-insert 'end)))
    (define-key map (kbd ", v")   #'mpvi-current-link-show-preview)
    (define-key map (kbd ", e")   #'mpvi-export)
    (define-key map (kbd ", SPC") #'mpvi-pause)
    (define-key map (kbd ", h")   #'mpvi-current-link-short-help)
    map))

(defvar mpvi-org-link-face '(:inherit org-link :underline nil :box (:style flat-button)))

(defun mpvi-org-link-push (link)
  "Play the mpv LINK."
  (pcase-let ((`(,path ,beg ,end) (mpvi-parse-link link)))
    (mpvi-start path beg end)))

(defcustom mpvi-org-https-link-rules nil
  "Rules to check if current https link should be opened with MPV.
One rule is a regexp string to check against link url."
  :type '(repeat string))

(defun mpvi-org-https-link-push (url arg)
  "Play the normal https URL with MPV if it matches any of the rules.
ARG is the argument."
  (if (cl-find-if (lambda (r) (string-match-p r url)) mpvi-org-https-link-rules)
      (mpvi-start (concat "https:" url))
    (browse-url (concat "https:" url) arg)))

(defun mpvi-current-link-update-end-pos ()
  "Update the end position on this link."
  (interactive nil org-mode)
  (mpvi-with-current-mpv-link (node)
    (when node
      (let ((ret (mpvi-seek (or (plist-get node :vend)
                                (max (plist-get node :vbeg) (mpvi-get 'time-pos)))
                            (format "Set end position (%d-%d): " (plist-get node :vbeg) (mpvi-get 'duration)))))
        (delete-region (plist-get node :begin) (plist-get node :end))
        (let ((link (funcall mpvi-build-link-function
                             (plist-get node :path)
                             (plist-get node :vbeg)
                             (car ret))))
          (save-excursion (insert link)))))))

(defvar x-gtk-use-system-tooltips)

(defun mpvi-current-link-show-preview ()
  "Show the preview tooltip for this link."
  (interactive nil org-mode)
  (when-let* ((node (mpvi-parse-link-at-point)))
    (let* ((scr (funcall mpvi-screenshot-function (plist-get node :path) (plist-get node :vbeg)))
           (img (create-image scr nil nil :width 400))
           (help (propertize " " 'display img))
           (x-gtk-use-system-tooltips nil))
      (tooltip-show help))))

(defun mpvi-current-link-short-help ()
  "Command tips for current link."
  (interactive nil org-mode)
  (let ((tips '((",s"   . "Seek")
                (",a"   . "StampStart")
                (",b"   . "StampEnd")
                (",e"   . "Export")
                (",v"   . "Preview")
                (",,"   . "Play")
                (",SPC" . "Pause"))))
    (message (mapconcat (lambda (tip)
                          (concat (propertize (car tip) 'face 'font-lock-keyword-face)
                                  "/" (cdr tip)))
                        tips "  "))))

;;;###autoload
(defun mpvi-org-link-init ()
  "Setup org link with `mpv' prefix."
  (set-keymap-parent mpvi-org-link-map org-mouse-map)
  (org-link-set-parameters "mpv"
                           :face mpvi-org-link-face
                           :keymap mpvi-org-link-map
                           :follow #'mpvi-org-link-push)
  (org-link-set-parameters "https"
                           :follow #'mpvi-org-https-link-push))

;;;###autoload
(eval-after-load 'org '(mpvi-org-link-init))


;;; Miscellaneous

(require 'mpvi-subtitle)
(require 'mpvi-living)
(require 'mpvi-bilibili)
(require 'mpvi-websocket)

(mpvi-emms-integrated-mode t)

(provide 'mpvi)

;;;###autoload
(eval-after-load 'emms '(require 'mpvi))

;;; mpvi.el ends here
