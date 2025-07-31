;;; mpvi.el --- Watch video and take interactive video notes -*- lexical-binding: t -*-

;; Copyright (C) 2023 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; URL: https://github.com/lorniu/mpvi
;; Package-Requires: ((emacs "28.1") (emms "11"))
;; Keywords: convenience, docs, multimedia, application
;; SPDX-License-Identifier: MIT
;; Version: 1.2

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
;;   - Use `mpvi-open' to open a video or audio
;;   - Use `mpvi-control' or `mpvi-seek' to operate the playing video
;;   - Take interactive video notes with command `mpvi-insert'
;;
;; You can also control MPV that is opened by `emms'.

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

(defvar mpvi-play-history nil)

(defvar mpvi-last-save-directory nil)

(defvar mpvi-seek--actived nil
  "Whether `mpvi-seek' is active.")

(defvar mpvi-seek--paused nil
  "Used in interactive seek. Value nil as unset, and yes or no for pause state.")

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

(defun mpvi-log (fmt &rest args)
  "Output log when `emms-player-mpv-debug' not nil.
FMT and ARGS are like arguments in `message'."
  (when emms-player-mpv-debug
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
                   (if (file-name-absolute-p default) default
                     (expand-file-name default dir)))))
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

(defun mpvi-read-file-or-url (&optional prompt map)
  "Read a file name or URL from minibuffer.
Optional PROMPT specifies the prompt string and MAP is used to define extra keys
for current minibuffer."
  (minibuffer-with-setup-hook
      (lambda ()
        (when map
          (use-local-map (make-composed-keymap (list (current-local-map) map)))))
    (let ((file-name-history mpvi-play-history))
      (unwind-protect
          (catch 'ffap-prompter
            (ffap-read-file-or-url
             (or prompt "File or url: ")
             (prog1 (mpvi-ffap-guesser) (ffap-highlight))))
        (setq mpvi-play-history file-name-history)
        (ffap-highlight t)))))

(defun mpvi-call-process (program &rest args)
  "Helper for `call-process', PROGRAM and ARGS are the same."
  (mpvi-log ">>> %s %s" program
            (mapconcat (lambda (a) (shell-quote-argument a)) args " "))
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
    (if (re-search-forward "v\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9+]\\)" (line-end-position) t)
        (list (string-to-number (match-string 1))
              (string-to-number (match-string 2))
              (string-to-number (match-string 3)))
      (user-error "No mpv version found"))))

(defun mpvi-compare-mpv-version (comparefn version)
  "Compare current mpv verion with the special VERSION through COMPAREFN.
VERSION should be a list, like \\='(0 38 0) representing version 0.38.0."
  (let ((current (mpvi-mpv-version)))
    (funcall comparefn
             (+ (* (car current) 1000000) (* (cadr current) 1000) (caddr current))
             (+ (* (car version) 1000000) (* (cadr version) 1000) (caddr version)))))


;;; MPV communication

(defvar mpvi-current-url-metadata nil)

(cl-defgeneric mpvi-extract-url (type url &rest _)
  "Extract URL for different platforms.

Return a plist:
- :url/title/subfile for the real url, display media title and sub-file
- :opts/cmds for extra options for `loadfile', and commands executed after load
- :started for function executed after loaded
- :out-url-decorator for function to decorate url when open in external program
- others maybe used in anywhere else

TYPE should be keyword as :host format, for example :www.youtube.com,
if it's nil then this method will be a dispatcher."
  (:method (type url &rest args)
           (unless type ; the first call
             (let* ((typefn (lambda (url) (intern (concat ":" (url-host (url-generic-parse-url url))))))
                    (playlist (mpvi-extract-playlist (funcall typefn url) url)))
               (if (and playlist (null (car playlist))) ; when no selected-index, return all items in playlist
                   (list :playlist-url url :playlist-items (cdr playlist))
                 (let ((purl (if playlist (nth (car playlist) (cdr playlist)))) ret)
                   (if-let* ((dest (apply #'mpvi-extract-url  ; dispatch to method
                                          (funcall typefn (or purl url))
                                          (or purl url) args)))
                       (progn (setq ret dest)
                              (unless (plist-get ret :url)
                                (plist-put ret :url (or purl url))))
                     (setq ret (list :url (or purl url))))
                   (when playlist
                     (plist-put ret :playlist-url url)
                     (plist-put ret :playlist-index (car playlist))
                     (plist-put ret :playlist-items (cdr playlist)))
                   (unless (equal (plist-get ret :url) url)
                     (plist-put ret :origin-url url))
                   ret))))))

(cl-defgeneric mpvi-extract-playlist (type url &optional no-choose)
  "Check if URL is a playlist link. If it is, return the selected playlist-item.
TYPE is platform as the same as in `mpvi-extract-url'.
Don't prompt user to choose When NO-CHOOSE is not nil.
Return list of (index-or-title playlist-items)."
  (:method (_type url &optional no-choose)
           (let ((meta (mpvi-ytdlp-url-metadata url)))
             (when (assoc 'is_playlist meta)
               (let ((urls (cl-loop for item across (alist-get 'entries meta)
                                    collect (alist-get 'url item))))
                 (if no-choose
                     (cons (alist-get 'title meta) urls)
                   (let* ((items (cl-loop
                                  for url in urls for i from 1
                                  for item = (if (member url mpvi-play-history) (propertize url 'face mpvi-annotation-face) url)
                                  collect (propertize item 'line-prefix (propertize (format "%2d. " i) 'face mpvi-annotation-face))))
                          (item (completing-read
                                 (concat "Playlist" (if-let* ((title (alist-get 'title meta))) (format " (%s)" title))  ": ")
                                 (lambda (input pred action)
                                   (if (eq action 'metadata)
                                       `(metadata (display-sort-function . ,#'identity))
                                     (complete-with-action action items input pred)))
                                 nil t nil nil (car items))))
                     (cons (cl-position item urls :test #'string=) urls))))))))

(defun mpvi-check-live ()
  "Check if MPV is runing."
  (unless (emms-player-mpv-proc-playing-p)
    (user-error "No living MPV found"))
  (unless (mpvi-compare-mpv-version #'> '(0 16 999))
    (user-error "You should update MPV to support ipc connect")))

(defun mpvi-origin-path (&optional path)
  "Reverse of `mpvi-extract-url', return the origin url for PATH.
When PATH is nil then return the path of current playing video."
  (unless path
    (mpvi-check-live)
    (setq path (mpvi-cmd `(get_property path))))
  (or (plist-get mpvi-current-url-metadata :origin-url) path))

(defalias 'mpvi-async-cmd #'emms-player-mpv-cmd)

(defun mpvi-cmd (cmd)
  "Request MPV for CMD synchronously and return its response."
  (when (emms-player-mpv-proc-playing-p)
    (let ((result nil) (error nil) (done nil) (timeout 5.0) (start-time (float-time)))
      (mpvi-async-cmd cmd
                      (lambda (data err)
                        (setq result data error err done t)))
      (while (and (not done) (< (- (float-time) start-time) timeout))
        (accept-process-output emms-player-mpv-proc 0.01))
      (if done
          (if error
              (user-error "MPV command failed: %s" error)
            (unless (eq result :json-false) result))
        (user-error "MPV command timed out after %s seconds" timeout)))))

(cl-defun mpvi-prop (sym &optional (val nil supplied))
  "Run command set_property SYM VAL in MPV.
Run get_property instead if VAL is absent."
  (if supplied
      (progn (mpvi-async-cmd `(set_property ,sym ,val)) val)
    (mpvi-cmd `(get_property ,sym))))

(defcustom mpvi-cmds-on-init '(((set_property autofit-larger "80%")))
  "Command list to run after MPV process initialized.
See `emms-player-mpv-cmd' for syntax."
  :type '(repeat sexp))

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

(defcustom mpvi-cmds-on-play nil
  "Command list let MPV process run after loading a file.
See `emms-player-mpv-cmd' for syntax."
  :type '(repeat sexp))

(cl-defun mpvi-play (path &optional (beg 0) end emms noseek)
  "Play PATH from BEG to END.
EMMS is a flag that this is invoked from EMMS.
When NOSEEK is not nil then dont try to seek but open directly."
  (if (mpvi-url-p path)
      (unless (executable-find "yt-dlp")
        (user-error "You should have 'yt-dlp' installed to play remote url"))
    (setq path (expand-file-name path)))
  ;; I want to make it loop N times then pause when beg and end is provided.
  ;; But it seems this is not an easy thing for current MPV.
  ;; Give up and use ab-loop instead, it will loop forever until manually pause.
  ;; https://github.com/mpv-player/mpv/issues/13860
  (if (and (not noseek) (emms-player-mpv-proc-playing-p) (equal path (mpvi-origin-path)))
      ;; when path is current playing, just seek to position
      (when (mpvi-seekable)
        (mpvi-prop 'ab-loop-a (if end beg "no"))
        (mpvi-prop 'ab-loop-b (or end "no"))
        (mpvi-prop 'time-pos beg)
        (mpvi-prop 'pause 'no))
    ;; If path is not the current playing, load it
    (let ((living (emms-player-mpv-proc-playing-p))
          logo title subfile load-opts load-cmds started)
      (unless emms (message "Waiting %s..." path))
      (if living (ignore-errors (mpvi-pause t)))
      ;; preprocessing url and extra mpv commands
      (when (mpvi-url-p path)
        (when-let* ((ret (mpvi-extract-url nil path)))
          (unless (plist-get ret :url) (user-error "Unknown url"))
          (setq mpvi-current-url-metadata ret)
          (setq path (or (plist-get ret :url) path))
          (setq logo (plist-get ret :logo))
          (setq title (plist-get ret :title))
          (setq subfile (plist-get ret :subfile))
          (setq load-opts (plist-get ret :opts))
          (setq load-cmds (plist-get ret :cmds))
          (setq started (plist-get ret :started))))
      (setq load-opts
            `((start . ,beg)
              ,@(when end    `((ab-loop-a . ,beg) (ab-loop-b . ,end)))
              ,(when title   `(force-media-title . ,(format "\"%s\"" title)))
              ,(when subfile `(sub-file . ,(format "\"%s\"" subfile)))
              ,@load-opts))
      (mpvi-log "load opts: %S" load-opts)
      (let* ((init-cmds (unless (or living emms)
                          `(,@mpvi-cmds-on-init
                            ((set_property ontop ,(or mpvi-mpv-ontop-p :json-false)))
                            ((set_property mute ,(or mpvi-mpv-mute-p :json-false)))
                            ((set_property border ,(or mpvi-mpv-border-p :json-false)))
                            ((set_property title-bar ,(or mpvi-mpv-title-bar-p :json-false)))
                            ((set_property sub-visibility ,(or mpvi-mpv-subtitle-p :json-false))))))
             (load-opts (mapconcat (lambda (x)
                                     (format "%s=%s" (car x) (cdr x)))
                                   (delq nil load-opts) ","))
             (load-handler (lambda (_ err)
                             (if err
                                 (message "Load video failed (%S)" err)
                               (if started
                                   (funcall started)
                                 (message "%s"
                                          (if title
                                              (concat (if logo (concat "/" logo)) ": "
                                                      (propertize title 'face 'font-lock-keyword-face))
                                            ""))))))
             (post-cmds (cl-loop for c in (append mpvi-cmds-on-play load-cmds)
                                 if (car-safe (car c)) collect c
                                 else collect (list c)))
             (cmds (cons 'batch
                         (delq nil
                               `(,@init-cmds
                                 ((set_property speed 1))
                                 ((set_property keep-open yes))
                                 ((set_property pause no))
                                 ;; Since mpv 0.38.0, an insertion index argument is added as the third argument
                                 ;; https://mpv.io/manual/master/#command-interface, loadfile
                                 ((loadfile ,path replace
                                            ,@(if (ignore-errors (mpvi-compare-mpv-version #'< '(0 38 0)))
                                                  (list load-opts)
                                                (list -1 load-opts)))
                                  . ,load-handler)
                                 ,@post-cmds)))))
        (mpvi-log "load-commands: %S" cmds)
        (mpvi-async-cmd cmds)))))

(cl-defun mpvi-pause (&optional (how nil supplied))
  "Set or toggle pause state of MPV.
When HOW is SUPPLIED, explictly turn pause on or off.
Otherwise, toggle pause state."
  (interactive)
  (mpvi-check-live)
  (mpvi-cmd (if supplied
                (let ((how1 (pcase how ('t 'yes) ('nil 'no) (_ how))))
                  `(set pause ,how1))
              `(cycle pause)))
  (when mpvi-seek--actived
    (when current-prefix-arg ; with prefix, remain the state after exit minibuffer
      (setq mpvi-seek--paused (if (mpvi-prop 'pause) 'yes 'no)))
    (mpvi-revert-seek)))

(defun mpvi-toggle-fullscreen ()
  "Toggle fullscreen for mpv."
  (interactive)
  (mpvi-check-live)
  (mpvi-async-cmd
   `(cycle fullscreen)
   (lambda (&rest _)
     (if mpvi-seek--actived (throw 'mpvi-seek nil)))))

(defun mpvi-toggle-ontop ()
  "Toggle display on top for mpv."
  (interactive)
  (mpvi-check-live)
  (mpvi-async-cmd
   `(cycle ontop)
   (lambda (&rest _)
     (setq mpvi-mpv-ontop-p (mpvi-prop 'ontop))
     (message "On Top: %s" (if mpvi-mpv-ontop-p "enable" "canceled")))))

(defun mpvi-toggle-mute ()
  "Toggle mute for mpv."
  (interactive)
  (mpvi-check-live)
  (mpvi-async-cmd
   `(cycle mute)
   (lambda (&rest _)
     (setq mpvi-mpv-mute-p (mpvi-prop 'mute))
     (message "Muted: %s" (if mpvi-mpv-mute-p "on" "off")))))

(defun mpvi-toggle-video ()
  "Toggle display video for mpv."
  (interactive)
  (mpvi-check-live)
  (mpvi-async-cmd `(cycle video)))

(defun mpvi-toggle-border ()
  "Toggle display border for mpv."
  (interactive)
  (mpvi-check-live)
  (mpvi-async-cmd
   `(cycle border)
   (lambda (&rest _)
     (setq mpvi-mpv-border-p (mpvi-prop 'border)))))

(defun mpvi-toggle-title-bar ()
  "Toggle display title bar for mpv."
  (interactive)
  (mpvi-check-live)
  (mpvi-async-cmd
   `(cycle title-bar)
   (lambda (&rest _)
     (setq mpvi-mpv-title-bar-p (mpvi-prop 'title-bar)))))

(defun mpvi-toggle-subtitle ()
  "Toggle display subtitle for mpv."
  (interactive)
  (mpvi-check-live)
  (mpvi-async-cmd
   `(cycle sub-visibility)
   (lambda (&rest _)
     (setq mpvi-mpv-subtitle-p (mpvi-prop 'sub-visibility)))))

(defun mpvi-load-subtitle (subfile)
  "Load or reload the SUBFILE for current playing video."
  (interactive (list (progn
                       (mpvi-check-live)
                       (read-file-name
                        "Danmaku file: " (mpvi-cache-directory) nil t
                        (ignore-errors
                          (file-name-nondirectory
                           (file-name-sans-extension (aref (mpvi-prop 'sub-files) 0))))))))
  (mpvi-check-live)
  (cl-assert (file-regular-p subfile))
  (when (string-suffix-p ".danmaku.xml" subfile) ; bilibili
    (setq subfile (mpvi-convert-danmaku subfile 'confirm)))
  (ignore-errors (mpvi-async-cmd `(sub-remove)))
  (mpvi-async-cmd `(sub-add ,subfile))
  (let ((msg "Sub file loaded!"))
    (if mpvi-seek--actived (throw 'mpvi-seek msg)
      (message msg))))

(defun mpvi-delay-subtitle (sec)
  "Delay subtitle for SEC for mpv."
  (interactive (list (progn
                       (mpvi-check-live)
                       (read-number "Subtitle to delay: " (or (mpvi-prop 'sub-delay) 0)))))
  (mpvi-check-live)
  (mpvi-prop 'sub-delay sec))

(defun mpvi-capture-subtitle ()
  "Copy subtitle text of current playing video."
  (interactive)
  (mpvi-check-live)
  (let ((msg "Copied to kill ring, yank to the place you want."))
    (if-let ((sub (mpvi-prop 'sub-text)))
        (kill-new sub)
      (setq msg "No sub text found"))
    (if mpvi-seek--actived (throw 'mpvi-seek msg)
      (message msg))))

(defun mpvi-switch-playlist ()
  "Extract playlist from current video url.
If any, prompt user to choose one video in playlist to play."
  (interactive)
  (mpvi-check-live)
  (if-let* ((playlist (plist-get mpvi-current-url-metadata :playlist-url))
            (playlist-index (plist-get mpvi-current-url-metadata :playlist-index))
            (msg "Switch done."))
      (condition-case nil
          (throw 'mpvi-seek (prog1 msg (mpvi-play playlist nil nil nil t)))
        (error (message msg)))
    (user-error "No playlist found for current playing url")))

(defun mpvi-ab-loop ()
  "Cycle ab-loop for mpv."
  (interactive)
  (mpvi-cmd `(ab-loop)))

(defun mpvi-seekable (&optional arg)
  "Whether current video is seekable.
Alert user when not seekable when ARG not nil."
  (mpvi-check-live)
  (let ((seekable (eq (mpvi-prop 'seekable) t)))
    (if (and arg (not seekable))
        (user-error "Current video is not seekable, do nothing")
      seekable)))

(defun mpvi-volume (&optional val)
  "Tune the volume base on VAL.
VAL is nil for reset to 100. If it is a number, set volume directly.
If it is a string, with prefix +/- for relative value, and can be:

  2, +2, -2, 2x, -2x

That is:

  To 2, add 2, sub 2, add 2 times, sub 2 times."
  (interactive (list (progn
                       (mpvi-check-live)
                       (let ((str (read-string (format "Set volume to (20,±20,2x ∈ [0, 100+]. current %s): "
                                                       (mpvi-prop 'volume)))))
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
      (if relative (setq val (if relative (+ (mpvi-prop 'volume) val) val)))
      (if (< val 0) (setq val 0))
      (mpvi-prop 'volume val)))
  (when (called-interactively-p 'any)
    (message "Volume changed to %s" (mpvi-prop 'volume))))

(defun mpvi-speed (&optional val)
  "Tune the speed base on VAL.
VAL is nil for reset to 1. If it is a number, tune to such speed directly.
If it is a string, with prefix +/- for relative value, and can be:

  2, +2, -2, 2x, -2x

That is:

  To 2, add 2, sub 2, add 2 times, sub 2 times."
  (interactive (list (progn
                       (mpvi-check-live)
                       (let ((str (read-string (format "Set speed to (2,±2,2x ∈ [0.1, 100]. current %s): "
                                                       (mpvi-prop 'speed)))))
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
      (mpvi-prop 'speed (if relative (+ (mpvi-prop 'speed) val) val))))
  (when (called-interactively-p 'any)
    (message "Speed changed to %s" (mpvi-prop 'speed))))

(defun mpvi-position (val)
  "Jump to new position based on VAL.
VAL can be a number or string. If it is a number, seek to absolute position.
If it is a string, with prefix +/- for relative value, and can be:

  6, 6s, 6f, 6%, 6:06

That is:

  6 seconds, 6 seconds, 6 frames, percent, datetime."
  (interactive (list (progn
                       (mpvi-seekable 'assert)
                       (let* ((duration (mpvi-prop 'duration))
                              (str (read-string (format "Jump (max: %.1fs, e.g.: [+-]6,6s,6f,6%%,6:06): " duration))))
                         (unless (string-blank-p str) str)))))
  (mpvi-seekable 'assert)
  (when val
    (if (and (stringp val) (string-match "^[+-]?[0-9]+fs?$" val)) ; Nfs for relative frames
        (let ((paused (mpvi-prop 'pause)))
          (unwind-protect
              (mpvi-cmd `(frame_step ,(string-to-number val) seek))
            (mpvi-prop 'pause (or paused :json-false))))
      (let (relative)
        (when (stringp val)
          (when (memq (aref val 0) '(?+ ?-)) ; string with +- prefix: relative position
            (setq relative t))
          (setq val
                (cond ((string-match-p "^[+-]?[0-9]\\{0,2\\}\\.?[0-9]*%$" val) ; percent%
                       (* (/ (string-to-number (cl-subseq val 0 -1)) 100.0) (mpvi-prop 'duration)))
                      ((string-match-p "^[+-]?[0-9.]+s?$" val) ; +32323s
                       (string-to-number val))
                      ((string-match "^\\+?\\(-?[0-9]+:[0-9:.]+\\)$" val) ; 2:23
                       (mpvi-time-to-secs (match-string 1 val)))
                      (t (user-error "Error input pos: %s" val)))))
        (let ((total (mpvi-prop 'duration)))
          (when relative
            (setq val (+ val
                         (if (and (> (recursion-depth) 0)
                                  (or (zerop val) (eq (mpvi-prop 'pause) t)))
                             (let ((str (string-trim (minibuffer-contents))))
                               (or (mpvi-time-to-secs str total)
                                   (user-error "Not valid time input")))
                           (mpvi-prop 'time-pos)))))
          (if (< val 0) (setq val 0)
            (if (> val total) (setq val total)))
          (mpvi-prop 'time-pos val))))))


;;; Utils Integrated

(defvar mpvi-screenshot-function #'mpvi-screenshot)

(defvar mpvi-ocr-function #'mpvi-ocr)

(defvar mpvi-local-video-handler #'mpvi-convert-by-ffmpeg)

(defvar mpvi-remote-video-handler #'mpvi-ytdlp-download)

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
                 (list (mpvi-read-file-or-url "Video to screenshot: ") nil
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
                                 (if mpvi-seek--actived (throw 'mpvi-seek msg) (message msg))))
                           (delete-file file))
                       (prog1 file
                         (kill-new file)
                         (let ((msg (format "Saved to `%s'." file)))
                           (if mpvi-seek--actived (throw 'mpvi-seek msg) (message msg))))))))
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
        (setq video (mpvi-read-file-or-url "Video: ")))
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
    (if mpvi-seek--actived (throw 'mpvi-seek msg) (message msg))))

;; ffmpeg

(defcustom mpvi-ffmpeg-extra-args nil
  "Extra options pass to `ffmpeg'."
  :type 'string)

(defcustom mpvi-ffmpeg-gif-filter "fps=10,crop=iw:ih:0:0,scale=320:-1:flags=lanczos,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse"
  "Filter used when use `ffmpeg' to convert to gif file."
  :type 'string)

(defun mpvi-convert-by-ffmpeg (file &optional target beg end opts)
  "Convert local video FILE from BEG to END using ffmpeg, output to TARGET.
This can be used to cut/resize/reformat and so on.
OPTS is a string, pass to `ffmpeg' when it is not nil."
  (cl-assert (file-regular-p file))
  (unless (executable-find "ffmpeg")
    (user-error "Program `ffmpeg' not found"))
  (let* ((beg (if (numberp beg) (format " -ss %s" beg) ""))
         (end (if (numberp end) (format " -to %s" end) ""))
         (target (expand-file-name
                  (or target (format-time-string "mpv-video-%s.mp4"))
                  mpvi-last-save-directory))
         (extra (concat (if (member (file-name-extension target) '("gif" "webp"))
                            (format " -vf \"%s\" -loop 0" mpvi-ffmpeg-gif-filter)
                          " -c copy")
                        (if (or opts mpvi-ffmpeg-extra-args)
                            (concat " " (string-trim (or opts mpvi-ffmpeg-extra-args))))))
         (command (string-trim
                   (minibuffer-with-setup-hook
                       (lambda ()
                         (use-local-map (make-composed-keymap nil (current-local-map)))
                         (local-set-key (kbd "C-x C-q")
                                        (lambda ()
                                          (interactive)
                                          (let ((inhibit-read-only t))
                                            (set-text-properties (minibuffer-prompt-end) (point-max) nil))))
                         (local-set-key (kbd "<return>")
                                        (lambda ()
                                          (interactive)
                                          (let ((cmd (minibuffer-contents)))
                                            (with-temp-buffer
                                              (insert (string-trim cmd))
                                              (let ((quote (if (member (char-before) '(?' ?\")) (char-before))))
                                                (when (re-search-backward (if quote (format " +%c" quote) " +") nil t)
                                                  (setq target (buffer-substring (match-end 0) (if quote (- (point-max) 1) (point-max)))))))
                                            (if (file-exists-p target)
                                                (message
                                                 (propertize
                                                  (format "Output file %s is already exist!" target)
                                                  'face 'font-lock-warning-face))
                                              (exit-minibuffer))))))
                     (read-string "Confirm: "
                                  (concat (propertize
                                           (concat "ffmpeg"
                                                   (propertize " -loglevel error" 'invisible t)
                                                   (format " -i %s" (expand-file-name file)))
                                           'face 'font-lock-constant-face 'read-only t)
                                          beg end extra (format " \"%s\"" target)))))))
    (make-directory (file-name-directory target) t) ; ensure directory
    (setq mpvi-last-save-directory (file-name-directory target)) ; record the dir
    (with-temp-buffer
      (mpvi-log "Convert file %s" file)
      (apply #'mpvi-call-process (split-string-and-unquote command))
      (if (file-exists-p target)
          (prog1 target
            (kill-new target)
            (message "Save to %s done." (propertize target 'face 'font-lock-keyword-face)))
        (user-error "Convert with ffmpeg failed: %s" (string-trim (buffer-string)))))))

;; yt-dlp

(defcustom mpvi-ytdlp-extra-args nil
  "The default extra options pass to `yt-dlp'."
  :type 'string)

(defvar mpvi-ytdlp-metadata-cache nil)

(defun mpvi-ytdlp-url-metadata (url &optional opts)
  "Return metadata for URL, pass extra OPTS to `yt-dlp' for querying.
I just want to judge if current URL is a playlist link, but I can't find
better/faster solution. Maybe cache the results is one choice, but I don't think
it's good enough. Then I can not find good way to get all descriptions of
playlist item with light request. This should be improved someday."
  (unless (executable-find "yt-dlp")
    (user-error "Program `yt-dlp' should be installed"))
  (or (cdr (assoc url mpvi-ytdlp-metadata-cache))
      (with-temp-buffer
        (condition-case nil
            (progn
              (mpvi-log "Request matadata for %s" url)
              (apply #'mpvi-call-process
                     "yt-dlp" url "-J" "--flat-playlist" "--no-warnings"
                     (split-string-and-unquote (or opts mpvi-ytdlp-extra-args "")))
              (goto-char (point-min))
              (let* ((json (json-read))
                     (playlistp (equal "playlist" (alist-get '_type json))))
                (if playlistp (nconc json (list '(is_playlist . t))))
                (push (cons url json) mpvi-ytdlp-metadata-cache)
                json))
          (error (user-error "Error when get metadata for %s: %s" url (string-trim (buffer-string))))))))

(defun mpvi-ytdlp-pick-format (url)
  "Completing read the formats for video with URL.
Return (suggestion-save-name . video-format)."
  (unless (executable-find "yt-dlp")
    (user-error "Program 'yt-dlp' should be installed"))
  (with-temp-buffer
    (mpvi-call-process "yt-dlp" "-F" url)
    (goto-char (point-min))
    (unless (re-search-forward "Available formats for \\(.+\\):" nil t)
      (user-error "Nothing found: %s" (string-trim (buffer-string))))
    (let* ((name (if (equal (mpvi-prop 'path) url)
                     (mpvi-prop 'media-title)
                   (match-string 1)))
           (fmts (cl-loop with text = (string-trim (buffer-substring
                                                    (progn (search-forward "-\n" nil t) (point))
                                                    (point-max)))
                          for item in (split-string text "\n")
                          collect (cons (concat (propertize ">  " 'face 'font-lock-keyword-face) item)
                                        (split-string item " +"))))
           (format (string-trim
                    (completing-read
                     "Format (choose directly for one, input like '1,4' for multiple. Default: 'bv,ba'): "
                     (lambda (input _pred action)
                       (pcase action
                         ('metadata
                          `(metadata (display-sort-function . ,#'identity)))
                         (`(boundaries . _)
                          `(boundaries . ,(cons (length input) 0)))
                         (_ (complete-with-action action fmts "" nil))))
                     nil nil nil nil "bv,ba")))
           (format (if (string-prefix-p ">" format)
                       (cadr (assoc format fmts))
                     (string-trim (cl-subseq format 0 (cl-position ?\> format)))))
           (ext (if-let* ((fmt (cl-find-if (lambda (c) (equal (cadr c) format)) fmts)))
                    (caddr fmt) "mp4")))
      (setq format (string-replace " " "" (string-replace "," "+" format)))
      (cons (concat name "_" format "." ext) format))))

(defun mpvi-ytdlp-output-field (url field &optional opts)
  "Get FIELD information for video URL.
FIELD can be id/title/urls/description/format/thumbnail/formats_table and so on.
Pass extra OPTS to mpv if it is not nil."
  (unless (executable-find "yt-dlp")
    (user-error "Program 'yt-dlp' should be installed"))
  (with-temp-buffer
    (mpvi-log "yt-dlp output template for %s of %s" field url)
    (apply #'mpvi-call-process
           "yt-dlp" url "--print" field
           (split-string-and-unquote (or opts mpvi-ytdlp-extra-args "")))
    (goto-char (point-min))
    (if (re-search-forward "^yt-dlp: error:.*$" nil t)
        (user-error "Error to get `yt-dlp' template/%s: %s" field (match-string 0))
      (string-trim (buffer-string)))))

(defun mpvi-ytdlp-download (url &optional target beg end opts)
  "Download and clip video for URL to TARGET. Use BEG and END for range (trim).
OPTS is a string, pass to `yt-dlp' when it is not nil."
  (cl-assert (mpvi-url-p url))
  (unless (and (executable-find "yt-dlp") (executable-find "ffmpeg"))
    (user-error "Programs `yt-dlp' and `ffmpeg' should be installed"))
  (let* ((fmt (mpvi-ytdlp-pick-format url))
         (beg (if (numberp beg) (format " -ss %s" beg)))
         (end (if (numberp end) (format " -to %s" end)))
         (extra (if (or opts mpvi-ytdlp-extra-args)
                    (concat " " (string-trim (or opts mpvi-ytdlp-extra-args)))
                  ""))
         (target (expand-file-name (or target (car fmt)) mpvi-last-save-directory))
         (command (string-trim
                   (minibuffer-with-setup-hook
                       (lambda ()
                         (backward-char)
                         (use-local-map (make-composed-keymap nil (current-local-map)))
                         (local-set-key (kbd "<return>")
                                        (lambda ()
                                          (interactive)
                                          (let ((cmd (minibuffer-contents)))
                                            (with-temp-buffer
                                              (insert cmd)
                                              (goto-char (point-min))
                                              (when (re-search-forward " -o +['\"]?\\([^'\"]+\\)" nil t)
                                                (setq target (match-string 1)))
                                              (if (file-exists-p target)
                                                  (message
                                                   (propertize
                                                    (format "Output file %s is already exist!" target)
                                                    'face 'font-lock-warning-face))
                                                (exit-minibuffer)))))))
                     (read-string
                      "Confirm: "
                      (concat (propertize (concat "yt-dlp " url) 'face 'font-lock-constant-face 'read-only t)
                              " -f \"" (cdr fmt) "\""
                              (if (or beg end) " --downloader ffmpeg --downloader-args \"ffmpeg_i:")
                              beg end (if (or beg end) "\"") extra
                              " -o \"" target "\""))))))
    (make-directory (file-name-directory target) t) ; ensure directory
    (setq mpvi-last-save-directory (file-name-directory target)) ; record the dir
    (with-temp-buffer
      (mpvi-log "Download/Clip url %s" url)
      (apply #'mpvi-call-process (split-string-and-unquote command))
      (if (file-exists-p target)
          (prog1 target
            (kill-new target)
            (message "Save to %s done." (propertize target 'face 'font-lock-keyword-face)))
        (user-error "Download and clip with yt-dlp/ffmpeg failed: %s" (string-trim (buffer-string)))))))

(defun mpvi-ytdlp-download-subtitle (url &optional prefix opts)
  "Download subtitle for URL and save as file named begin with PREFIX.
Pass OPTS to `yt-dlp' when it is not nil."
  (unless (executable-find "yt-dlp")
    (user-error "Program `yt-dlp' should be installed"))
  (with-temp-buffer
    (mpvi-log "Downloading subtitle for %s" url)
    (apply #'mpvi-call-process
           "yt-dlp" url "--write-subs" "--skip-download"
           "-o" (or prefix (expand-file-name "SUB-%(fulltitle)s-%(id)s" (mpvi-cache-directory)))
           (split-string-and-unquote (or opts mpvi-ytdlp-extra-args "")))
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
  (if (string-match "^\\([^#]+\\)\\(?:#\\([0-9:.]+\\)?-?\\([0-9:.]+\\)?\\)?$" link)
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
  "Insert or update timestamp link in a org mode buffer.
When ENDP, update end time of the link, and remove end time if seek return nil."
  (interactive "P")
  (mpvi-seekable 'assert)
  (let* ((paused (prog1 (mpvi-prop 'pause) (mpvi-pause t)))
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
                           (mpvi-prop 'time-pos)
                           (format "%s (%s ∈ [%d, %d]; mm:ss,n,n%%): "
                                   (if (or end endp) (if endp "Loop-B" "Loop-A") "Time")
                                   (if endp end beg)
                                   (if endp beg 0)
                                   (if (or endp (null end)) (mpvi-prop 'duration) end)))
                        (mpvi-prop 'time-pos)))
            (when (and endp (null pos))
              (setq pos -1)) ; for remove loop-B
            (unless (numberp pos)
              (user-error "Wrong seek position"))
            (delete-region (plist-get node :begin) (plist-get node :end)))
        ;; when call the command directly, and A-B Loop is on, use the range
        (when in-place
          (let ((a (mpvi-prop 'ab-loop-a))
                (b (mpvi-prop 'ab-loop-b)))
            (unless (or (equal a "no") (equal b "no"))
              (setq beg a end b)))))
      (unless pos (setq desc (string-trim (read-string "Description: "))))
      (setq link (funcall mpvi-build-link-function
                          path
                          (if endp
                              (or beg (mpvi-prop 'time-pos))
                            (or pos (mpvi-prop 'time-pos)))
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
    (if mpvi-seek--actived (throw 'mpvi-seek msg) (message msg))))


;;; Interactive Commands

;;;###autoload
(defun mpvi-open (path)
  "Open PATH for playing with MPV.
PATH is a local video or remote url. Prefer the one at point."
  (interactive (catch 'mpvi-open
                 (list (mpvi-read-file-or-url "Play video (file or url): " mpvi-open-map))))
  (unless (and (> (length path) 0) (or (mpvi-url-p path) (file-exists-p path)))
    (user-error "Not correct file or url"))
  (setq path (if (mpvi-url-p path) path (expand-file-name path)))
  (setq mpvi-current-url-metadata nil)
  (with-current-emms-playlist (setq emms-playlist-selected-marker nil))
  (mpvi-play path))

;;;###autoload
(defun mpvi-emms-add (path &optional label)
  "Add PATH to EMMS playlist. LABEL is extra info to show in EMMS buffer."
  (interactive (list (mpvi-read-file-or-url "Add to EMMS (file or url): ")))
  (unless (and (> (length path) 0) (or (mpvi-url-p path) (file-exists-p path)))
    (user-error "Not correct file or url"))
  (if (mpvi-url-p path)
      (let ((playlist (mpvi-extract-playlist
                       (intern (concat ":" (url-host (url-generic-parse-url path)))) path t))
            choosen)
        (when playlist
          (setq choosen
                (completing-read "Choose from playlist: "
                                 (lambda (input pred action)
                                   (if (eq action 'metadata)
                                       `(metadata (display-sort-function . ,#'identity))
                                     (complete-with-action action (cons "ALL" (cdr playlist)) input pred)))
                                 nil t)))
        (if (equal choosen "ALL") (setq choosen (cdr playlist)))
        (setq choosen (or choosen path))
        (unless (consp choosen) (setq choosen (list choosen)))
        (cl-loop with desc = (or label (read-string "Label for current url: " (car playlist)))
                 for url in choosen
                 for disp = (if (> (length desc) 0) (format "%s - %s" desc url) url)
                 do (emms-add-url (propertize url 'display disp))))
    (setq path (expand-file-name path))
    (cond ((file-directory-p path)
           (emms-add-directory path))
          ((file-regular-p path)
           (emms-add-file path))
          (t (user-error "Unkown source: %s" path)))))

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
                 (setq path (ignore-errors (mpvi-prop 'path))))
            (list path))
           (t
            (setq path (mpvi-read-file-or-url "Clip video (file or url): "))
            (list path (unless (mpvi-url-p path) (mpvi-read-path "Save to: " path)))))))
  (funcall (if (mpvi-url-p path) mpvi-remote-video-handler mpvi-local-video-handler)
           path target beg end)
  (if mpvi-seek--actived (throw 'mpvi-seek "Export success.")))

;;;###autoload
(defun mpvi-browse (path)
  "Open current playing video PATH with system program."
  (interactive (list (if (and (not current-prefix-arg) (emms-player-mpv-proc-playing-p))
                         (mpvi-origin-path)
                       (mpvi-read-file-or-url "File or url: "))))
  (unless (and (stringp path) (mpvi-url-p path))
    (user-error "This should be a remote video"))
  (if (or (not mpvi-seek--actived)
          (y-or-n-p (format "Open '%s' externally?" path)))
      (let ((msg "Open in system program done."))
        ;; add begin time for url if necessary
        (when-let* ((fn (plist-get mpvi-current-url-metadata :out-url-decorator)))
          (setq path (funcall fn path (mpvi-prop 'time-pos))))
        (browse-url path)
        (if mpvi-seek--actived
            (throw 'mpvi-seek msg)
          (mpvi-pause t)
          (message msg)))
    (message "")))

;;; Control Panel

(defvar mpvi-control-buffer "*mpvi-control*")

(defvar mpvi-control-display-action
  `((display-buffer-below-selected)
    (window-height . 7)))

(defvar mpvi-control-render-function #'mpvi-control-render)

(defvar mpvi-control--timer nil)

(defvar mpvi-control--last-data nil)

(defmacro mpvi-mkcmd1 (form &optional revert-p)
  "Helper to build interactive command with FORM.
When REVERT-P is t, try to revert the input for seek."
  `(lambda ()
     (interactive)
     (prog1 ,form
       ,(if revert-p `(mpvi-revert-seek)))))

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
    (define-key map (kbd "n")   (mpvi-mkcmd1 (mpvi-position "+1s") t))
    (define-key map (kbd "p")   (mpvi-mkcmd1 (mpvi-position "-1s") t))
    (define-key map (kbd "N")   (mpvi-mkcmd1 (mpvi-position "+5s") t))
    (define-key map (kbd "P")   (mpvi-mkcmd1 (mpvi-position "-5s") t))
    (define-key map (kbd "M-n") (mpvi-mkcmd1 (mpvi-position "+1%") t))
    (define-key map (kbd "M-p") (mpvi-mkcmd1 (mpvi-position "-1%") t))
    (define-key map (kbd "C-n") (mpvi-mkcmd1 (mpvi-position "+1s") t))
    (define-key map (kbd "C-p") (mpvi-mkcmd1 (mpvi-position "-1s") t))
    (define-key map (kbd ".")   (mpvi-mkcmd1 (mpvi-position "+1f") t))
    (define-key map (kbd ",")   (mpvi-mkcmd1 (mpvi-position "-1f") t))
    (define-key map (kbd ">")   (mpvi-mkcmd1 (mpvi-position "+1f") t))
    (define-key map (kbd "<")   (mpvi-mkcmd1 (mpvi-position "-1f") t))
    (define-key map (kbd "C-l") (mpvi-mkcmd1 (mpvi-position "-0s") t))
    (define-key map [left]      (mpvi-mkcmd1 (mpvi-position "-5s") t))
    (define-key map [right]     (mpvi-mkcmd1 (mpvi-position "+5s") t))
    (define-key map (kbd "0")   (mpvi-mkcmd1 (mpvi-volume "+10") t))
    (define-key map (kbd "9")   (mpvi-mkcmd1 (mpvi-volume "-10") t))
    (define-key map [up]        (mpvi-mkcmd1 (mpvi-volume "+10") t))
    (define-key map [down]      (mpvi-mkcmd1 (mpvi-volume "-10") t))
    (define-key map (kbd "M-<") (mpvi-mkcmd1 (mpvi-revert-seek 0)))
    (define-key map (kbd "l")   (mpvi-mkcmd1 (mpvi-speed)))
    (define-key map (kbd "j")   (mpvi-mkcmd1 (mpvi-speed "-1.1x")))
    (define-key map (kbd "k")   (mpvi-mkcmd1 (mpvi-speed "+1.1x")))
    (define-key map (kbd "J")   (mpvi-mkcmd1 (mpvi-speed "-0.5")))
    (define-key map (kbd "K")   (mpvi-mkcmd1 (mpvi-speed "+0.5")))
    (define-key map (kbd "[")   (mpvi-mkcmd1 (mpvi-speed "0.9x")))
    (define-key map (kbd "]")   (mpvi-mkcmd1 (mpvi-speed "1.1x")))
    (define-key map (kbd "{")   (mpvi-mkcmd1 (mpvi-speed "0.5x")))
    (define-key map (kbd "}")   (mpvi-mkcmd1 (mpvi-speed "2.0x")))
    (define-key map [backspace] (mpvi-mkcmd1 (mpvi-speed)))
    (define-key map (kbd "s")   (mpvi-mkcmd1 (mpvi-screenshot t 'interact)))
    (define-key map (kbd "C-s") (mpvi-mkcmd1 (mpvi-screenshot t t)))
    (define-key map (kbd "c")   #'mpvi-switch-playlist)
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
    (define-key map (kbd "q")   #'mpvi-control-quit)
    (define-key map (kbd "h")   #'mpvi-control-tips)
    map)
  "Keymap for `mpvi-control-mode'.")

(define-derived-mode mpvi-control-mode special-mode "MPVi-Control"
  "Major mode for MPV control panel."
  :interactive nil
  (setq buffer-read-only t cursor-type nil)
  (use-local-map mpvi-control-map))

(defun mpvi-control-tips ()
  "Command tips for current seek."
  (interactive)
  (let ((tips '(("aa" . "bb")
                ("SPC m M T f L" . "Toggle Pause/Mute/Video/Ontop/Fullscreen/AB-Loop")
                ("n p N P M-n M-p C-l"   . "Forward/Backward")
                ("< >" . "Prev/Next Frame")
                ("/ g" . "Interactively Seek")
                ("9 0 up down" . "Tune volume")
                ("j k l [ ] { } Backspace" . "Tune Speed")
                ("s C-s" . "Screenshot")
                ("v z t C-t" . "Subtitle")
                ("r o c e" . "OCR/Browser/Playlist/Export")
                ("i I C-i" . "Video Notes (timestamp link/screenshot)"))))
    (prog1 tips
      (let ((s (mapconcat (lambda (tip)
                            (concat (propertize (car tip) 'face 'bold) ": " (cdr tip)))
                          tips "\n")))
        (message "------------") (message s)))))

(defun mpvi-control-quit ()
  "Close the control panel."
  (interactive)
  (when mpvi-control--timer
    (cancel-timer mpvi-control--timer)
    (setq mpvi-control--timer nil))
  (setq mpvi-control--last-data nil)
  (let ((buf (get-buffer mpvi-control-buffer)))
    (when (buffer-live-p buf)
      (when-let* ((win (get-buffer-window buf)))
        (delete-window win))
      (kill-buffer buf))))

(cl-defun mpvi-control-render (&key path title time total speed volume muted video loop pause &allow-other-keys)
  "Insert playback status for the control panel.
PATH, TITLE, TIME, TOTAL, SPEED, VOLUME, MUTED, VIDEO, LOOP and PAUSE are status
of mpvi process."
  (let* ((ww (window-width))
         (title (if (stringp title) (propertize title 'help-echo path) path))
         (tip (if (> ww 90) (concat "press " (propertize "h" 'face 'warning) " for help")))
         (width (min (max 30 (- ww 15)) 60))
         (percent (/ time total))
         (elapsed (if (> total 0) (round (* width percent)) 0))
         (remained (- width elapsed))
         (bar (concat (apply #'propertize
                             (format "%s [%s%s] %s"
                                     (mpvi-secs-to-hms time nil t)
                                     (make-string (max 1 elapsed) (if pause ?▬ ?■))
                                     (make-string remained ?-)
                                     (mpvi-secs-to-hms (- total time) nil t))
                             'pointer 'hand
                             (when (eq (window-buffer) (current-buffer))
                               '(face font-lock-keyword-face)))))
         (prop (lambda (k v r)
                 (concat "[" k "]" (if v " ")
                         (if v (propertize (format "%s" v) 'face 'font-lock-variable-use-face)) r)))
         (rest (concat (funcall prop
                                (if pause "⏸" (if video "Video" "Audio"))
                                (format "%.1f/%.1f, %.1f%%" time total (* 100 percent)) " ")
                       (funcall prop (if muted (propertize "Volume" 'face '(:strike-through t)) "Volume") volume " ")
                       (funcall prop "Speed" speed " ")
                       (if loop (funcall prop "Loop" loop " ")))))
    (save-excursion
      (erase-buffer)
      (setq header-line-format
            (concat title (if tip (propertize " " 'display `((space :align-to (- right ,(length tip)))))) tip))
      (insert (propertize " " 'display (concat "\n" bar "\n\n" rest))))))

(defun mpvi-control-refresh ()
  "Refresh the control panel with current playback info."
  (condition-case err
      (let* ((buf (get-buffer mpvi-control-buffer))
             (win (and buf (buffer-live-p buf) (get-buffer-window buf))))
        (if (and win (emms-player-mpv-proc-playing-p))
            (let ((data (list :path   (mpvi-prop 'path)
                              :title  (mpvi-prop 'media-title)
                              :time   (mpvi-prop 'time-pos)
                              :total  (mpvi-prop 'duration)
                              :speed  (mpvi-prop 'speed)
                              :volume (mpvi-prop 'volume)
                              :muted  (mpvi-prop 'mute)
                              :video  (mpvi-prop 'video)
                              :pause  (mpvi-prop 'pause)
                              :loop (let ((a (mpvi-prop 'ab-loop-a)) (b (mpvi-prop 'ab-loop-b)))
                                      (if (equal a "no") (setq a nil))
                                      (if (equal b "no") (setq b nil))
                                      (when (or a b) (cons a b)))
                              :ww (window-width win) :focus (eq (selected-window) win))))
              (unless (equal data mpvi-control--last-data)
                (with-current-buffer buf
                  (let ((inhibit-read-only t))
                    (apply mpvi-control-render-function data)
                    (set-buffer-modified-p nil)))
                (setq mpvi-control--last-data data)))
          (mpvi-control-quit)))
    ((debug error)
     (mpvi-control-quit)
     (message "Control refresh error and quit (%s)" err))))

;;;###autoload
(defun mpvi-control ()
  "Open a control panel in a buffer for MPV."
  (interactive)
  (mpvi-check-live)
  (unless (buffer-live-p (get-buffer mpvi-control-buffer))
    (with-current-buffer (get-buffer-create mpvi-control-buffer)
      (mpvi-control-mode)
      (if mpvi-control--timer (cancel-timer mpvi-control--timer))
      (setq mpvi-control--timer (run-with-timer 0 0.2 #'mpvi-control-refresh))))
  (when (buffer-live-p (get-buffer mpvi-control-buffer))
    (pop-to-buffer mpvi-control-buffer mpvi-control-display-action)))

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
              (let* ((loop (if (mpvi-prop 'loop) (funcall vf "[Looping] ")))
                     (paused (if (mpvi-prop 'pause) (funcall vf "[Paused] ")))
                     (time (funcall vf (mpvi-secs-to-hms (mpvi-prop 'time-pos) nil t)))
                     (total (funcall vf (mpvi-secs-to-hms (mpvi-prop 'duration) nil t)))
                     (percent (funcall vf (format "%.1f%%" (mpvi-prop 'percent-pos))))
                     (speed (funcall vf (format "%.2f" (mpvi-prop 'speed))))
                     (concated (concat loop (if loop " ") paused (if paused " ")
                                       time "/" total "  " percent "  Speed: " speed))
                     (space (funcall sf concated)))
                (overlay-put ov 'before-string (propertize (concat space concated) 'cursor t))
                (setq mpvi-seek-refresh-timer (run-with-timer 0.5 nil #'mpvi-seek-refresh)))
            (error nil))
        (let* ((title (funcall vf (concat "        >> " (string-trim (or (mpvi-prop 'media-title) "")))))
               (state (funcall vf (if (mpvi-prop 'pause) "Paused")))
               (space (funcall sf state)))
          (delete-minibuffer-contents)
          (insert "0")
          (overlay-put ov 'before-string (propertize (concat title space state) 'cursor t)))))))

;;;###autoload
(defun mpvi-seek (&optional pos prompt)
  "Interactively seek POS for current playing video.
PROMPT is used if non-nil for `minibuffer-prompt'."
  (interactive)
  (unless mpvi-seek--actived
    (mpvi-seekable 'assert)
    (let ((paused (mpvi-prop 'pause))
          (target-depth nil))
      (unwind-protect
          (let* ((enable-recursive-minibuffers t)
                 (ret (catch 'mpvi-seek
                        (minibuffer-with-setup-hook
                            (lambda ()
                              (unless target-depth ; Notice: setup-hook can affect minibuffers in any depth
                                (setq target-depth (minibuffer-depth)))
                              (when (eq (minibuffer-depth) target-depth)
                                (setq mpvi-seek--actived t)
                                (setq mpvi-seek--paused nil)
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
                                            (when (and mpvi-seek--actived (eq (minibuffer-depth) target-depth))
                                              (ignore-errors (cancel-timer mpvi-seek-refresh-timer))
                                              (setq mpvi-seek-refresh-timer nil)
                                              (setq mpvi-seek--actived nil))))))
                          (ignore-errors ; case for empty string
                            (read-from-minibuffer
                             (or prompt (if (mpvi-seekable)
                                            (format "Seek ([0,%d] mm:ss,n,n%%): " (mpvi-prop 'duration))
                                          "MPV Controller: "))
                             (format "%.1f" (or pos (mpvi-prop 'time-pos)))
                             mpvi-seek-map t 'mpvi-seek-hist))))))
            (pcase ret
              ('nil (ignore))
              ((pred stringp) (message "%s" ret))
              ((pred numberp) (mpvi-prop 'time-pos ret))
              ((pred symbolp) (mpvi-prop 'time-pos (mpvi-time-to-secs (format "%s" ret) (mpvi-prop 'duration))))))
        (mpvi-pause (or mpvi-seek--paused paused))))))

(defun mpvi-revert-seek (&optional num)
  "Insert current time-pos to minibuffer.
If NUM is not nil, go back that position first."
  (interactive)
  (when (and mpvi-seek--actived (minibufferp))
    (when (and num (mpvi-seekable))
      (mpvi-prop 'time-pos num))
    (delete-minibuffer-contents)
    (insert (mpvi-secs-to-string (mpvi-prop 'time-pos)))))

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
    (mpvi-play path beg end)))

(defcustom mpvi-org-https-link-rules nil
  "Rules to check if current https link should be opened with MPV.
One rule is a regexp string to check against link url."
  :type '(repeat string))

(defun mpvi-org-https-link-push (url arg)
  "Play the normal https URL with MPV if it matches any of the rules.
ARG is the argument."
  (if (cl-find-if (lambda (r) (string-match-p r url)) mpvi-org-https-link-rules)
      (mpvi-open (concat "https:" url))
    (browse-url (concat "https:" url) arg)))

(defun mpvi-current-link-update-end-pos ()
  "Update the end position on this link."
  (interactive nil org-mode)
  (mpvi-with-current-mpv-link (node)
    (when node
      (let ((ret (mpvi-seek (or (plist-get node :vend)
                                (max (plist-get node :vbeg) (mpvi-prop 'time-pos)))
                            (format "Set end position (%d-%d): " (plist-get node :vbeg) (mpvi-prop 'duration)))))
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
(require 'mpvi-bilibili)

(mpvi-emms-integrated-mode t)

(provide 'mpvi)

;;;###autoload
(if (memq system-type '(cygwin windows-nt)) (eval-after-load 'emms '(require 'mpvi)))

;;; mpvi.el ends here
