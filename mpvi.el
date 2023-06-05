;;; mpvi.el --- Integrated video tool based on EMMS and MPV -*- lexical-binding: t -*-

;; Copyright (C) 2023 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; URL: https://github.com/lorniu/mpvi
;; Package-Requires: ((emacs "28.1") (emms "11"))
;; Keywords: convenience, docs, multimedia, application
;; SPDX-License-Identifier: MIT
;; Version: 1.0

;;; Commentary:
;;
;; Integrate MPV, EMMS, Org and others with Emacs, make watching videos
;; conveniently and taking notes easily. Make EMMS support Windows.
;;
;; Installation:
;;  - Install `emms' from elpa
;;  - Install `mpvi' from melpa, then load it
;;  - Install the dependencies: mpv (required), yt-dlp, ffmpeg, seam, danmaku2ass and tesseract
;;
;; Use `mpvi-open' to open a video, then control the MPV with `mpvi-seek'.
;; Also you can play videos from `emms'.
;;
;; For more information, see README file.

;;; Code:

(require 'ffap)
(require 'emms)
(require 'emms-player-mpv)

(defgroup mpvi nil
  "Integrate MPV with Emacs."
  :group 'external
  :prefix 'mpvi-)

(defcustom mpvi-cache-directory
  (let ((dir (expand-file-name "mpvi/" (temporary-file-directory))))
    (unless (file-exists-p dir) (make-directory dir))
    dir)
  "Used to save temporary files."
  :type 'directory)

(defvar mpvi-last-save-directory nil)

(defvar mpvi-play-history nil)

(defvar mpvi-annotation-face '(:inherit completions-annotations))

(defvar mpvi-build-link-function #'mpvi-build-mpv-link)

(defvar mpvi-screenshot-function #'mpvi-screenshot)

(defvar mpvi-ocr-function #'mpvi-ocr-by-tesseract)

(defvar mpvi-local-video-handler #'mpvi-convert-by-ffmpeg)

(defvar mpvi-remote-video-handler #'mpvi-ytdlp-download)

;; Silence compiler

(defvar org-attach-method)
(defvar org-mouse-map)
(declare-function org-link-set-parameters   "org.el" t t)
(declare-function org-open-at-point         "org.el" t t)
(declare-function org-insert-item           "org.el" t t)
(declare-function org-at-item-p             "org.el" t t)
(declare-function org-display-inline-images "org.el" t t)
(declare-function org-attach-attach         "org.el" t t)
(declare-function org-timer-secs-to-hms     "org.el" t t)
(declare-function org-timer-fix-incomplete  "org.el" t t)
(declare-function org-timer-hms-to-secs     "org.el" t t)
(declare-function org-element-context       "org.el" t t)

;; Helpers

(defun mpvi-log (fmt &rest args)
  "Output log when `emms-player-mpv-debug' not nil.
FMT and ARGS are like arguments in `message'."
  (when emms-player-mpv-debug
    (apply #'message (concat "[mpvi] " fmt) args)))

(defun mpvi-call-process (program &rest args)
  "Helper for `call-process', PROGRAM and ARGS are the same."
  (mpvi-log ">>> %s %s" program
            (mapconcat (lambda (a) (shell-quote-argument a)) args " "))
  (apply #'call-process program nil t nil args))

(defun mpvi-url-p (url)
  "Return if URL is an URL."
  (member (url-type (url-generic-parse-url url)) '("http" "https")))

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

(defun mpvi-read-file-name (prompt default-name)
  "Read file name using a PROMPT minibuffer.
DEFAULT-NAME is used when only get a directory name."
  (let* ((default-directory (or mpvi-last-save-directory default-directory))
         (target (read-file-name prompt)))
    (if (directory-name-p target)
        (expand-file-name (file-name-nondirectory default-name) target)
      (expand-file-name target))))

(defun mpvi-time-to-secs (time)
  "Convert TIME to seconds format."
  (require 'org-timer)
  (cond ((or (null time) (numberp time)) time)
        ((or (not (stringp time)) (not (string-match-p "^-?[0-9:.]+$" time)))
         (user-error "This is not a valid time: %s" time))
        ((cl-find ?: time)
         (+ (org-timer-hms-to-secs (org-timer-fix-incomplete time))
            (if-let (p (cl-search "." time)) (string-to-number (cl-subseq time p)) 0)))
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

;; MPV

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
                   (if-let ((dest (apply #'mpvi-extract-url  ; dispatch to method
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
                                 (concat "Playlist" (if-let (title (alist-get 'title meta)) (format " (%s)" title))  ": ")
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
  (unless (eq emms-player-mpv-ipc-method 'ipc-server)
    (user-error "You should update MPV to support ipc connect")))

(defun mpvi-origin-path (&optional path)
  "Reverse of `mpvi-extract-url', return the origin url for PATH.
When PATH is nil then return the path of current playing video."
  (unless path
    (mpvi-check-live)
    (setq path (mpvi-cmd `(get_property path))))
  (or (plist-get mpvi-current-url-metadata :origin-url) path))

(defun mpvi-cmd (cmd)
  "Request MPV for CMD. This is sync version of `emms-player-mpv-cmd'."
  (when (emms-player-mpv-proc-playing-p)
    (catch 'mpvi-ret
      (emms-player-mpv-cmd cmd (lambda (data _err)
                                 (ignore-errors
                                   (throw 'mpvi-ret data))))
      (while (emms-player-mpv-proc-playing-p) (sleep-for 0.05))
      (throw 'mpvi-ret nil))))

(defalias 'mpvi-async-cmd #'emms-player-mpv-cmd)

(cl-defun mpvi-prop (sym &optional (val nil supplied))
  "Run command set_property SYM VAL in MPV.
Run get_property instead if VAL is absent."
  (if supplied
      (mpvi-async-cmd `(set_property ,sym ,val))
    (mpvi-cmd `(get_property ,sym))))

(defun mpvi-pause (&optional how)
  "Set or toggle pause state of MPV.
HOW is :json-false or t that returned by get-property.
Toggle pause if HOW is nil."
  (interactive)
  (mpvi-async-cmd
   (if how
       `(set pause ,(if (eq how :json-false) 'no 'yes))
     `(cycle pause))))

(defun mpvi-seekable (&optional arg)
  "Whether current video is seekable.
Alert user when not seekable when ARG not nil."
  (let ((seekable (eq (mpvi-prop 'seekable) t)))
    (if (and arg (not seekable))
        (user-error "Current video is not seekable, do nothing")
      seekable)))

(defun mpvi-speed (&optional n)
  "Tune the speed base on N."
  (mpvi-seekable 'assert)
  (pcase n
    ('nil (mpvi-prop 'speed 1)) ; reset
    ((pred numberp)
     (let ((factor (* 1.1 n)))
       (mpvi-async-cmd `(multiply speed ,(if (>= n 0) factor (/ -1 factor))))))
    (_ (mpvi-prop 'speed (read-from-minibuffer "Speed to: " n nil t)))))

(defcustom mpvi-post-play-cmds nil
  "Command list let MPV process run after loading a file.
See `emms-player-mpv-cmd' for syntax."
  :type 'list)

(cl-defun mpvi-play (path &optional (beg 0) end emms)
  "Play PATH from BEG to END.
EMMS is a flag that this is invoked from EMMS."
  (if (mpvi-url-p path)
      (unless (executable-find "yt-dlp")
        (user-error "You should have 'yt-dlp' installed to play remote url"))
    (setq path (expand-file-name path)))
  (if (and (emms-player-mpv-proc-playing-p) (equal path (mpvi-origin-path)))
      ;; when path is current playing, just seek to position
      (when (mpvi-seekable)
        (mpvi-prop 'ab-loop-a (if end beg "no"))
        (mpvi-prop 'ab-loop-b (or end "no"))
        (mpvi-prop 'playback-time beg)
        (mpvi-prop 'pause 'no))
    ;; start and loadfile
    (message "Waiting %s..." path)
    (if (emms-player-mpv-proc-playing-p) (ignore-errors (mpvi-pause t)))
    ;; If path is not the current playing, load it
    (let (logo title subfile opts cmds started)
      (when (mpvi-url-p path)
        ;; preprocessing url and extra mpv commands
        (when-let ((ret (mpvi-extract-url nil path)))
          (unless (plist-get ret :url) (user-error "Unknown url"))
          (setq mpvi-current-url-metadata ret)
          (setq path (or (plist-get ret :url) path))
          (setq logo (plist-get ret :logo))
          (setq title (plist-get ret :title))
          (setq subfile (plist-get ret :subfile))
          (setq opts (plist-get ret :opts))
          (setq cmds (plist-get ret :cmds))
          (setq started (plist-get ret :started))))
      (setq opts
            `((start . ,beg)
              ,@(when end
                  `((ab-loop-a . ,beg)
                    (ab-loop-b . ,end)))
              ,(when title
                 `(force-media-title . ,(format "\"%s\"" title)))
              ,(when subfile
                 `(sub-file . ,(format "\"%s\"" subfile)))
              ,@opts))
      (mpvi-log "load opts: %S" opts)
      (let* ((lst `(((set_property speed 1))
                    ((set_property keep-open ,(if emms 'no 'yes)))
                    ((loadfile ,path replace
                               ,(mapconcat (lambda (x)
                                             (format "%s=%s" (car x) (cdr x)))
                                           (delq nil opts) ","))
                     . ,(lambda (_ err)
                          (if err
                              (message "Load video failed (%S)" err)
                            (if started
                                (funcall started)
                              (message "Started%s"
                                       (if title
                                           (concat (if logo (concat "/" logo)) ": "
                                                   (propertize title 'face 'font-lock-keyword-face))
                                         ".")))
                            (push path mpvi-play-history))))
                    ((set_property pause no))
                    ,@(cl-loop for c in `(,@cmds ,@mpvi-post-play-cmds)
                               if (car-safe (car c)) collect c
                               else collect (list c))))
             (cmd (cons 'batch (delq nil lst))))
        (mpvi-log "load-commands: %S" cmd)
        (if (and emms (emms-player-mpv-proc-playing-p)) (emms-player-mpv-stop))
        (mpvi-async-cmd cmd)))))

;; Timestamp Link

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
  (let ((node (cadr (org-element-context))))
    (when (equal "mpv" (plist-get node :type))
      (let ((meta (mpvi-parse-link (plist-get node :path)))
            (end (save-excursion (goto-char (plist-get node :end)) (skip-chars-backward " \t") (point))))
        `(:path ,(car meta) :vbeg ,(cadr meta) :vend ,(caddr meta) :end ,end ,@node)))))

(defun mpvi-build-mpv-link (path &optional beg end desc)
  "Build mpv link with timestamp that used in org buffer.
PATH is local video file or remote url. BEG and END is the position number.
DESC is optional, used to describe the current timestamp link."
  (concat "[[mpv:" path (if (or beg end) "#")
          (if beg (number-to-string beg))
          (if end "-")
          (if end (number-to-string end))
          "][▶ "
          (if beg (mpvi-secs-to-hms beg nil t))
          (if end " → ")
          (if end (mpvi-secs-to-hms end nil t))
          "]]"
          (if desc (concat " " desc))))

(defcustom mpvi-attach-link-attrs "#+attr_html: :width 666"
  "Attrs insert above a inserted attach image.
The :width can make image cannot display too large in org mode."
  :type 'string)

(defun mpvi-insert-attach-link (file)
  "Save image FILE to org file using `org-attach'."
  (require 'org-attach)
  ;; attach it
  (let ((org-attach-method 'mv)) (org-attach-attach file))
  ;; insert the attrs
  (when mpvi-attach-link-attrs
    (insert (string-trim mpvi-attach-link-attrs) "\n"))
  ;; insert the link
  (insert "[[attachment:" (file-name-base file) "." (file-name-extension file) "]]")
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

;; screenshot

(defvar mpvi-clipboard-command
  (cond ((executable-find "xclip")
         ;; A hangs issue:
         ;; https://www.reddit.com/r/emacs/comments/da9h10/why_does_shellcommand_hang_using_xclip_filter_to/
         "xclip -selection clipboard -t image/png -filter < \"%s\" &>/dev/null")
        ((and (executable-find "powershell") (memq system-type '(cygwin windows-nt)))
         "powershell -Command \"Add-Type -AssemblyName System.Windows.Forms; [Windows.Forms.Clipboard]::SetImage($([System.Drawing.Image]::Fromfile(\\\"%s\\\")))\"")))

(defun mpvi-image-to-clipboard (image-file)
  "Save IMAGE-FILE data to system clipboard.
I don't know whether better solutions exist."
  (if (and mpvi-clipboard-command (file-exists-p image-file))
      (let ((command (format mpvi-clipboard-command (shell-quote-argument image-file))))
        (mpvi-log "Copy image to clipboard: %s" command)
        (shell-command command))
    (user-error "Nothing to do with copy image file")))

(defun mpvi-screenshot (path pos &optional target)
  "Capture the screenshot of PATH at POS and save to TARGET."
  (unless (mpvi-url-p path)
    (setq path (expand-file-name path)))
  (setq target
        (if target (expand-file-name target)
          (expand-file-name (format-time-string "IMG-%s.png") mpvi-cache-directory)))
  (with-temp-buffer
    (if (zerop (call-process "mpv" nil nil nil path
                             "--no-terminal" "--no-audio" "--vo=image" "--frames=1"
                             (format "--start=%s" (or pos 0))
                             "-o" target))
        target
      (user-error "Capture failed: %s" (string-trim (buffer-string))))))

(defun mpvi-screenshot-current-playing (&optional target flag)
  "Capture screenshot from current playing mpv and save to TARGET.
If TARGET is nil save to temporary directory, if it is t save to clipboard.
If FLAG is string, pass directly to mpv as <flags> of screenshot-to-file, if
it is nil pass \"video\" as default, else prompt user to choose one."
  (mpvi-check-live)
  (let ((file (if (stringp target)
                  (expand-file-name target)
                (expand-file-name (format-time-string "IMG-%s.png") mpvi-cache-directory)))
        (flags (list "video" "subtitles" "window")))
    (unless (or (null flag) (stringp flag))
      (setq flag (completing-read "Flag of screenshot: " flags nil t)))
    (unless (member flag flags) (setq flag "video"))
    (mpvi-cmd `(screenshot-to-file ,file ,flag))
    (if (eq target t) ; if filename is t save data to clipboard
        (mpvi-image-to-clipboard file)
      (prog1 file (kill-new file)))))

;; tesseract

(defcustom mpvi-tesseract-args "-l chi_sim"
  "Extra options pass to `tesseract'."
  :type 'string)

(defun mpvi-ocr-by-tesseract (file)
  "Run tesseract OCR on the screenshot FILE."
  (unless (executable-find "tesseract")
    (user-error "Program `tesseract' not found"))
  (with-temp-buffer
    (if (zerop (apply #'mpvi-call-process "tesseract" file "stdout"
                      (if mpvi-tesseract-args (split-string-and-unquote mpvi-tesseract-args))))
        (buffer-string)
      (user-error "OCR tesseract failed: %s" (string-trim (buffer-string))))))

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
           (ext (if-let ((fmt (cl-find-if (lambda (c) (equal (cadr c) format)) fmts)))
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
           "-o" (or prefix (expand-file-name "SUB-%(fulltitle)s" mpvi-cache-directory))
           (split-string-and-unquote (or opts mpvi-ytdlp-extra-args "")))
    (goto-char (point-min))
    (if (re-search-forward "Destination:\\(.*\\)$" nil t)
        (string-trim (match-string 1))
      (user-error "Error when download subtitle: %s" (string-trim (buffer-string))))))


;;; Patch `emms-player-mpv.el' for better integrated
;;
;; 1) Emacs don't have builtin way of connecting to Windows named pipe server
;;
;;    - Should improve `make-network-process' to support this. Here solved by PowerShell
;;
;; 2) Some MPV events like 'end-file/playback-restart' not triggered as expected on Windows (BUG?),
;;    So some logics in `emms-player-mpv-event-handler' are not working.
;;
;;    - Maybe should improve MPV for Windows. Here workaround by adding some ugly patches in EMMS
;;
;; 3) The APIs in `emms-player-mpv.el' are too tightly tied to EMMS playlist
;;
;;    - Maybe should refactor the APIs to make them can be used standalone, that is, can connect
;;      to MPV and play videos without having to update EMMS playlist and so on
;;

;; Windows support, implement by PowerShell

(defun mpvi-emms-player-mpv-ipc-init (func)
  "Advice for FUNC `emms-player-mpv-ipc-init', add Windows support."
  (if (eq system-type 'windows-nt)
      (mpvi-connect-to-win-named-pipe emms-player-mpv-ipc-socket)
    (funcall func)))

(defun mpvi-emms-player-mpv-ipc-recv (json-string)
  "Advice for `emms-player-mpv-ipc-recv', patch for output of PowerShell.
JSON-STRING is json format string return by ipc process."
  (emms-player-mpv-debug-msg "json << %s" json-string)
  (let (json)
    (condition-case err
        (setq json (json-read-from-string json-string))
      ;; PowerShell will output error message when something goes wrong to standard output,
      ;; It's not json format, so catch it here
      (error (erase-buffer) (user-error "ERR in IPC-RECV: %s\n------\n%s" err json-string)))
    (let ((rid (alist-get 'request_id json)))
      (when (and rid (not (alist-get 'command json))) ; skip the echoed 'command' for Windows
        (emms-player-mpv-ipc-req-resolve
         rid (alist-get 'data json) (alist-get 'error json)))
      (when (alist-get 'event json)
        (emms-player-mpv-event-handler json)
        ;; Only call the hook when video is played from EMMS
        (when (emms-playlist-current-selected-track)
          (run-hook-with-args 'emms-player-mpv-event-functions json))))))

(defun mpvi-emms-player-mpv-event-handler (func json-data)
  "Advice for FUNC `mpvi-emms-player-mpv-event-handler', workaround for Windows.
JSON-DATA is argument."
  (when (eq system-type 'windows-nt)
    (pcase (alist-get 'event json-data)
      ("start-file" ; playback-restart event not working in Windows
       (unless (emms-player-mpv-proc-playing-p)
         (emms-player-mpv-proc-playing t)
         (emms-player-started emms-player-mpv))
       (emms-player-mpv-event-playing-time-sync))))
  (funcall func json-data))

(defun mpvi-emms-player-mpv-force-stop (&rest _)
  "Advice for `emms-player-mpv-proc-sentinel' and `emms-player-mpv-stop'."
  ;; Event 'end-file' is not working correctly on Windows! So have a try like this..
  (when (eq system-type 'windows-nt)
    (emms-player-mpv-proc-stop)
    (emms-player-mpv-ipc-stop)
    (emms-player-mpv-proc-playing nil)))

(defun mpvi-connect-to-win-named-pipe (pipename)
  "Connect to MPV by PIPENAME via `PowerShell'."
  (emms-player-mpv-ipc-stop)
  (emms-player-mpv-debug-msg "ipc: init for windows")
  (with-current-buffer (get-buffer-create emms-player-mpv-ipc-buffer)
    (erase-buffer))
  (setq emms-player-mpv-ipc-id 1
        emms-player-mpv-ipc-req-table nil)
  (setq pipename (string-replace "/" "\\" pipename)) ; path seperator on Windows is different
  (with-timeout (5 (emms-player-mpv-ipc-stop)
                   (user-error "No MPV process found"))
    (while (not (mpvi-win-named-pipe-exists-p pipename))
      (sleep-for 0.05)))
  (let* ((ps1 " $conn = [System.IO.Pipes.NamedPipeClientStream]::new('.', '%s');
                try {
                  $reader = [System.IO.StreamReader]::new($conn);
                  $writer = [System.IO.StreamWriter]::new($conn);
                  $conn.Connect(5000);
                  while (1) {
                    $msg = Read-Host;
                    $writer.WriteLine($msg);
                    $writer.Flush();
                    $conn.WaitForPipeDrain();
                    do {
                      $ret = $reader.ReadLine();
                      Write-Host $ret;
                    } while ($ret -match '\"event\":');
                  }
                }
                catch [System.TimeoutException], [System.InvalidOperationException] { Write-Host 'Connect to MPV failed'; }
                catch { Write-Host $_; }
                finally { $conn.Dispose(); } ")
         (cmd (format "& {%s}" (replace-regexp-in-string
                                "[ \n\r\t]+" " " (format ps1 pipename))))
         (proc (make-process :name "emms-player-mpv-ipc"
                             :connection-type 'pipe
                             :buffer (get-buffer-create emms-player-mpv-ipc-buffer)
                             :noquery t
                             :filter #'emms-player-mpv-ipc-filter
                             :sentinel #'emms-player-mpv-ipc-sentinel
                             :command (list "powershell" "-NoProfile" "-Command" cmd))))
    (with-timeout (5 (setq emms-player-mpv-ipc-proc nil)
                     (user-error "Connect to MPV failed"))
      (while (not (eq (process-status emms-player-mpv-proc) 'run))
        (sleep-for 0.05)))
    (setq emms-player-mpv-ipc-proc proc)))

(defun mpvi-win-named-pipe-exists-p (pipename)
  "Check if named pipe with name of PIPENAME exists on Windows."
  (unless (executable-find "powershell")
    (user-error "Cannot find PowerShell"))
  (with-temp-buffer
    (call-process "powershell" nil t nil
                  "-Command"
                  (format "& {Get-ChildItem \\\\.\\pipe\\ | Where-Object {$_.Name -eq '%s'}}"
                          pipename))
    (> (length (buffer-string)) 0)))

;; Only update track when videos are played from EMMS buffer

(defun mpvi-emms-player-started (player)
  "Advice for `emms-player-started', PLAYER is the current player."
  (setq emms-player-playing-p player
        emms-player-paused-p nil)
  (when (emms-playlist-current-selected-track) ; add this
    (run-hooks 'emms-player-started-hook)))

(defun mpvi-emms-player-stopped ()
  "Advice for `emms-player-stopped'."
  (setq emms-player-playing-p nil)
  (when (emms-playlist-current-selected-track) ; add this
    (if emms-player-stopped-p
        (run-hooks 'emms-player-stopped-hook)
      (sleep-for emms-player-delay)
      (run-hooks 'emms-player-finished-hook)
      (funcall emms-player-next-function))))

;; Integrate `emms-player-start' with `mpvi-play'

(defun mpvi-emms-player-mpv-start (track)
  "Play TRACK in EMMS. Integrate with `mpvi-play'."
  (setq emms-player-mpv-stopped nil)
  (emms-player-mpv-proc-playing nil)
  (let ((track-name (emms-track-get track 'name))
        (track-playlist-option
         (and emms-player-mpv-use-playlist-option
              (memq (emms-track-get track 'type)
                    '(streamlist playlist)))))
    (if (emms-player-mpv-ipc-fifo-p)
        (progn ;; ipc-stop is to clear any buffered commands
          (emms-player-mpv-ipc-stop)
          (apply #'emms-player-mpv-proc-init
                 (if track-playlist-option
                     (list (concat "--playlist=" track-name))
                   (list "--" track-name)))
          (emms-player-started emms-player-mpv))
      (let ((start-func (lambda () (mpvi-play track-name nil nil t)))) ; <- change this
        (if (and (not (eq system-type 'windows-nt)) ; pity, auto switch next not working on Windows
                 emms-player-mpv-ipc-stop-command)
            (setq emms-player-mpv-ipc-stop-command start-func)
          (funcall start-func))))))

;; Minor mode

;;;###autoload
(define-minor-mode mpvi-emms-integrated-mode
  "Global minor mode to toggle EMMS integration."
  :global t
  (if mpvi-emms-integrated-mode
      (progn
        (advice-add #'emms-player-mpv-ipc-init :around #'mpvi-emms-player-mpv-ipc-init)
        (advice-add #'emms-player-mpv-ipc-recv :override #'mpvi-emms-player-mpv-ipc-recv)
        (advice-add #'emms-player-mpv-event-handler :around #'mpvi-emms-player-mpv-event-handler)
        (advice-add #'emms-player-mpv-proc-sentinel :after #'mpvi-emms-player-mpv-force-stop)
        ;;
        (advice-add #'emms-player-started :override #'mpvi-emms-player-started)
        (advice-add #'emms-player-stopped :override #'mpvi-emms-player-stopped)
        ;;
        (advice-add #'emms-player-mpv-start :override #'mpvi-emms-player-mpv-start)
        (advice-add #'emms-player-mpv-stop :after #'mpvi-emms-player-mpv-force-stop))
    (advice-remove #'emms-player-mpv-ipc-init #'mpvi-emms-player-mpv-ipc-init)
    (advice-remove #'emms-player-mpv-ipc-recv #'mpvi-emms-player-mpv-ipc-recv)
    (advice-remove #'emms-player-mpv-event-handler #'mpvi-emms-player-mpv-event-handler)
    (advice-remove #'emms-player-mpv-proc-sentinel #'mpvi-emms-player-mpv-force-stop)
    (advice-remove #'emms-player-started #'mpvi-emms-player-started)
    (advice-remove #'emms-player-stopped #'mpvi-emms-player-stopped)
    (advice-remove #'emms-player-mpv-start #'mpvi-emms-player-mpv-start)
    (advice-remove #'emms-player-mpv-stop #'mpvi-emms-player-mpv-force-stop)))

(mpvi-emms-integrated-mode 1)


;;; Interactive Commands

;; [open]

(defcustom mpvi-favor-paths nil
  "Your favor video path list.
Item should be a path string or a cons.

For example:

  \\='(\"~/video/aaa.mp4\"
    \"https://www.youtube.com/watch?v=NQXA\"
    (\"https://www.douyu.com/110\" . \"some description\"))

This can be used by `mpvi-open-from-favors' to quick open video."
  :type 'list)

(defvar mpvi-open-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-x b") #'mpvi-open-from-favors)
    (define-key map (kbd "C-x <return>") (lambda () (interactive) (throw 'mpvi-open (list (minibuffer-contents) 'add))))
    (define-key map (kbd "C-x C-w") (lambda () (interactive) (throw 'mpvi-open (list (minibuffer-contents) 'dup))))
    map))

;;;###autoload
(defun mpvi-open (path &optional act)
  "Deal with PATH, which is a local video or remote url.
Play the video if ACT is nil or play, add to EMMS if ACT is add,
clip the video if ACT is dup.
Keybind `C-x RET' to add to playlist.
Keybind `C-x b' to choose video path from `mpvi-favor-paths'."
  (interactive (catch 'mpvi-open
                 (minibuffer-with-setup-hook
                     (lambda ()
                       (use-local-map (make-composed-keymap (list (current-local-map) mpvi-open-map))))
                   (list (unwind-protect
                             (catch 'ffap-prompter
                               (ffap-read-file-or-url
                                "Playing video (file or url): "
                                (prog1 (mpvi-ffap-guesser) (ffap-highlight))))
                           (ffap-highlight t))))))
  (unless (and (> (length path) 0) (or (mpvi-url-p path) (file-exists-p path)))
    (user-error "Not correct file or url"))
  (prog1 (setq path (if (mpvi-url-p path) path (expand-file-name path)))
    (cond
     ((or (null act) (equal act 'play))
      (setq mpvi-current-url-metadata nil)
      (with-current-emms-playlist (setq emms-playlist-selected-marker nil))
      (mpvi-play path))
     ((equal act 'add)
      (mpvi-emms-add path))
     ((equal act 'dup)
      (if (mpvi-url-p path)
          (mpvi-ytdlp-download path)
        (mpvi-convert-by-ffmpeg path))))))

;;;###autoload
(defun mpvi-open-from-favors ()
  "Choose video from `mpvi-favor-paths' and play it."
  (interactive)
  (unless (consp mpvi-favor-paths)
    (user-error "You should add your favor paths into `mpvi-favor-paths' first"))
  (let* ((annfn (lambda (it)
                  (when-let (s (alist-get it mpvi-favor-paths))
                    (format "    (%s)" s))))
         (path (completing-read "Choose video to play: "
                                (lambda (input pred action)
                                  (if (eq action 'metadata)
                                      `(metadata (display-sort-function . ,#'identity)
                                                 (annotation-function . ,annfn))
                                    (complete-with-action action mpvi-favor-paths input pred)))
                                nil t)))
    ;; called directly vs called from minibuffer
    (if (= (recursion-depth) 0)
        (mpvi-open path)
      (throw 'mpvi-open (list path 'play)))))

;; [seek]

(defvar mpvi-seek-paused nil)

(defvar mpvi-seek-overlay nil)

(defvar mpvi-seek-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "i")   #'mpvi-seeking-insert)
    (define-key map (kbd "g")   #'mpvi-seeking-revert)
    (define-key map (kbd "n")   (lambda () (interactive) (mpvi-seeking-walk 1)))
    (define-key map (kbd "p")   (lambda () (interactive) (mpvi-seeking-walk -1)))
    (define-key map (kbd "N")   (lambda () (interactive) (mpvi-seeking-walk "1%")))
    (define-key map (kbd "P")   (lambda () (interactive) (mpvi-seeking-walk "-1%")))
    (define-key map (kbd "M-n") (lambda () (interactive) (mpvi-seeking-walk :ff)))
    (define-key map (kbd "M-p") (lambda () (interactive) (mpvi-seeking-walk :fb)))
    (define-key map (kbd "C-l") (lambda () (interactive) (mpvi-seeking-walk 0)))
    (define-key map (kbd "C-n") (lambda () (interactive) (mpvi-seeking-walk 1)))
    (define-key map (kbd "C-p") (lambda () (interactive) (mpvi-seeking-walk -1)))
    (define-key map (kbd "M-<") (lambda () (interactive) (mpvi-seeking-revert 0)))
    (define-key map (kbd "k")   (lambda () (interactive) (mpvi-speed 1)))
    (define-key map (kbd "j")   (lambda () (interactive) (mpvi-speed -1)))
    (define-key map (kbd "l")   (lambda () (interactive) (mpvi-speed nil)))
    (define-key map (kbd "v")   #'mpvi-current-playing-switch-playlist)
    (define-key map (kbd "C-v") #'mpvi-current-playing-switch-playlist)
    (define-key map (kbd "c")   #'mpvi-seeking-clip)
    (define-key map (kbd "C-c") #'mpvi-seeking-clip)
    (define-key map (kbd "s")   #'mpvi-seeking-capture-save-as)
    (define-key map (kbd "C-s") #'mpvi-seeking-capture-to-clipboard)
    (define-key map (kbd "C-i") #'mpvi-seeking-capture-as-attach)
    (define-key map (kbd "r")   #'mpvi-seeking-ocr-to-kill-ring)
    (define-key map (kbd "C-r") #'mpvi-seeking-ocr-to-kill-ring)
    (define-key map (kbd "t")   #'mpvi-seeking-copy-sub-text)
    (define-key map (kbd "C-t") #'mpvi-seeking-copy-sub-text)
    (define-key map (kbd "T")   #'mpvi-current-playing-load-subtitle)
    (define-key map (kbd "SPC") #'mpvi-seeking-pause)
    (define-key map (kbd "o")   #'mpvi-current-playing-open-externally)
    (define-key map (kbd "C-o") #'mpvi-current-playing-open-externally)
    (define-key map (kbd "q")   #'abort-minibuffers)
    (define-key map (kbd "C-q") #'abort-minibuffers)
    map))

(defvar mpvi-seek-annotation-alist
  '((if (eq (mpvi-prop 'loop) t) "Looping")
    (if (eq (mpvi-prop 'pause) t) "Paused")
    ("Speed" . (format "%.2f" (mpvi-prop 'speed)))
    ("Total" . (mpvi-secs-to-hms (mpvi-prop 'duration) nil t)))
  "The items displayed in the minibuffer when `mpvi-seek-refresh-annotation'.")

(defun mpvi-seek-refresh-annotation ()
  "Show information of the current playing in minibuffer."
  (if mpvi-seek-overlay (delete-overlay mpvi-seek-overlay))
  (let ((kf (lambda (s) (if s (format " %s:" s))))
        (vf (lambda (s) (if s (propertize (format " %s " s) 'face mpvi-annotation-face))))
        (sf (lambda (s) (propertize " " 'display `(space :align-to (- right-fringe ,(1+ (length s))))))) ; space
        (ov (make-overlay (point-max) (point-max) nil t t)))
    (overlay-put ov 'intangible t)
    (setq mpvi-seek-overlay ov)
    (if (mpvi-seekable)
        (condition-case nil
            (let* ((hms (when-let (s (ignore-errors (mpvi-secs-to-hms (string-to-number (minibuffer-contents)))))
                          (funcall vf (format "%s  %.2f%% " s (mpvi-prop 'percent-pos)))))
                   (text (cl-loop for i in mpvi-seek-annotation-alist
                                  if (stringp (car i)) concat (concat (funcall kf (car i)) " " (funcall vf (eval (cdr i))))
                                  else concat (funcall vf (eval i))))
                   (space (funcall sf (concat hms text))))
              (overlay-put ov 'before-string (propertize (concat space hms text) 'cursor t)))
          (error nil))
      (let* ((title (funcall vf (concat "        >> " (string-trim (or (mpvi-prop 'media-title) "")))))
             (state (funcall vf (if (eq (mpvi-prop 'pause) t) "Paused")))
             (space (funcall sf state)))
        (delete-minibuffer-contents) (insert "0")
        (overlay-put ov 'before-string (propertize (concat title space state) 'cursor t))))))

;;;###autoload
(defun mpvi-seek (&optional pos prompt)
  "Interactively seek POS for current playing video.
PROMPT is used if non-nil for `minibuffer-prompt'."
  (interactive)
  (mpvi-check-live)
  (let ((paused (mpvi-prop 'pause))
        (keep-open (mpvi-prop 'keep-open)))
    (mpvi-pause t)
    (mpvi-prop 'keep-open 'yes) ; dont close on end
    (unwind-protect
        (when-let
            ((ret
              (catch 'mpvi-seek
                (minibuffer-with-setup-hook
                    (lambda ()
                      (add-hook 'after-change-functions
                                (lambda (start end _)
                                  (when (or (not (string-match-p "^[0-9]+\\.?[0-9]*$" (buffer-substring start end)))
                                            (not (<= 0 (string-to-number (minibuffer-contents)) (mpvi-prop 'duration))))
                                    (delete-region start end)))
                                nil t)
                      (add-hook 'post-command-hook #'mpvi-seek-refresh-annotation nil t))
                  (ignore-errors
                    (read-from-minibuffer
                     (or prompt (if (mpvi-seekable)
                                    (format "MPV Seek (0-%d): " (mpvi-prop 'duration))
                                  "MPV Controller: "))
                     (number-to-string (or pos (mpvi-prop 'playback-time)))
                     mpvi-seek-map t 'mpvi-seek-hist))))))
          (cond ((stringp ret) (message "%s" ret))
                ((eq (mpvi-prop 'pause) :json-false))
                ((and (mpvi-seekable) (numberp ret))
                 (mpvi-prop 'playback-time ret)))
          (cons (ignore-errors (mpvi-prop 'playback-time)) paused))
      (mpvi-pause (or mpvi-seek-paused paused))
      (mpvi-prop 'keep-open (if (eq keep-open :json-false) 'no 'yes)))))

(defun mpvi-seeking-walk (offset)
  "Seek forward or backward with factor of OFFSET.
If OFFSET is number then step by seconds.
If OFFSET is xx% format then step by percent.
If OFFSET is :ff or :fb then step forward/backward one frame."
  (pcase offset
    (:ff (mpvi-cmd `(frame_step)))
    (:fb (mpvi-cmd `(frame_back_step)))
    (_
     (when (and (stringp offset) (string-match-p "^-?[0-9]\\{0,2\\}\\.?[0-9]*%$" offset)) ; percent
       (setq offset (* (/ (string-to-number (cl-subseq offset 0 -1)) 100.0) (mpvi-prop 'duration))))
     (unless (numberp offset) (setq offset 1))
     (let* ((old (if (or (zerop offset) (eq (mpvi-prop 'pause) t))
                     (let ((str (string-trim (minibuffer-contents))))
                       (unless (string-match-p "^[0-9]+\\(\\.[0-9]+\\)?$" str)
                         (user-error "Not valid number"))
                       (string-to-number str))
                   (mpvi-prop 'playback-time)))
            (new (+ old offset))
            (total (mpvi-prop 'duration)))
       (if (< new 0) (setq new 0))
       (if (> new total) (setq new total))
       (unless (= old new)
         (delete-minibuffer-contents)
         (insert (mpvi-secs-to-string new)))
       (mpvi-prop 'playback-time new))))
  (mpvi-seeking-revert))

(defun mpvi-seeking-revert (&optional num)
  "Insert current playback-time to minibuffer.
If NUM is not nil, go back that position first."
  (interactive)
  (when (and num (mpvi-seekable))
    (mpvi-prop 'playback-time num))
  (delete-minibuffer-contents)
  (insert (mpvi-secs-to-string (mpvi-prop 'playback-time))))

(defun mpvi-seeking-pause ()
  "Revert and pause."
  (interactive)
  (mpvi-async-cmd `(cycle pause))
  (setq mpvi-seek-paused (eq (mpvi-prop 'pause) t))
  (when mpvi-seek-paused (mpvi-seeking-revert)))

(defun mpvi-seeking-insert ()
  "Insert new link in minibuffer seek."
  (interactive)
  (mpvi-seekable 'assert)
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (let ((paused (mpvi-prop 'pause)))
      (mpvi-pause t)
      (unwind-protect
          (if (derived-mode-p 'org-mode)
              (let* ((desc (string-trim (read-string "Notes: ")))
                     (link (funcall mpvi-build-link-function
                                    (mpvi-origin-path)
                                    (mpvi-prop 'playback-time)
                                    nil desc)))
                (cond ((org-at-item-p) (end-of-line) (org-insert-item))
                      (t               (end-of-line) (insert "\n")))
                (set-window-point (get-buffer-window) (point))
                (save-excursion (insert link)))
            (user-error "This is not org-mode, should not insert timestamp link"))
        (mpvi-pause paused))))
  (mpvi-seeking-revert))

(defun mpvi-seeking-clip ()
  "Download/Clip current playing video."
  (interactive)
  (let ((path (mpvi-prop 'path)))
    (funcall (if (mpvi-url-p path) mpvi-remote-video-handler mpvi-local-video-handler) path))
  (throw 'mpvi-seek nil))

(defun mpvi-seeking-copy-sub-text ()
  "Copy current sub text to kill ring."
  (interactive)
  (when-let ((sub (ignore-errors (mpvi-prop 'sub-text))))
    (kill-new sub)
    (throw 'mpvi-seek "Copied to kill ring, yank to the place you want.")))

(defun mpvi-seeking-capture-save-as ()
  "Capture current screenshot and prompt to save."
  (interactive)
  (let ((target (mpvi-read-file-name "Screenshot save to: " (format-time-string "mpv-%F-%X.png"))))
    (make-directory (file-name-directory target) t)
    (mpvi-screenshot-current-playing target current-prefix-arg)
    (throw 'mpvi-seek (format "Captured to %s" target))))

(defun mpvi-seeking-capture-to-clipboard ()
  "Capture current screenshot and save to clipboard."
  (interactive)
  (mpvi-screenshot-current-playing t current-prefix-arg)
  (throw 'mpvi-seek "Screenshot is in clipboard, paste to use"))

(defun mpvi-seeking-capture-as-attach ()
  "Capture current screenshot and insert as attach link."
  (interactive)
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (unless (derived-mode-p 'org-mode)
      (user-error "This is not org-mode, should not insert org link")))
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (when (mpvi-parse-link-at-point)
      (end-of-line) (insert "\n"))
    (mpvi-insert-attach-link (mpvi-screenshot-current-playing nil current-prefix-arg)))
  (throw 'mpvi-seek "Capture and insert done."))

(defun mpvi-seeking-ocr-to-kill-ring ()
  "OCR current screenshot and save the result into kill ring."
  (interactive)
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (let ((ret (funcall mpvi-ocr-function (mpvi-screenshot-current-playing))))
      (kill-new ret)))
  (throw 'mpvi-seek "OCR done into kill ring, please yank it."))

(defun mpvi-current-playing-switch-playlist ()
  "Extract playlist from current video url.
If any, prompt user to choose one video in playlist to play."
  (interactive)
  (mpvi-check-live)
  (if-let ((playlist (plist-get mpvi-current-url-metadata :playlist-url))
           (playlist-index (plist-get mpvi-current-url-metadata :playlist-index))
           (msg "Switch done."))
      (condition-case nil
          (throw 'mpvi-seek (prog1 msg (mpvi-play playlist)))
        (error (message msg)))
    (user-error "No playlist found for current playing url")))

(defun mpvi-current-playing-load-subtitle (subfile)
  "Load or reload the SUBFILE for current playing video."
  (interactive (list (read-file-name "Danmaku file: " mpvi-cache-directory nil t)))
  (mpvi-check-live)
  (cl-assert (file-regular-p subfile))
  (when (string-suffix-p ".danmaku.xml" subfile) ; bilibili
    (require 'mpvi-ps)
    (setq subfile (mpvi-convert-danmaku2ass subfile 'confirm)))
  (ignore-errors (mpvi-async-cmd `(sub-remove)))
  (mpvi-async-cmd `(sub-add ,subfile))
  (message "Sub file loaded!"))

(defun mpvi-current-playing-open-externally ()
  "Open current playing video PATH with system program."
  (interactive)
  (mpvi-check-live)
  (if-let ((path (mpvi-origin-path)))
      (let ((called-from-seek (> (recursion-depth) 0)))
        (if (or (not called-from-seek)
                (y-or-n-p (format "Open '%s' externally?" path)))
            (let ((msg "Open in system program done."))
              ;; add begin time for url if necessary
              (when-let (f (plist-get mpvi-current-url-metadata :out-url-decorator))
                (setq path (funcall f path (mpvi-prop 'playback-time))))
              (browse-url path)
              (if called-from-seek
                  (progn (setq mpvi-seek-paused t)
                         (throw 'mpvi-seek msg))
                (mpvi-pause t)
                (message msg)))
          (message "")))
    (user-error "No playing path found")))

;; [others]

;;;###autoload
(defun mpvi-insert (&optional prompt)
  "Insert a mpv link or update a mpv link at point.
PROMPT is used in minibuffer when invoke `mpvi-seek'."
  (interactive "P")
  (if (derived-mode-p 'org-mode)
      (let ((path (mpvi-origin-path)) description)
        (unless (mpvi-seekable)
          (user-error "Current video is not seekable, it makes no sense to insert timestamp link"))
        (mpvi-with-current-mpv-link (node path)
          (when-let (ret (mpvi-seek (if node (plist-get node :vbeg)) prompt))
            (mpvi-pause t)
            ;; if on a mpv link, update it
            (if node (delete-region (plist-get node :begin) (plist-get node :end))
              ;; if new insert, prompt for description
              (unwind-protect
                  (setq description (string-trim (read-string "Description: ")))
                (mpvi-pause (cdr ret))))
            ;; insert the new link
            (let ((link (funcall mpvi-build-link-function path (car ret)
                                 (if node (plist-get node :vend))
                                 (if (> (length description) 0) description))))
              (save-excursion (insert link))))))
    (user-error "This is not org-mode, should not insert org link")))

;;;###autoload
(defun mpvi-clip (path &optional target beg end)
  "Cut or convert video for PATH from BEG to END, save to TARGET.
Default handle current video at point."
  (interactive
   (if-let ((node (ignore-errors (mpvi-parse-link-at-point))))
       (let ((path (plist-get node :path)))
         (if (or (mpvi-url-p path) (file-exists-p path))
             (list path
                   (unless (mpvi-url-p path) (mpvi-read-file-name "Save to: " path))
                   (plist-get node :vbeg) (plist-get node :vend))
           (user-error "File not found: %s" path)))
     (let* ((path (unwind-protect
                      (ffap-read-file-or-url
                       "Clip video (file or url): "
                       (prog1 (mpvi-ffap-guesser) (ffap-highlight)))
                    (ffap-highlight t)))
            (target (unless (mpvi-url-p path) (mpvi-read-file-name "Save to: " path))))
       (list path target))))
  (funcall (if (mpvi-url-p path) mpvi-remote-video-handler mpvi-local-video-handler)
           path target beg end))

;;;###autoload
(defun mpvi-emms-add (path &optional label)
  "Add PATH to EMMS playlist. LABEL is extra info to show in EMMS buffer."
  (interactive (list (ffap-read-file-or-url
                      "Add to EMMS (file or url): "
                      (prog1 (mpvi-ffap-guesser) (ffap-highlight)))))
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
        (cl-loop with desc = (or label (read-string "Description: " (car playlist)))
                 for url in choosen
                 for disp = (if (> (length desc) 0) (format "%s - %s" desc url) url)
                 do (emms-add-url (propertize url 'display disp))))
    (setq path (expand-file-name path))
    (cond ((file-directory-p path)
           (emms-add-directory path))
          ((file-regular-p path)
           (emms-add-file path))
          (t (user-error "Unkown source: %s" path)))))


;;; Integrate with Org Link

(defvar mpvi-org-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ", s")   #'mpvi-current-link-seek)
    (define-key map (kbd ", a")   #'mpvi-insert)
    (define-key map (kbd ", b")   #'mpvi-current-link-update-end-pos)
    (define-key map (kbd ", v")   #'mpvi-current-link-show-preview)
    (define-key map (kbd ", c")   #'mpvi-clip)
    (define-key map (kbd ", ,")   #'org-open-at-point)
    (define-key map (kbd ", SPC") #'mpvi-pause)
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

(defun mpvi-current-link-seek ()
  "Seek position for this link."
  (interactive)
  (mpvi-with-current-mpv-link (node)
    (when node (mpvi-seek))))

(defun mpvi-current-link-update-end-pos ()
  "Update the end position on this link."
  (interactive)
  (mpvi-with-current-mpv-link (node)
    (when node
      (let ((ret (mpvi-seek (or (plist-get node :vend)
                                (max (plist-get node :vbeg) (mpvi-prop 'playback-time)))
                            (format "Set end position (%d-%d): " (plist-get node :vbeg) (mpvi-prop 'duration)))))
        (delete-region (plist-get node :begin) (plist-get node :end))
        (let ((link (funcall mpvi-build-link-function (plist-get node :path)
                             (plist-get node :vbeg) (car ret))))
          (save-excursion (insert link)))))))

(defun mpvi-current-link-show-preview ()
  "Show the preview tooltip for this link."
  (interactive)
  (when-let ((node (mpvi-parse-link-at-point)))
    (let* ((scr (funcall mpvi-screenshot-function (plist-get node :path) (plist-get node :vbeg)))
           (img (create-image scr nil nil :width 400))
           (help (propertize " " 'display img))
           (x-gtk-use-system-tooltips nil))
      (tooltip-show help))))

;;;###autoload
(defun mpvi-org-link-init ()
  "Setup org link with `mpv' prefix."
  (require 'org)
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

(require 'mpvi-ps) ; optional platform specialized config

(provide 'mpvi)

;;; mpvi.el ends here
