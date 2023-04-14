;;; org-mpvi.el --- Integrate MPV with org mode -*- lexical-binding: t -*-

;; Copyright (C) 2023 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; URL: https://github.com/lorniu/org-mpvi
;; Package-Requires: ((emacs "28.1") (org "9.6") (mpv "0.2.0"))
;; Keywords: convenience, docs
;; SPDX-License-Identifier: MIT
;; Version: 1.0

;;; Commentary:
;;
;; Integrate MPV with Org Mode, so watching video in Emacs conveniently and taking notes easily.
;;
;; Installation:
;;  - Install `mpv.el'
;;  - Download and add this repo to your `load-path', then \\=(require 'org-mpvi)
;;  - Install the dependencies: `mpv' (required), `yt-dlp', `ffmpeg', `seam', `danmaku2ass', `tesseract'
;;
;; Use `org-mpvi-open' to open a video, then control the MPV with `org-mpvi-seek'.
;;
;; For more information, see README file.
;;
;; References:
;;  - https://mpv.io/manual/master/#properties
;;  - https://kitchingroup.cheme.cmu.edu/blog/2016/11/04/New-link-features-in-org-9/

;;; Code:

(require 'ffap)
(require 'org-attach)
(require 'org-element)
(require 'mpv)

(defgroup org-mpvi nil
  "Integrate MPV with org mode."
  :group 'external
  :prefix 'org-mpvi-)

(defvar org-mpvi-enable-debug nil)

(defcustom org-mpvi-extra-mpv-args nil
  "Extra options you want to pass to MPV player."
  :type 'list)

(defcustom org-mpvi-cache-directory
  (let ((dir (expand-file-name "org-mpvi/" (temporary-file-directory))))
    (unless (file-exists-p dir) (make-directory dir))
    dir)
  "Used to save temporary files."
  :type 'directory)

(defvar org-mpvi-last-save-directory nil)

(defvar org-mpvi-play-history nil)

(defvar org-mpvi-build-link-function #'org-mpvi-build-mpv-link)

(defvar org-mpvi-screenshot-function #'org-mpvi-screenshot)

(defvar org-mpvi-ocr-function #'org-mpvi-ocr-by-tesseract)

(defvar org-mpvi-local-video-handler #'org-mpvi-convert-by-ffmpeg)

(defvar org-mpvi-remote-video-handler #'org-mpvi-ytdlp-download)

(defvar org-mpvi-annotation-face '(:inherit completions-annotations))

(defun org-mpvi-log (fmt &rest args)
  "Output log when `org-mpvi-enable-debug' not nil.
FMT and ARGS are like arguments in `message'."
  (when org-mpvi-enable-debug
    (apply #'message (concat "[org-mpvi] " fmt) args)))

(defun org-mpvi-call-process (program &rest args)
  "Helper for `call-process', PROGRAM and ARGS are the same."
  (org-mpvi-log ">>> %s %s" program
                (mapconcat (lambda (a) (shell-quote-argument a)) args " "))
  (apply #'call-process program nil t nil args))

(defun org-mpvi-bark-if-not-live ()
  "Check if mpv is runing."
  (unless (and (mpv-live-p) (ignore-errors (mpv-get-property "time-pos")))
    (user-error "No living mpv found")))

(cl-defmacro org-mpvi-with-current-mpv-link ((var &optional path errmsg) &rest form)
  "Run FORM when there is a mpv PATH at point that is playing.
Bind the link object to VAR for convenience. Alert user with ERRMSG when
there is a different path at point."
  (declare (indent 1))
  `(progn
     (org-mpvi-bark-if-not-live)
     (let ((,var (org-mpvi-parse-link-at-point)))
       (when (and ,var (not (equal (plist-get ,var :path)
                                   ,(or path `(org-mpvi-origin-path)))))
         (user-error ,(or errmsg "Current link is not the actived one, do nothing")))
       ,@form)))

(defun org-mpvi-seekable (&optional arg)
  "Whether current video is seekable.
Alert user when not seekable when ARG not nil."
  (let ((seekable (eq (mpv-get-property "seekable") t)))
    (if (and arg (not seekable))
        (user-error "Current video is not seekable, do nothing")
      seekable)))

(defun org-mpvi-set-pause (how)
  "Set pause state of mpv.
HOW is :json-false or t that returned by get-property."
  (mpv-set-property "pause" (if (eq how :json-false) "no" "yes")))

(defun org-mpvi-time-to-secs (time)
  "Convert TIME to seconds format."
  (cond ((or (null time) (numberp time)) time)
        ((or (not (stringp time)) (not (string-match-p "^-?[0-9:.]+$" time)))
         (user-error "This is not a valid time: %s" time))
        ((cl-find ?: time)
         (+ (org-timer-hms-to-secs (org-timer-fix-incomplete time))
            (if-let (p (cl-search "." time)) (string-to-number (cl-subseq time p)) 0)))
        (t (string-to-number time))))

(defun org-mpvi-secs-to-hms (secs &optional full truncate)
  "Convert SECS to h:mm:ss.xx format.
If FULL is nil, remove '0:' prefix. If TRUNCATE is non-nil, remove frac suffix."
  (let* ((frac (cadr (split-string (number-to-string secs) "\\.")))
         (ts (concat (org-timer-secs-to-hms (truncate secs)) (if frac ".") frac)))
    (when (and (not full) (string-prefix-p "0:" ts))
      (setq ts (cl-subseq ts 2)))
    (if truncate (car (split-string ts "\\.")) ts)))

(defun org-mpvi-secs-to-string (secs &optional groupp)
  "Truncate SECS and format to string, keep at most 2 float digits.
When GROUPP not nil then try to insert commas to string for better reading."
  (let ((ret (number-to-string
              (if (integerp secs) secs
                (/ (truncate (* 100 secs)) (float 100))))))
    (when groupp
      (while (string-match "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)" ret)
	    (setq ret (concat (match-string 1 ret) "," (match-string 2 ret)))))
    ret))

(defvar org-mpvi-clipboard-command
  (cond ((executable-find "xclip")
         ;; A hangs issue:
         ;; https://www.reddit.com/r/emacs/comments/da9h10/why_does_shellcommand_hang_using_xclip_filter_to/
         "xclip -selection clipboard -t image/png -filter < \"%s\" &>/dev/null")
        ((and (executable-find "powershell") (memq system-type '(cygwin windows-nt)))
         "powershell -Command \"Add-Type -AssemblyName System.Windows.Forms; [Windows.Forms.Clipboard]::SetImage($([System.Drawing.Image]::Fromfile(\\\"%s\\\")))\"")))

(defun org-mpvi-image-to-clipboard (image-file)
  "Save IMAGE-FILE data to system clipboard.
I don't know whether better solutions exist."
  (if (and org-mpvi-clipboard-command (file-exists-p image-file))
      (let ((command (format org-mpvi-clipboard-command image-file)))
        (org-mpvi-log "Copy image to clipboard: %s" command)
        (shell-command command))
    (user-error "Nothing to do with copy image file")))

(defun org-mpvi-read-file-name (prompt default-name)
  "Read file name using a PROMPT minibuffer.
DEFAULT-NAME is used when only get a directory name."
  (let* ((default-directory org-mpvi-last-save-directory)
         (target (read-file-name prompt)))
    (if (directory-name-p target)
        (expand-file-name (file-name-nondirectory default-name) target)
      (expand-file-name target))))

(defun org-mpvi-ffap-guesser ()
  "Return proper url or file at current point."
  (let* ((mark-active nil)
         (guess (or (when (derived-mode-p 'org-mode)
                      (let ((elem (org-element-context)))
                        (when (equal 'link (car elem))
                          (setq elem (cadr elem))
                          (pcase (plist-get elem :type)
                            ("mpv" (car (org-mpvi-parse-link (plist-get elem :path))))
                            ((or "http" "https") (plist-get elem :raw-link))))))
                    (ffap-url-at-point)
                    (ffap-file-at-point))))
    (when (and guess (not (mpv--url-p guess)))
      (if (file-exists-p guess)
          (when (file-directory-p guess)
            (setq guess (file-name-as-directory guess)))
        (setq guess nil)))
    guess))

(defun org-mpvi-build-mpv-link (path &optional beg end desc)
  "Build mpv link with timestamp that used in org buffer.
PATH is local video file or remote url. BEG and END is the position number.
DESC is optional, used to describe the current timestamp link."
  (concat "[[mpv:" path (if (or beg end) "#")
          (if beg (number-to-string beg))
          (if end "-")
          (if end (number-to-string end))
          "][▶ "
          (if beg (org-mpvi-secs-to-hms beg nil t))
          (if end " → ")
          (if end (org-mpvi-secs-to-hms end nil t))
          "]]"
          (if desc (concat " " desc))))

(defun org-mpvi-parse-link (link)
  "Extract path, beg, end from LINK."
  (if (string-match "^\\([^#]+\\)\\(?:#\\([0-9:.]+\\)?-?\\([0-9:.]+\\)?\\)?$" link)
      (let ((path (match-string 1 link))
            (beg (match-string 2 link))
            (end (match-string 3 link)))
        (list path (org-mpvi-time-to-secs beg) (org-mpvi-time-to-secs end)))
    (user-error "Link is not valid")))

(defun org-mpvi-parse-link-at-point ()
  "Return the mpv link object at point."
  (unless (derived-mode-p 'org-mode)
    (user-error "You must parse MPV link in org mode"))
  (let ((node (cadr (org-element-context))))
    (when (equal "mpv" (plist-get node :type))
      (let ((meta (org-mpvi-parse-link (plist-get node :path)))
            (end (save-excursion (goto-char (plist-get node :end)) (skip-chars-backward " \t") (point))))
        `(:path ,(car meta) :vbeg ,(cadr meta) :vend ,(caddr meta) :end ,end ,@node)))))

(defcustom org-mpvi-attach-link-attrs "#+attr_html: :width 666"
  "Attrs insert above a inserted attach image.
The :width can make image cannot display too large in org mode."
  :type 'string)

(defun org-mpvi-insert-attach-link (file)
  "Save image FILE to org file using `org-attach'."
  ;; attach it
  (let ((org-attach-method 'mv)) (org-attach-attach file))
  ;; insert the attrs
  (when org-mpvi-attach-link-attrs
    (insert (concat (string-trim org-mpvi-attach-link-attrs) "\n")))
  ;; insert the link
  (insert "[[attachment:" (file-name-base file) "." (file-name-extension file) "]]")
  ;; show it
  (org-display-inline-images))

(defvar org-mpvi-current-url-metadata nil)

(cl-defgeneric org-mpvi-extract-url (type url &rest _)
  "Extract URL for different platforms.

Return a plist:
- :url for the real url
- :opts for extra options passed to `mpv-start'
- :hook for function added to `mpv-on-start-hook'
- :out-url-decorator for function used to decorate url when open in external program
- others maybe used in anywhere else

TYPE should be keyword as :host format, for example :www.youtube.com,
if it's nil then this method will be a dispatcher."
  (:method (type url &rest args)
           (unless type ; the first call
             (let* ((typefn (lambda (url)
                              (intern (concat ":" (url-host (url-generic-parse-url url))))))
                    (playlist (org-mpvi-extract-playlist
                               (funcall typefn url)  url))
                    (purl (car playlist)) ret)
               (if-let ((dest (apply #'org-mpvi-extract-url  ; dispatch to method
                                     (funcall typefn (or purl url))
                                     (or purl url) args)))
                   (progn (setq ret dest)
                          (unless (plist-get ret :url)
                            (plist-put ret :url (or purl url))))
                 (setq ret (list :url (or purl url))))
               (when playlist
                 (plist-put ret :playlist url)
                 (plist-put ret :playlist-index (cadr playlist)))
               (unless (equal (plist-get ret :url) url)
                 (plist-put ret :origin url))
               ret))))

(cl-defgeneric org-mpvi-extract-playlist (type url)
  "Check if URL is a playlist link. If it is, return the selected playlist-item.
TYPE is platform as the same as in `org-mpvi-extract-url'."
  (:method (_type url)
           (let ((meta (org-mpvi-ytdlp-url-metadata url)))
             (when (alist-get 'is_playlist meta)
               (let* ((items (cl-loop for item across (alist-get 'entries meta) for i from 1
                                      for url = (alist-get 'url item)
                                      for styled = (if (member url org-mpvi-play-history) (propertize url 'face org-mpvi-annotation-face) url)
                                      collect (propertize styled 'line-prefix (propertize (format "%2d. " i) 'face org-mpvi-annotation-face))))
                      (item (completing-read
                             (concat "Playlist" (if-let (title (alist-get 'title meta)) (format "(%s)" title))  ": ")
                             (lambda (input pred action)
                               (if (eq action 'metadata)
                                   `(metadata (display-sort-function . ,#'identity))
                                 (complete-with-action action items input pred)))
                             nil t nil nil (car items))))
                 (list item (cl-position item items :test #'string=)))))))

(defun org-mpvi-origin-path (&optional path)
  "Reverse of `org-mpvi-extract-url', return the origin url for PATH.
When PATH is nil then return the path of current playing video."
  (unless path
    (org-mpvi-bark-if-not-live)
    (setq path (mpv-get-property "path")))
  (or (plist-get org-mpvi-current-url-metadata :origin) path))

(defun org-mpvi-play (path &optional beg end paused)
  "Play PATH from BEG to END. Pause at BEG when PAUSED not-nil."
  (if (mpv--url-p path)
      (unless (or (executable-find "youtube-dl") (executable-find "yt-dlp"))
        (user-error "You should have 'yt-dlp' installed to play remote url"))
    (setq path (expand-file-name path)))
  (unless beg (setq beg 0))
  (if (and (mpv-live-p) (equal path (ignore-errors (org-mpvi-origin-path))))
      ;; is playing: try to seek position
      (when (org-mpvi-seekable)
        (mpv-set-property "ab-loop-a" (if end beg "no"))
        (mpv-set-property "ab-loop-b" (or end "no"))
        (mpv-set-property "playback-time" beg))
    ;; not playing: start new
    (let (opts (hook (lambda (&rest _) (message "Started."))))
      (when (mpv--url-p path) ; preprocessing url and extra mpv options
        (when-let ((ret (org-mpvi-extract-url nil path)))
          (setq org-mpvi-current-url-metadata ret)
          (setq path (or (plist-get ret :url) path))
          (setq opts (plist-get ret :opts))
          (setq hook (or (plist-get ret :hook) hook))))
      (let ((mpv-default-options (append opts org-mpvi-extra-mpv-args))
            (mpv-on-start-hook (cons hook mpv-on-start-hook)))
        (format "Waiting %s..." path)
        (org-mpvi-log "MPV start extra options: %s"
                      (mapconcat (lambda (a) (shell-quote-argument a))
                                 mpv-default-options " "))
        (apply #'org-mpvi-start path (format "--start=+%s" beg)
               (if end (list (format "--ab-loop-a=%s" beg)
                             (format "--ab-loop-b=%s" end))))
        (push path org-mpvi-play-history))))
  ;; initial state
  (org-mpvi-set-pause (or paused :json-false)))

(defun org-mpvi-screenshot (path pos &optional target)
  "Capture the screenshot of PATH at POS and save to TARGET."
  (unless (mpv--url-p path)
    (setq path (expand-file-name path)))
  (setq target
        (if target (expand-file-name target)
          (expand-file-name (format-time-string "IMG-%s.png") org-mpvi-cache-directory)))
  (with-temp-buffer
    (if (zerop (call-process "mpv" nil nil nil path
                             "--no-terminal" "--no-audio" "--vo=image" "--frames=1"
                             (format "--start=%s" (or pos 0))
                             "-o" target))
        target
      (user-error "Capture failed: %s" (string-trim (buffer-string))))))

(defun org-mpvi-screenshot-current-playing (&optional target flag)
  "Capture screenshot from current playing mpv and save to TARGET.
If TARGET is nil save to temporary directory, if it is t save to clipboard.
If FLAG is string, pass directly to mpv as <flags> of screenshot-to-file, if
it is nil pass \"video\" as default, else prompt user to choose one."
  (org-mpvi-bark-if-not-live)
  (let ((file (if (stringp target)
                  (expand-file-name target)
                (expand-file-name (format-time-string "IMG-%s.png") org-mpvi-cache-directory)))
        (flags (list "video" "subtitles" "window")))
    (unless (or (null flag) (stringp flag))
      (setq flag (completing-read "Flag of screenshot: " flags nil t)))
    (unless (member flag flags) (setq flag "video"))
    (mpv-run-command "screenshot-to-file" file flag)
    (if (eq target t) ; if filename is t save data to clipboard
        (org-mpvi-image-to-clipboard file)
      (prog1 file (kill-new file)))))

(defcustom org-mpvi-tesseract-args "-l chi_sim"
  "Extra options pass to 'tesseract'."
  :type 'string)

(defun org-mpvi-ocr-by-tesseract (file)
  "Run tesseract OCR on the screenshot FILE."
  (unless (executable-find "tesseract")
    (user-error "Program 'tesseract' not found"))
  (with-temp-buffer
    (if (zerop (apply #'org-mpvi-call-process "tesseract" file "stdout"
                      (if org-mpvi-tesseract-args (split-string-shell-command org-mpvi-tesseract-args))))
        (buffer-string)
      (user-error "OCR tesseract failed: %s" (string-trim (buffer-string))))))

(defcustom org-mpvi-ffmpeg-extra-args nil
  "Extra options pass to 'ffmpeg'."
  :type 'string)

(defun org-mpvi-convert-by-ffmpeg (file &optional target beg end opts)
  "Convert local video FILE from BEG to END using ffmpeg, output to TARGET.
This can be used to cut/resize/reformat and so on.
OPTS is a string, pass to 'ffmpeg' when it is not nil."
  (cl-assert (file-regular-p file))
  (unless (executable-find "ffmpeg")
    (user-error "Program 'ffmpeg' not found"))
  (let* ((beg (if (numberp beg) (format " -ss %s" beg) ""))
         (end (if (numberp end) (format " -to %s" end) ""))
         (extra (if (or opts org-mpvi-ffmpeg-extra-args)
                    (concat " " (string-trim (or opts org-mpvi-ffmpeg-extra-args)))
                  ""))
         (target (expand-file-name
                  (or target (format-time-string "mpv-video-%s.mp4"))
                  org-mpvi-last-save-directory))
         (command (string-trim
                   (read-string
                    "Confirm: "
                    (concat (propertize
                             (concat "ffmpeg"
                                     (propertize " -loglevel error" 'invisible t)
                                     (format " -i %s" (expand-file-name file)))
                             'face 'font-lock-constant-face 'read-only t)
                            " -c copy" extra beg end (format " \"%s\"" target)))))
         (target (with-temp-buffer
                   (insert (string-trim command))
                   (let ((quote (if (member (char-before) '(?' ?\")) (char-before))))
                     (re-search-backward (if quote (format " +%c" quote) " +") nil t)
                     (buffer-substring (match-end 0) (if quote (- (point-max) 1) (point-max)))))))
    (when (file-exists-p target)
      (user-error "Output file %s is already exist!" target))
    (make-directory (file-name-directory target) t) ; ensure directory
    (setq org-mpvi-last-save-directory (file-name-directory target)) ; record the dir
    (with-temp-buffer
      (org-mpvi-log "Convert file %s" file)
      (apply #'org-mpvi-call-process (split-string-and-unquote command))
      (if (file-exists-p target)
          (prog1 target
            (kill-new target)
            (message "Save to %s done." (propertize target 'face 'font-lock-keyword-face)))
        (user-error "Convert with ffmpeg failed: %s" (string-trim (buffer-string)))))))

(defcustom org-mpvi-ytdlp-extra-args nil
  "The default extra options pass to 'yt-dlp'."
  :type 'string)

(defun org-mpvi-ytdlp-download (url &optional target beg end opts)
  "Download and clip video for URL to TARGET. Use BEG and END for range (trim).
OPTS is a string, pass to 'yt-dlp' when it is not nil."
  (cl-assert (mpv--url-p url))
  (unless (and (executable-find "yt-dlp") (executable-find "ffmpeg"))
    (user-error "Programs 'yt-dlp' and 'ffmpeg' should be installed"))
  (let* ((fmt (org-mpvi-ytdlp-pick-format url))
         (beg (if (numberp beg) (format " -ss %s" beg)))
         (end (if (numberp end) (format " -to %s" end)))
         (extra (if (or opts org-mpvi-ytdlp-extra-args)
                    (concat " " (string-trim (or opts org-mpvi-ytdlp-extra-args)))
                  ""))
         (target (expand-file-name (or target (car fmt)) org-mpvi-last-save-directory))
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
    (setq org-mpvi-last-save-directory (file-name-directory target)) ; record the dir
    (with-temp-buffer
      (org-mpvi-log "Download/Clip url %s" url)
      (apply #'org-mpvi-call-process (split-string-shell-command command))
      (if (file-exists-p target)
          (prog1 target
            (kill-new target)
            (message "Save to %s done." (propertize target 'face 'font-lock-keyword-face)))
        (user-error "Download and clip with yt-dlp/ffmpeg failed: %s" (string-trim (buffer-string)))))))

(defun org-mpvi-ytdlp-download-subtitle (url &optional prefix opts)
  "Download subtitle for URL and save as file named begin with PREFIX.
Pass OPTS to 'yt-dlp' when it is not nil."
  (unless (executable-find "yt-dlp")
    (user-error "Program 'yt-dlp' should be installed"))
  (with-temp-buffer
    (org-mpvi-log "Downloading subtitle for %s" url)
    (apply #'org-mpvi-call-process
           "yt-dlp" url "--write-subs" "--skip-download"
           "-o" (or prefix (expand-file-name "SUB-%(fulltitle)s" org-mpvi-cache-directory))
           (split-string-shell-command (or opts org-mpvi-ytdlp-extra-args "")))
    (goto-char (point-min))
    (if (re-search-forward "Destination:\\(.*\\)$" nil t)
        (string-trim (match-string 1))
      (user-error "Error when download subtitle: %s" (string-trim (buffer-string))))))

(defun org-mpvi-ytdlp-pick-format (url)
  "Completing read the formats for video with URL.
Return (suggestion-save-name . video-format)."
  (unless (executable-find "yt-dlp")
    (user-error "Program 'yt-dlp' should be installed"))
  (with-temp-buffer
    (org-mpvi-call-process "yt-dlp" "-F" url)
    (goto-char (point-min))
    (unless (re-search-forward "Available formats for \\(.+\\):" nil t)
      (user-error "Nothing found: %s" (string-trim (buffer-string))))
    (let* ((name (if (equal (mpv-get-property "path") url)
                     (mpv-get-property "media-title")
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
                     (lambda (input pred action)
                       (pcase action
                         ('metadata
                          `(metadata (display-sort-function . ,#'identity)))
                         (`(boundaries . ,suffix)
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

(defun org-mpvi-ytdlp-url-metadata (url &optional opts)
  "Return metadata for URL, pass extra OPTS to `yt-dlp' for querying.
I just want to judge if current URL is a playlist link, but I can't find
better/faster solution. Maybe cache the results is one choice, but I don't think
it's good enough. Then I can not find good way to get all descriptions of
playlist item with light request. This should be improved someday."
  (unless (executable-find "yt-dlp")
    (user-error "Program 'yt-dlp' should be installed"))
  (with-temp-buffer
    (condition-case err
        (progn
          (org-mpvi-log "Request matadata for %s" url)
          (apply #'org-mpvi-call-process
                 "yt-dlp" url "-J" "--flat-playlist"
                 (split-string-shell-command (or opts org-mpvi-ytdlp-extra-args "")))
          (goto-char (point-min))
          (let* ((json (json-read))
                 (playlistp (equal "playlist" (alist-get '_type json))))
            (if playlistp (nconc json (list '(is_playlist . t))))
            json))
      (error (user-error "Error when get metadata for %s: %s" url (string-trim (buffer-string)))))))

(defun org-mpvi-ytdlp-output-field (url field &optional opts)
  "Get FIELD information for video URL.
FIELD can be id/title/urls/description/format/thumbnail/formats_table and so on.
Pass extra OPTS to mpv if it is not nil."
  (unless (executable-find "yt-dlp")
    (user-error "Program 'yt-dlp' should be installed"))
  (with-temp-buffer
    (org-mpvi-log "yt-dlp output template for %s of %s" field url)
    (apply #'org-mpvi-call-process
           "yt-dlp" url "--print" field
           (split-string-shell-command (or opts org-mpvi-ytdlp-extra-args "")))
    (goto-char (point-min))
    (if (re-search-forward "^yt-dlp: error:.*$" nil t)
        (user-error "Error to get `yt-dlp' template/%s: %s" field (match-string 0))
      (string-trim (buffer-string)))))


;;; Patch 'mpv.el' for Windows

(defun org-mpvi-start (&rest args)
  "Start an mpv process with the specified ARGS.
This is just `mpv-start' that with windows support."
  (mpv-kill)
  (let ((pipe (make-temp-name "mpv-")))
    (unless (eq system-type 'windows-nt)
      (setq pipe (expand-file-name pipe temporary-file-directory)))
    (setq mpv--process
          (apply #'start-process "mpv-player" nil mpv-executable
                 "--no-terminal"
                 (concat "--input-ipc-server=" pipe)
                 (append mpv-default-options args)))
    (set-process-query-on-exit-flag mpv--process nil)
    (set-process-sentinel
     mpv--process
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (mpv-kill)
         (when (file-exists-p pipe)
           (with-demoted-errors (delete-file pipe)))
         (run-hooks 'mpv-on-exit-hook))))
    (org-mpvi-connect pipe)
    (run-hook-with-args 'mpv-on-start-hook args)
    t))

(defun org-mpvi-connect (pipename)
  "Connect to mpv via named pipe.
PIPENAME should be name of pipe on Windows or socket file on others."
  (interactive (list (read-string "Connect to pipe with name/socket: ")))
  (with-timeout (mpv-start-timeout
                 (mpv-kill)
                 (error "Failed to connect to MPV"))
    (while (not (if (eq system-type 'windows-nt)
                    (org-mpvi-named-pipe-exists-p pipename)
                  (file-exists-p pipename)))
      (sleep-for 0.05)))
  (setq mpv--queue (tq-create
                    (if (eq system-type 'windows-nt)
                        (org-mpvi-make-named-pipe-client-process pipename)
                      (make-network-process :name "mpv-socket"
                                            :family 'local
                                            :service pipename))))
  (set-process-filter (tq-process mpv--queue)
                      (lambda (_proc string)
                        (let ((buffer (tq-buffer mpv--queue)))
                          (when (buffer-live-p buffer)
                            (with-current-buffer buffer
                              (goto-char (point-max))
                              (insert string)
                              (when (eq system-type 'windows-nt)
                                (goto-char (point-min))
                                ;; when find error raised by powershell
                                (skip-chars-forward " \n\r\t")
                                (unless (or (eobp) (equal (char-after) ?\{))
                                  (user-error "Pipe error: %s" (string-trim (buffer-string))))
                                ;; powershell will output read-host content, filter it
                                (while (re-search-forward "{\"command\":[^}]+}" nil t)
                                  (delete-region (match-beginning 0) (match-end 0))))
                              (mpv--tq-process-buffer mpv--queue)))))))

(defun org-mpvi-named-pipe-exists-p (pipename)
  "Check if pipe with PIPENAME exists on Windows."
  (unless (executable-find "powershell")
    (user-error "Cannot find PowerShell"))
  (with-temp-buffer
    (call-process "powershell" nil t nil
                  "-Command"
                  (format "& {Get-ChildItem \\\\.\\pipe\\ | Where-Object {$_.Name -eq '%s'}}"
                          pipename))
    (> (length (buffer-string)) 0)))

(defun org-mpvi-make-named-pipe-client-process (pipename)
  "Connect to named pipe server with PIPENAME for Windows.
Implement with `powershell'."
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
         (cmd (format "& {%s}" (replace-regexp-in-string "[ \n\r\t]+" " " (format ps1 pipename)))))
    (make-process :name "mpv-socket"
                  :connection-type 'pipe
                  :command (list "powershell" "-NoProfile" "-Command" cmd))))


;;; Commands and Keybinds

;;;###autoload
(defun org-mpvi-open (path &optional act)
  "Open video with mpv, PATH is a local file or remote url.
When ACT is nil or 'play, play the video. If ACT is 'add, just add to playlist.
When called interactively, prompt minibuffer with `C-x RET' to add to playlist,
type `C-x b' to choose video path from `org-mpvi-favor-paths'."
  (interactive (catch 'org-mpvi-open
                 (minibuffer-with-setup-hook
                     (lambda ()
                       (use-local-map (make-composed-keymap (list (current-local-map) org-mpvi-open-map))))
                   (list (unwind-protect
                             (catch 'ffap-prompter
                               (ffap-read-file-or-url
                                "Playing video (file or url): "
                                (prog1 (org-mpvi-ffap-guesser) (ffap-highlight))))
                           (ffap-highlight t))))))
  (unless (and (> (length path) 0) (or (mpv--url-p path) (file-exists-p path)))
    (user-error "Not correct file or url"))
  (prog1 (setq path (if (mpv--url-p path) path (expand-file-name path)))
    (cond
     ((or (null act) (equal act 'play))
      (setq org-mpvi-current-url-metadata nil)
      (org-mpvi-play path))
     ((equal act 'add)
      (org-mpvi-bark-if-not-live)
      (when (mpv--url-p path)
        (setq path
              (or (plist-get (org-mpvi-extract-url nil path :urlonly t) :url) path)))
      (mpv--playlist-append path))
     ((equal act 'dup)
      (if (mpv--url-p path)
          (org-mpvi-ytdlp-download path)
        (org-mpvi-convert-by-ffmpeg path))))))

(defcustom org-mpvi-favor-paths nil
  "Your favor video path list.
Item should be a path string or a cons.

For example:

  \\='(\"~/video/aaa.mp4\"
    \"https://www.youtube.com/watch?v=NQXA\"
    (\"https://www.douyu.com/110\" . \"some description\"))

This can be used by `org-mpvi-open-from-favors' to quick open video."
  :type 'list)

(defun org-mpvi-open-from-favors ()
  "Choose video from `org-mpvi-favor-paths' and play it."
  (interactive)
  (unless (consp org-mpvi-favor-paths)
    (user-error "You should add your favor paths into `org-mpvi-favor-paths' first"))
  (let* ((annfn (lambda (it)
                  (when-let (s (alist-get it org-mpvi-favor-paths))
                    (format "    (%s)" s))))
         (path (completing-read "Choose video to play: "
                                (lambda (input pred action)
                                  (if (eq action 'metadata)
                                      `(metadata (display-sort-function . ,#'identity)
                                                 (annotation-function . ,annfn))
                                    (complete-with-action action org-mpvi-favor-paths input pred)))
                                nil t)))
    ;; called directly vs called from minibuffer
    (if (= (recursion-depth) 0)
        (org-mpvi-open path)
      (throw 'org-mpvi-open (list path 'play)))))

(defvar org-mpvi-open-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-x b") #'org-mpvi-open-from-favors)
    (define-key map (kbd "C-x <return>") (lambda () (interactive) (throw 'org-mpvi-open (list (minibuffer-contents) 'add))))
    (define-key map (kbd "C-x C-w") (lambda () (interactive) (throw 'org-mpvi-open (list (minibuffer-contents) 'dup))))
    map))

;;;###autoload
(defun org-mpvi-insert (&optional prompt)
  "Insert a mpv link or update a mpv link at point.
PROMPT is used in minibuffer when invoke `org-mpvi-seek'."
  (interactive "P")
  (if (derived-mode-p 'org-mode)
      (let ((path (org-mpvi-origin-path)) description)
        (unless (org-mpvi-seekable)
          (org-mpvi-set-pause t)
          (user-error "Current video is not seekable, it makes no sense to insert timestamp link"))
        (org-mpvi-with-current-mpv-link (node path)
          (when-let (ret (org-mpvi-seek (if node (plist-get node :vbeg)) prompt))
            (org-mpvi-set-pause t)
            ;; if on a mpv link, update it
            (if node (delete-region (plist-get node :begin) (plist-get node :end))
              ;; if new insert, prompt for description
              (unwind-protect
                  (setq description (string-trim (read-string "Description: ")))
                (org-mpvi-set-pause (cdr ret))))
            ;; insert the new link
            (let ((link (funcall org-mpvi-build-link-function path (car ret)
                                 (if node (plist-get node :vend))
                                 (if (> (length description) 0) description))))
              (save-excursion (insert link))))))
    (user-error "This is not org-mode, should not insert org link")))

(defvar org-mpvi-seek-overlay nil)

(defvar org-mpvi-seek-paused nil)

;;;###autoload
(defun org-mpvi-seek (&optional pos prompt)
  "Interactively seek POS for current playing video.
PROMPT is used if non-nil for `minibuffer-prompt'."
  (interactive)
  (if (not (mpv-live-p))
      (call-interactively #'org-mpvi-open)
    (org-mpvi-bark-if-not-live)
    (mpv-set-property "keep-open" "yes") ; prevent unexpected close
    (let ((paused (mpv-get-property "pause")))
      (org-mpvi-set-pause t)
      (unwind-protect
          (let ((ret
                 (catch 'org-mpvi-seek
                   (minibuffer-with-setup-hook
                       (lambda ()
                         (add-hook 'after-change-functions
                                   (lambda (start end old-len)
                                     (when (or (not (string-match-p "^[0-9]+\\.?[0-9]*$" (buffer-substring start end)))
                                               (not (<= 0 (string-to-number (minibuffer-contents)) (mpv-get-duration))))
                                       (delete-region start end)))
                                   nil t)
                         (add-hook 'post-command-hook #'org-mpvi-seek-refresh-annotation nil t))
                     (ignore-errors
                       (read-from-minibuffer
                        (or prompt (if (org-mpvi-seekable)
                                       (format "MPV Seek (0-%d): " (mpv-get-duration))
                                     "MPV Controller: "))
                        (number-to-string (or pos (mpv-get-playback-position)))
                        org-mpvi-seek-map t 'org-mpvi-seek-hist))))))
            (when ret
              (cond ((stringp ret) (message "%s" ret))
                    ((eq (mpv-get-property "pause") :json-false))
                    ((and (org-mpvi-seekable) (numberp ret))
                     (mpv-set-property "playback-time" ret)))
              (cons (ignore-errors (mpv-get-playback-position)) paused)))
        (org-mpvi-set-pause (or org-mpvi-seek-paused paused))))))

(defvar org-mpvi-seek-annotation-alist
  '((if (eq (mpv-get-property "loop") t) "Looping")
    (if (eq (mpv-get-property "pause") t) "Paused")
    ("Speed" . (format "%.2f" (mpv-get-property "speed")))
    ("Total" . (org-mpvi-secs-to-hms (mpv-get-duration) nil t)))
  "The items displayed in the minibuffer when `org-mpvi-seek-refresh-annotation'.")

(defun org-mpvi-seek-refresh-annotation ()
  "Show information of the current playing in minibuffer."
  (when org-mpvi-seek-overlay
    (delete-overlay org-mpvi-seek-overlay))
  (let ((kf (lambda (s) (if s (format " %s:" s))))
        (vf (lambda (s) (if s (propertize (format " %s " s) 'face org-mpvi-annotation-face))))
        (sf (lambda (s) (propertize " " 'display `(space :align-to (- right-fringe ,(1+ (length s))))))) ; space
        (ov (make-overlay (point-max) (point-max) nil t t)))
    (overlay-put ov 'intangible t)
    (setq org-mpvi-seek-overlay ov)
    (if (org-mpvi-seekable)
        (condition-case nil
            (let* ((hms (when-let (s (ignore-errors (org-mpvi-secs-to-hms (string-to-number (minibuffer-contents)))))
                          (funcall vf (format "%s  %.2f%% " s (mpv-get-property "percent-pos")))))
                   (text (cl-loop for i in org-mpvi-seek-annotation-alist
                                  if (stringp (car i)) concat (concat (funcall kf (car i)) " " (funcall vf (eval (cdr i))))
                                  else concat (funcall vf (eval i))))
                   (space (funcall sf (concat hms text))))
              (overlay-put ov 'before-string (propertize (concat space hms text) 'cursor t)))
          (error nil))
      (let* ((title (funcall vf (concat "        >> " (string-trim (or (mpv-get-property "media-title") "")))))
             (state (funcall vf (if (eq (mpv-get-property "pause") t) "Paused")))
             (space (funcall sf state)))
        (delete-minibuffer-contents) (insert "0")
        (overlay-put ov 'before-string (propertize (concat title space state) 'cursor t))))))

(defun org-mpvi-seek-walk (offset)
  "Seek forward or backward with factor of OFFSET.
If OFFSET is number then step by seconds.
If OFFSET is xx% format then step by percent.
If OFFSET is :ff or :fb then step forward/backward one frame."
  (pcase offset
    (:ff (mpv-run-command "frame_step"))
    (:fb (mpv-run-command "frame_back_step"))
    (_
     (when (and (stringp offset) (string-match-p "^-?[0-9]\\{0,2\\}\\.?[0-9]*%$" offset)) ; percent
       (setq offset (* (/ (string-to-number (cl-subseq offset 0 -1)) 100.0) (mpv-get-duration))))
     (unless (numberp offset) (setq offset 1))
     (let* ((old (if (or (zerop offset) (eq (mpv-get-property "pause") t))
                     (let ((str (string-trim (minibuffer-contents))))
                       (unless (string-match-p "^[0-9]+\\(\\.[0-9]+\\)?$" str)
                         (user-error "Not valid number"))
                       (string-to-number str))
                   (mpv-get-playback-position)))
            (new (+ old offset))
            (total (mpv-get-duration)))
       (if (< new 0) (setq new 0))
       (if (> new total) (setq new total))
       (unless (= old new)
         (delete-minibuffer-contents)
         (insert (org-mpvi-secs-to-string new)))
       (mpv-set-property "playback-time" new))))
  (org-mpvi-seek-revert))

(defun org-mpvi-seek-speed (&optional num)
  "Tune the speed base on NUM."
  (interactive)
  (org-mpvi-seekable 'assert)
  (pcase num
    ('nil (mpv-set-property "speed" "1")) ; reset
    ((pred numberp) (mpv-speed-increase num))
    (_ (mpv-speed-increase (read-number "Step: " num)))))

(defun org-mpvi-seek-revert (&optional num)
  "Insert current playback-time to minibuffer.
If NUM is not nil, go back that position first."
  (interactive)
  (when (and num (org-mpvi-seekable))
    (mpv-set-property "playback-time" num))
  (delete-minibuffer-contents)
  (insert (org-mpvi-secs-to-string (mpv-get-playback-position))))

(defun org-mpvi-seek-pause ()
  "Revert and pause."
  (interactive)
  (mpv-pause)
  (setq org-mpvi-seek-paused (eq (mpv-get-property "pause") t))
  (when org-mpvi-seek-paused (org-mpvi-seek-revert)))

(defun org-mpvi-seek-insert ()
  "Insert new link in minibuffer seek."
  (interactive)
  (org-mpvi-seekable 'assert)
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (let ((paused (mpv-get-property "pause")))
      (org-mpvi-set-pause t)
      (unwind-protect
          (if (derived-mode-p 'org-mode)
              (let* ((desc (string-trim (read-string "Notes: ")))
                     (link (funcall org-mpvi-build-link-function
                                    (org-mpvi-origin-path)
                                    (mpv-get-playback-position)
                                    nil desc)))
                (cond ((org-at-item-p) (end-of-line) (org-insert-item))
                      (t               (end-of-line) (insert "\n")))
                (set-window-point (get-buffer-window) (point))
                (save-excursion (insert link)))
            (user-error "This is not org-mode, should not insert timestamp link"))
        (org-mpvi-set-pause paused))))
  (org-mpvi-seek-revert))

(defun org-mpvi-seek-clip ()
  "Download/Clip current playing video."
  (interactive)
  (let ((path (mpv-get-property "path")))
    (funcall (if (mpv--url-p path) org-mpvi-remote-video-handler org-mpvi-local-video-handler) path))
  (throw 'org-mpvi-seek nil))

(defun org-mpvi-seek-copy-sub-text ()
  "Copy current sub text to kill ring."
  (interactive)
  (when-let ((sub (ignore-errors (mpv-get-property "sub-text"))))
    (kill-new sub)
    (throw 'org-mpvi-seek "Copied to kill ring, yank to the place you want.")))

(defun org-mpvi-seek-capture-save-as ()
  "Capture current screenshot and prompt to save."
  (interactive)
  (let ((target (org-mpvi-read-file-name "Screenshot save to: " (format-time-string "mpv-%F-%X.png"))))
    (make-directory (file-name-directory target) t)
    (org-mpvi-screenshot-current-playing target current-prefix-arg)
    (throw 'org-mpvi-seek (format "Captured to %s" target))))

(defun org-mpvi-seek-capture-to-clipboard ()
  "Capture current screenshot and save to clipboard."
  (interactive)
  (org-mpvi-screenshot-current-playing t current-prefix-arg)
  (throw 'org-mpvi-seek "Screenshot is in clipboard, paste to use"))

(defun org-mpvi-seek-capture-as-attach ()
  "Capture current screenshot and insert as attach link."
  (interactive)
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (unless (derived-mode-p 'org-mode)
      (user-error "This is not org-mode, should not insert org link")))
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (when (org-mpvi-parse-link-at-point)
      (end-of-line) (insert "\n"))
    (org-mpvi-insert-attach-link (org-mpvi-screenshot-current-playing nil current-prefix-arg)))
  (throw 'org-mpvi-seek "Capture and insert done."))

(defun org-mpvi-seek-ocr-to-kill-ring ()
  "OCR current screenshot and save the result into kill ring."
  (interactive)
  (with-current-buffer (window-buffer (minibuffer-selected-window))
    (let ((ret (funcall org-mpvi-ocr-function (org-mpvi-screenshot-current-playing))))
      (kill-new ret)))
  (throw 'org-mpvi-seek "OCR done into kill ring, please yank it."))

(defun org-mpvi-current-playing-switch-playlist ()
  "Extract playlist from current video url.
If any, prompt user to choose one video in playlist to play."
  (interactive)
  (org-mpvi-bark-if-not-live)
  (if-let ((playlist (plist-get org-mpvi-current-url-metadata :playlist))
           (playlist-index (plist-get org-mpvi-current-url-metadata :playlist-index))
           (msg "Switch done."))
      (condition-case nil
          (throw 'org-mpvi-seek (prog1 msg (org-mpvi-play playlist)))
        (error (message msg)))
    (user-error "No playlist found for current playing url")))

(defun org-mpvi-current-playing-load-subtitle (subfile)
  "Load or reload the SUBFILE for current playing video."
  (interactive (list (read-file-name "Danmaku file: " org-mpvi-cache-directory nil t)))
  (org-mpvi-bark-if-not-live)
  (cl-assert (file-regular-p subfile))
  (when (string-suffix-p ".danmaku.xml" subfile) ; bilibili
    (setq subfile (org-mpvi-convert-danmaku2ass subfile 'confirm)))
  (ignore-errors (mpv-run-command "sub-remove"))
  (mpv-run-command "sub-add" subfile)
  (message "Sub file loaded!"))

(defun org-mpvi-current-playing-open-externally ()
  "Open current playing video PATH with system program."
  (interactive)
  (org-mpvi-bark-if-not-live)
  (if-let ((path (org-mpvi-origin-path)))
      (if (y-or-n-p (format "Open '%s' externally?" path))
          (let ((msg "Open in system program done."))
            ;; add begin time for url if necessary
            (when-let (f (plist-get org-mpvi-current-url-metadata :out-url-decorator))
              (setq path (funcall f path (mpv-get-playback-position))))
            (browse-url path)
            (setq org-mpvi-seek-paused t)
            (condition-case nil (throw 'org-mpvi-seek msg)
              (error (message msg))))
        (message ""))
    (user-error "No playing path found")))

(defvar org-mpvi-seek-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "i")   #'org-mpvi-seek-insert)
    (define-key map (kbd "g")   #'org-mpvi-seek-revert)
    (define-key map (kbd "n")   (lambda () (interactive) (org-mpvi-seek-walk 1)))
    (define-key map (kbd "p")   (lambda () (interactive) (org-mpvi-seek-walk -1)))
    (define-key map (kbd "N")   (lambda () (interactive) (org-mpvi-seek-walk "1%")))
    (define-key map (kbd "P")   (lambda () (interactive) (org-mpvi-seek-walk "-1%")))
    (define-key map (kbd "M-n") (lambda () (interactive) (org-mpvi-seek-walk :ff)))
    (define-key map (kbd "M-p") (lambda () (interactive) (org-mpvi-seek-walk :fb)))
    (define-key map (kbd "C-l") (lambda () (interactive) (org-mpvi-seek-walk 0)))
    (define-key map (kbd "C-n") (lambda () (interactive) (org-mpvi-seek-walk 1)))
    (define-key map (kbd "C-p") (lambda () (interactive) (org-mpvi-seek-walk -1)))
    (define-key map (kbd "M-<") (lambda () (interactive) (org-mpvi-seek-revert 0)))
    (define-key map (kbd "k")   (lambda () (interactive) (org-mpvi-seek-speed 1)))
    (define-key map (kbd "j")   (lambda () (interactive) (org-mpvi-seek-speed -1)))
    (define-key map (kbd "l")   #'org-mpvi-seek-speed)
    (define-key map (kbd "<")   #'mpv-chapter-prev)
    (define-key map (kbd ">")   #'mpv-chapter-next)
    (define-key map (kbd "v")   #'org-mpvi-current-playing-switch-playlist)
    (define-key map (kbd "C-v") #'org-mpvi-current-playing-switch-playlist)
    (define-key map (kbd "c")   #'org-mpvi-seek-clip)
    (define-key map (kbd "C-c") #'org-mpvi-seek-clip)
    (define-key map (kbd "s")   #'org-mpvi-seek-capture-save-as)
    (define-key map (kbd "C-s") #'org-mpvi-seek-capture-to-clipboard)
    (define-key map (kbd "C-i") #'org-mpvi-seek-capture-as-attach)
    (define-key map (kbd "r")   #'org-mpvi-seek-ocr-to-kill-ring)
    (define-key map (kbd "C-r") #'org-mpvi-seek-ocr-to-kill-ring)
    (define-key map (kbd "t")   #'org-mpvi-seek-copy-sub-text)
    (define-key map (kbd "C-t") #'org-mpvi-seek-copy-sub-text)
    (define-key map (kbd "T")   #'org-mpvi-current-playing-load-subtitle)
    (define-key map (kbd "SPC") #'org-mpvi-seek-pause)
    (define-key map (kbd "o")   #'org-mpvi-current-playing-open-externally)
    (define-key map (kbd "C-o") #'org-mpvi-current-playing-open-externally)
    (define-key map (kbd "q")   #'minibuffer-keyboard-quit)
    (define-key map (kbd "C-q") #'minibuffer-keyboard-quit)
    map))

;;;###autoload
(defun org-mpvi-current-link-clip (path &optional target beg end)
  "Cut or convert video for PATH from BEG to END, save to TARGET.
Default handle current video at point."
  (interactive
   (if-let ((node (org-mpvi-parse-link-at-point)))
       (let ((path (plist-get node :path)))
         (if (or (mpv--url-p path) (file-exists-p path))
             (list path
                   (unless (mpv--url-p path) (org-mpvi-read-file-name "Save to: " path))
                   (plist-get node :vbeg) (plist-get node :vend))
           (user-error "File not found: %s" path)))
     (user-error "No MPV link found at point")))
  (funcall (if (mpv--url-p path) org-mpvi-remote-video-handler org-mpvi-local-video-handler)
           path target beg end))

(defun org-mpvi-current-link-seek ()
  "Seek position for this link."
  (interactive)
  (org-mpvi-with-current-mpv-link (node)
    (when node (org-mpvi-seek))))

(defun org-mpvi-current-link-update-end-pos ()
  "Update the end position on this link."
  (interactive)
  (org-mpvi-with-current-mpv-link (node)
    (when node
      (let ((ret (org-mpvi-seek (or (plist-get node :vend)
                                    (max (plist-get node :vbeg) (mpv-get-playback-position)))
                                (format "Set end position (%d-%d): " (plist-get node :vbeg) (mpv-get-duration)))))
        (delete-region (plist-get node :begin) (plist-get node :end))
        (let ((link (funcall org-mpvi-build-link-function (plist-get node :path)
                             (plist-get node :vbeg) (car ret))))
          (save-excursion (insert link)))))))

(defun org-mpvi-current-link-show-preview ()
  "Show the preview tooltip for this link."
  (interactive)
  (when-let ((node (org-mpvi-parse-link-at-point)))
    (let* ((scr (funcall org-mpvi-screenshot-function (plist-get node :path) (plist-get node :vbeg)))
           (img (create-image scr nil nil :width 400))
           (help (propertize " " 'display img))
           (x-gtk-use-system-tooltips nil))
      (tooltip-show help))))


;;; Integrate with Org Link

(defvar org-mpvi-link-face '(:inherit org-link :underline nil :box (:style flat-button)))

(defvar org-mpvi-link-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mouse-map)
    (define-key map (kbd ", s")   #'org-mpvi-current-link-seek)
    (define-key map (kbd ", a")   #'org-mpvi-insert)
    (define-key map (kbd ", b")   #'org-mpvi-current-link-update-end-pos)
    (define-key map (kbd ", v")   #'org-mpvi-current-link-show-preview)
    (define-key map (kbd ", c")   #'org-mpvi-current-link-clip)
    (define-key map (kbd ", ,")   #'org-open-at-point)
    (define-key map (kbd ", SPC") #'mpv-pause)
    map))

(defun org-mpvi-link-push (link)
  "Play this LINK."
  (pcase-let ((`(,path ,beg ,end) (org-mpvi-parse-link link)))
    (org-mpvi-play path beg end)))

(org-link-set-parameters "mpv"
                         :face org-mpvi-link-face
                         :keymap org-mpvi-link-keymap
                         :follow #'org-mpvi-link-push)

(require 'org-mpvi-ps) ; optional platform specialized config

(provide 'org-mpvi)

;;; org-mpvi.el ends here
