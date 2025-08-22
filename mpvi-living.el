;;; mpvi-living.el --- Extract url for living stream -*- lexical-binding: t -*-

;; Copyright (C) 2023 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; URL: https://github.com/lorniu/mpvi
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Resolve living stream.
;;
;; - https://github.com/yt-dlp/yt-dlp
;; - https://github.com/streamlink/streamlink
;; - https://github.com/Borber/seam
;; - https://github.com/alley-rs/lsar
;; - https://github.com/ihmily/DouyinLiveRecorder
;; - https://github.com/wushuaihua520/BarrageGrab
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function pdd "pdd")
(declare-function pdd-exec "pdd")
(declare-function pdd-split-string-by "pdd")
(declare-function pdd-string-to-object "pdd")
(declare-function pdd-object-to-string "pdd")

(defun mpvi-eval-js (js-code)
  (pdd-exec 'node :init (string-trim js-code)
    :sync t :pipe t :done (lambda (r) (string-trim r))))

(defun mpvi-req-sync (&rest args)
  (declare (indent 1))
  (require 'pdd)
  (apply #'pdd (car args) :sync t (cdr args)))


;;;;; 虎牙 (huya.com)

(defun mpvi-huya-room-id (id-or-url)
  (let ((url id-or-url))
    (unless (and (stringp id-or-url) (string-prefix-p "http" id-or-url))
      (setq url (format "https://m.huya.com/%s" id-or-url)))
    (if (string-match ".com/\\([0-9]+\\)$" url)
        (string-to-number (match-string 1 url))
      (mpvi-req-sync url
        :done (lambda (html)
                (unless (string-match "stream: \\({.+iFrameRate.:[0-9]+}\\)" html)
                  (user-error "无法从 html 中找到 room 信息"))
                (let ((json (json-parse-string (match-string 1 html) :object-type 'alist)))
                  (let-alist (aref (alist-get 'data json) 0)
                    .gameLiveInfo.profileRoom)))))))

(defun mpvi-huya-anonymous-uid ()
  (mpvi-req-sync "https://udblgn.huya.com/web/anonymousLogin"
    :headers '(json)
    :data `((appId . 5002) (byPass . 3) (context . "") (version . "2.4") (data . ((nil . nil))))
    :done (lambda (rs)
            (if (zerop (alist-get 'returnCode rs))
                (string-to-number (alist-get 'uid (alist-get 'data rs)))
              (user-error "Get anonymous id: %s" (alist-get 'description rs))))))

(defun mpvi-huya-uuid ()
  (let ((now (truncate (* (time-to-seconds (current-time)) 1000))))
    (mod (+ (* (mod now 10000000000) 1000) (random 1000)) (1- (expt 2 32)))))

(defun mpvi-huya-anticode (name ac uid)
  (let* ((qs `(,@(cl-loop for item in (split-string ac "&")
                          for pair = (split-string item "=")
                          collect (cons (intern (car pair))
                                        (url-unhex-string (cadr pair))))
               (ver . "1")
               (sv . "2110211124")
               (uid . ,uid)
               (uuid . ,(format "%s" (mpvi-huya-uuid)))
               (seqid . ,(format "%s" (+ uid (round (* 1000 (float-time))))))))
         (fm (thread-last
               (base64-decode-string (alist-get 'fm qs))
               (string-replace "$0" (number-to-string uid))
               (string-replace "$1" name)
               (string-replace "$2" (let-alist qs
                                      (secure-hash 'md5 (format "%s|%s|%s" .seqid .ctype .t))))
               (string-replace "$3" (alist-get 'wsTime qs)))))
    (setf (cdr (assoc 'wsSecret qs)) (secure-hash 'md5 fm))
    (pdd-object-to-string 'query
      (cl-loop for q in qs unless (memq (car q) '(fm txyp)) collect q))))

(defun mpvi-huya-room-info (id-or-url)
  "Return room info for ID-OR-URL of Huya."
  (interactive "s输入虎牙的房间号或直播间地址: ")
  (let* ((profile (mpvi-req-sync "https://mp.huya.com/cache.php"
                    :params `((m . Live) (do . profileRoom) (roomid . ,(mpvi-huya-room-id id-or-url)))
                    :as 'json
                    :done (lambda (profile)
                            (unless (equal (alist-get 'status profile) 200)
                              (user-error "获取 profile 失败: %s" (alist-get 'message profile)))
                            (pcase (alist-get 'liveData (alist-get 'data profile))
                              ("OFF" (user-error "此房间未开播"))
                              ("REPLAY" (user-error "这是重播，不做解析")))
                            profile)))
         (data (alist-get 'data profile))
         (live-data (alist-get 'liveData data))
         (nick (alist-get 'nick live-data))
         (desc (alist-get 'introduction live-data))
         (streams (alist-get 'baseSteamInfoList (alist-get 'stream data)))
         (uid (mpvi-huya-anonymous-uid))
         (urls (cl-loop for item across streams
                        for name = (alist-get 'sStreamName item)
                        for flv-ac = (alist-get 'sFlvAntiCode item)
                        for hls-ac = (alist-get 'sHlsAntiCode item)
                        when (> (length flv-ac) 0)
                        collect
                        (format "%s/%s.%s?%s"
                                (alist-get 'sFlvUrl item) name
                                (alist-get 'sFlvUrlSuffix item)
                                (mpvi-huya-anticode name flv-ac uid))
                        when (> (length hls-ac) 0)
                        collect
                        (format "%s/%s.%s?%s"
                                (alist-get 'sHlsUrl item) name
                                (alist-get 'sHlsUrlSuffix item)
                                (mpvi-huya-anticode name hls-ac uid)))))
    (if (called-interactively-p 'any)
        (message "[%s/%s]\n\n%s" nick desc (mapconcat (lambda (u) (format "- %s" u)) urls "\n"))
      (list :urls urls :nick nick :title desc))))

(cl-defmethod mpvi-extract ((_ (eql :huya.com)) url &rest _)
  "Return the real video URL for 虎牙."
  (unless (string-match-p "\\.\\(flv\\|m3u8\\)\\?" url)
    (let* ((info (mpvi-huya-room-info url))
           (urls (plist-get info :urls))
           (title (concat "[" (plist-get info :nick) "]: " (plist-get info :title))))
      (list :origin url :items urls :title title :logo "虎牙"))))


;;;;; 斗鱼 (douyu.com)

(defconst mpvi-douyu-device-id "10000000000000000000000000001501")

(defvar mpvi-douyu-cookie-string nil)

(defun mpvi-douyu-parse-page (url)
  (let ((html (mpvi-req-sync url)) room-id func val)
    (unless (string-match "\\$ROOM\\.room_id ?= ?\\([0-9]+\\);" html)
      (user-error "未能解析到房间号"))
    (setq room-id (match-string 1 html))
    (unless (string-match "<script type=\"text/javascript\">\\([^>]+\"\\([0-9]\\{12\\}\\)\".*?\\)</script>" html)
      (user-error "未能解析到签名代码"))
    (setq func (match-string 1 html) val (match-string 2 html))
    (list room-id (string-trim func) val)))

(defun mpvi-douyu-extract-sign (func val room-id dt)
  (let* ((js (concat (replace-regexp-in-string "eval(strc)[^)]+)" "strc" func)
                     ";console.log(ub98484234(0,0,0))")) ; deobfuscate
         (v1 (mpvi-eval-js js))
         (v1 (replace-regexp-in-string ; use native md5 to avoid dependency of CryptoJS
              "CryptoJS.MD5(cb).toString();"
              (format "'%s';" (secure-hash 'md5 (format "%s%s%s%s" room-id mpvi-douyu-device-id dt val)))
              v1))
         (v1 (replace-regexp-in-string "(function" "function ccc" v1)) ; invoke and output the sign function
         (v1 (replace-regexp-in-string "rt;})" (format "rt;}console.log(ccc(%s,'%s',%s))" room-id mpvi-douyu-device-id dt) v1))
         (v2 (mpvi-eval-js v1)))
    (unless (string-match "sign=\\([^=&]+\\)" v2)
      (user-error "解析签名失败"))
    (match-string 1 v2)))

(defun mpvi-douyu-room-info (id-or-url)
  "Return room info for ID-OR-URL of Douyu."
  (interactive "s输入斗鱼的房间号或直播间地址: ")
  (pcase-let* ((url (if (and (stringp id-or-url) (string-prefix-p "http" id-or-url))
                        id-or-url
                      (format "https://www.douyu.com/%s" id-or-url)))
               (`(,room-id ,func ,val) (mpvi-douyu-parse-page url))
               (`(,title ,nick) (mpvi-req-sync (format "https://www.douyu.com/betard/%s" room-id)
                                  :done (lambda (resp)
                                          (let ((ri (alist-get 'room resp)))
                                            (unless (equal (alist-get 'status ri) "1")
                                              (user-error "当前直播不在线"))
                                            (list (alist-get 'room_name ri)
                                                  (alist-get 'nickname ri))))))
               (tt (format "%d" (floor (float-time))))
               (sign (mpvi-douyu-extract-sign func val room-id tt))
               (stream (mpvi-req-sync (format "https://www.douyu.com/lapi/live/getH5Play/%s" room-id)
                         :data `((v . ,val) (did . ,mpvi-douyu-device-id) (tt . ,tt) (sign . ,sign))
                         :headers `(,@(when mpvi-douyu-cookie-string
                                        `((cookie . ,mpvi-douyu-cookie-string))))
                         :done (lambda (resp)
                                 (unless (equal (alist-get 'error resp) 0)
                                   (user-error "解析失败: %s" (alist-get 'msg resp)))
                                 (alist-get 'data resp))))
               (urls (list (format "%s/%s" (alist-get 'rtmp_url stream) (alist-get 'rtmp_live stream)))))
    (list :nick nick :title title :urls urls)))

(cl-defmethod mpvi-extract ((_ (eql :douyu.com)) url &rest _)
  "Return the real video URL for 斗鱼."
  (let* ((info (mpvi-douyu-room-info url))
         (urls (plist-get info :urls))
         (title (concat "[" (plist-get info :nick) "]: " (plist-get info :title))))
    (list :origin url :items urls :title title :logo "斗鱼")))


;;;;; 抖音 (douyin.com)

(defun mpvi-douyin-parse-page (url)
  (let* ((html (mpvi-req-sync url
                 :headers `((cookie . ,(format "__ac_nonce=%s" (substring (secure-hash 'sha1 (current-time-string)) 0 21))))))
         (regexp "self\\.__pace_f\\.push(\\[[0-9]+,\\(\"\\w+:[^<]+?\"\\)\\])</script>")
         (res nil))
    (with-temp-buffer
      (save-excursion (insert html))
      (while (and (null res) (re-search-forward regexp nil t))
        (let ((s (match-string 1)))
          (when (and (cl-search "state" s) (cl-search "streamStore" s))
            (setq res s))))
      (pdd-string-to-object 'json
        (cdr (pdd-split-string-by (pdd-string-to-object 'json res) ":"))))))

(defun mpvi-douyin-room-info (id-or-url)
  "Return room info for ID-OR-URL of Douyin."
  (interactive "s输入抖音直播的房间号或直播间地址: ")
  (let* ((url (if (and (stringp id-or-url) (string-prefix-p "http" id-or-url))
                  id-or-url
                (format "https://live.douyin.com/%s" id-or-url)))
         (info (alist-get 'state (aref (mpvi-douyin-parse-page url) 3))))
    (let-alist info
      (unless (equal .roomStore.roomInfo.room.status 2)
        (user-error "直播已结束"))
      (let* ((title .roomStore.roomInfo.room.title)
             (nick  .roomStore.roomInfo.anchor.nickname)
             (stream .streamStore.streamData.H264_streamData.stream)
             (urls (cl-loop for v in '(origin hd sd ld)
                            for V = (pcase v ('ld "标清") ('sd "高清") ('hd "超清") ('origin "蓝光"))
                            for s = (alist-get 'main (alist-get v stream))
                            when s
                            collect (propertize (alist-get 'hls s) 'line-prefix (format "[hls/%s] " V)) and
                            collect (propertize (alist-get 'flv s) 'line-prefix (format "[flv/%s] " V)))))
        (if (called-interactively-p 'any)
            (message "\n[%s/%s]\n\n%s" nick title (mapconcat (lambda (u) (format "- %s" u)) urls "\n"))
          (list :urls urls :nick nick :title title))))))

(cl-defmethod mpvi-extract ((_ (eql :douyin.com)) url &rest _)
  "Return the real video URL for 抖音直播."
  (when (string-match-p "live[/.]" url)
    (let* ((info (mpvi-douyin-room-info url))
           (urls (plist-get info :urls))
           (title (concat "[" (plist-get info :nick) "]: " (plist-get info :title))))
      (list :origin url :items urls :title title :logo "抖音"))))

(provide 'mpvi-living)

;;; mpvi-living.el ends here
