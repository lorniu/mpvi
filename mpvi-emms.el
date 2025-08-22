;;; mpvi-emms.el --- Patch EMMS for better integration -*- lexical-binding: t -*-

;; Copyright (C) 2023 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; URL: https://github.com/lorniu/mpvi
;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Patch `emms-player-mpv.el' for better integration
;;
;; 1) Make EMMS support Windows
;;
;;    Emacs don't have builtin way of connecting to Windows named pipe server,
;;    here improve `make-network-process' to support it through PowerShell.
;;
;; 2) The APIs in `emms-player-mpv.el' are too tightly tied to EMMS playlist
;;
;;    Refactor the APIs to make them can be used standalone, that is, can
;;    connect MPV and play videos without updating EMMS playlist and so on.
;;

;;; Code:

(require 'emms-player-mpv)
(require 'emms-source-file)

(defun mpvi-emms-video-regex ()
  "Regexp of extensions of local video file that MPV supported."
  (emms-player-get emms-player-mpv 'regex))


;;; Make EMMS support Windows through PowerShell

(defun mpvi-emms-player-mpv-ipc-init (func)
  "Advice FUNC `emms-player-mpv-ipc-init', add Windows support."
  (if (memq system-type '(cygwin windows-nt))
      (mpvi-emms-connect-to-win-named-pipe emms-player-mpv-ipc-socket)
    (funcall func)))

(defun mpvi-emms-player-mpv-ipc-recv (func json-string)
  "Advice FUNC `emms-player-mpv-ipc-recv', patch for windows.
JSON-STRING is json format string return by ipc process."
  (if (memq system-type '(cygwin windows-nt))
      (let (json)
        (emms-player-mpv-debug-msg "json << %s" json-string)
        (condition-case err
            (setq json (json-read-from-string json-string))
          ;; PowerShell will output error message when something goes wrong to standard output,
          ;; It's not json format, so catch it here
          (error (erase-buffer) (signal (car err) (cdr err))))
        (let ((rid (alist-get 'request_id json)))
          (when (and rid (not (alist-get 'command json))) ; skip the echoed 'command' for Windows
            (emms-player-mpv-ipc-req-resolve
             rid (alist-get 'data json) (alist-get 'error json)))
          (when (alist-get 'event json)
            (emms-player-mpv-event-handler json)
            ;; Only call the hook when video is played from EMMS
            (when (emms-playlist-current-selected-track)
              (run-hook-with-args 'emms-player-mpv-event-functions json)))))
    (funcall func json-string)))

(defun mpvi-emms-make-named-pipe-process-for-windows (pipe process buffer filter sentinel)
  "Create a PROCESS for communicating with MPV via a named PIPE on Windows."
  (let* ((ps1 "
$ErrorActionPreference = 'Stop';
$pipename = '%s';
try {
    $utf8 = [System.Text.UTF8Encoding]::new($false);
    [Console]::OutputEncoding = $utf8;

    $pipe = [System.IO.Pipes.NamedPipeClientStream]::new('.', $pipename, [System.IO.Pipes.PipeDirection]::InOut, [System.IO.Pipes.PipeOptions]::Asynchronous);
    $pipe.Connect(5000);

    $pipeReader = [System.IO.StreamReader]::new($pipe, $utf8);
    $pipeWriter = [System.IO.StreamWriter]::new($pipe, $utf8);
    $pipeWriter.AutoFlush = $true;

    $stdInStream = [System.Console]::OpenStandardInput();
    $stdInReader = [System.IO.StreamReader]::new($stdInStream, $utf8);

    $pipeReadTask = $pipeReader.ReadLineAsync();
    $consoleReadTask = $stdInReader.ReadLineAsync();

    while ($true) {
        $tasks = @($pipeReadTask, $consoleReadTask);
        $waitIndex = [System.Threading.Tasks.Task]::WaitAny($tasks);

        if ($waitIndex -eq 0) {
            $completedTask = $pipeReadTask;
            if ($completedTask.IsFaulted) { break; }
            $line = $completedTask.GetAwaiter().GetResult();
            if ($line -eq $null) { break; }
            [Console]::Out.WriteLine($line);
            [Console]::Out.Flush();
            $pipeReadTask = $pipeReader.ReadLineAsync();
        }
        elseif ($waitIndex -eq 1) {
            $completedTask = $consoleReadTask;
            if ($completedTask.IsFaulted) { break; }
            $line = $completedTask.GetAwaiter().GetResult();
            if ($line -eq $null) { break; }
            $pipeWriter.WriteLine($line);
            $consoleReadTask = $stdInReader.ReadLineAsync();
        }
    }
}
catch [System.TimeoutException] {
    [Console]::Error.WriteLine('mpvi-error: Timeout: Failed to connect to MPV pipe: ' + $pipename);
}
catch {
    [Console]::Error.WriteLine('mpvi-error: An unexpected error occurred: ' + $_.ToString());
}
finally {
    if ($stdInReader) { $stdInReader.Dispose(); }
    if ($pipeReader) { $pipeReader.Dispose(); }
    if ($pipeWriter) { $pipeWriter.Dispose(); }
    if ($pipe) { $pipe.Dispose(); }
    [Console]::Error.WriteLine('mpvi: PowerShell bridge process terminated.');
}
")
         (cmd (format "& {%s}" (replace-regexp-in-string "[ \n\r\t]+" " " (format ps1 pipe )))))
    (make-process :name process
                  :connection-type 'pipe
                  :buffer (get-buffer-create buffer)
                  :noquery t
                  :coding 'utf-8-emacs-unix
                  :filter filter
                  :sentinel sentinel
                  :command (list "powershell" "-NoProfile" "-Command" cmd))))

(defun mpvi-emms-connect-to-win-named-pipe (pipename)
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
    (while (not (mpvi-emms-win-named-pipe-exists-p pipename))
      (sleep-for 0.05)))
  (let ((proc (mpvi-emms-make-named-pipe-process-for-windows
               pipename "emms-player-mpv-ipc" emms-player-mpv-ipc-buffer
               #'emms-player-mpv-ipc-filter #'emms-player-mpv-ipc-sentinel)))
    (with-timeout (5 (setq emms-player-mpv-ipc-proc nil)
                     (user-error "Connect to MPV failed"))
      (while (not (eq (process-status emms-player-mpv-proc) 'run))
        (sleep-for 0.05)))
    (setq emms-player-mpv-ipc-proc proc)))

(defun mpvi-emms-win-named-pipe-exists-p (pipename)
  "Check if named pipe with name of PIPENAME exists on Windows."
  (unless (executable-find "powershell")
    (user-error "Cannot find PowerShell"))
  (with-temp-buffer
    (call-process "powershell" nil t nil
                  "-Command"
                  (format "& {Get-ChildItem \\\\.\\pipe\\ | Where-Object {$_.Name -eq '%s'}}"
                          pipename))
    (> (length (buffer-string)) 0)))


;;; Only update track when videos are played from EMMS buffer

(defun mpvi-emms-player-started (func player)
  "Advice FUNC `emms-player-started' when PLAYER MPV started."
  (if (emms-playlist-current-selected-track)
      (funcall func player)
    (setq emms-player-playing-p player
          emms-player-paused-p nil)))

(defun mpvi-emms-player-stopped (func)
  "Advice FUNC `emms-player-stopped' that only update track for EMMS."
  (if (emms-playlist-current-selected-track)
      (funcall func)
    (setq emms-player-playing-p nil)))


;;; Integrate `emms' with `mpvi-start'

(declare-function mpvi-start "mpvi.el" t t)

(defun mpvi-emms-player-start (func track)
  "Advice FUNC `emms-player-start' to play TRACK with `mpvi-start'."
  (if (eq (emms-track-type track) 'mpvi)
      (progn
        (setq emms-player-mpv-stopped nil)
        (emms-player-mpv-proc-playing nil)
        (mpvi-start (emms-track-get track 'name) 'emms))
    (funcall func track)))

(defun mpvi-emms-event-handler (func json-data)
  "Advice FUNC `emms-player-mpv-event-handler'.
Enhance it to deal with callbacks defined by `mpvi-event' and `mpvi-prop-event'.
JSON-DATA is the original argument that is the json data responsed from MPV."
  (let ((ev (intern (alist-get 'event json-data))))
    (funcall #'mpvi-event ev json-data)
    (when (eq ev 'property-change)
      (funcall #'mpvi-prop-event
               (intern (alist-get 'name json-data))
               (alist-get 'data json-data)
               (alist-get 'id json-data))))
  (funcall func json-data))

(cl-defgeneric mpvi-event (type data)
  "Callback for MPV event TYPE, with DATA as a json object or nil."
  (ignore type data))

(cl-defgeneric mpvi-prop-event (prop data id)
  "Callback for PROP changed. DATA is the changed value and ID is the identity."
  (ignore prop data id))


;;; Minor mode

;;;###autoload
(define-minor-mode mpvi-emms-integrated-mode
  "Global minor mode to toggle EMMS integration."
  :global t :group 'mpvi
  (if mpvi-emms-integrated-mode
      (progn
        (advice-add #'emms-player-mpv-ipc-init :around #'mpvi-emms-player-mpv-ipc-init)
        (advice-add #'emms-player-mpv-ipc-recv :around #'mpvi-emms-player-mpv-ipc-recv)
        (advice-add #'emms-player-mpv-event-handler :around #'mpvi-emms-event-handler)
        (advice-add #'emms-player-start :around #'mpvi-emms-player-start)
        (advice-add #'emms-player-started :around #'mpvi-emms-player-started)
        (advice-add #'emms-player-stopped :around #'mpvi-emms-player-stopped))
    (advice-remove #'emms-player-mpv-ipc-init #'mpvi-emms-player-mpv-ipc-init)
    (advice-remove #'emms-player-mpv-ipc-recv #'mpvi-emms-player-mpv-ipc-recv)
    (advice-remove #'emms-player-mpv-event-handler #'mpvi-emms-event-handler)
    (advice-remove #'emms-player-start #'mpvi-emms-player-start)
    (advice-remove #'emms-player-started #'mpvi-emms-player-started)
    (advice-remove #'emms-player-stopped #'mpvi-emms-player-stopped)))

(provide 'mpvi-emms)

;;; mpvi-emms.el ends here
