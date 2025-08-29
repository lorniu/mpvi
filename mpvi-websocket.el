;;; mpvi-websocket.el --- Communicate through websocket -*- lexical-binding: t -*-

;; Copyright (C) 2023 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; URL: https://github.com/lorniu/mpvi
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Communicate through websocket from external program such as browsers.
;;
;; This allows handle webpage videos through mpvi in Chrome, Edge and Firefox.
;;
;; Invoke `mpvi-install-browser-extension' to generate the files and install
;; them according to the guide, then you will find `MPVi in Emacs' in the
;; context menu of links.
;;
;; With the context menu, you can play/add/export videos from browser directly.
;; The tasks are sent to `mpvi' via websocket, so make sure the websocket server
;; is actived through `mpvi-websocket-start'.

;;; Code:

(require 'json)

(defvar mpvi-websocket-port 19123
  "The port for the WebSocket server that the extension will connect to.
If this is changed, remember to modify connection port of external program.")

(defvar mpvi-websocket-server nil)

(declare-function mpvi-start "mpvi" t)
(declare-function mpvi-add-emms "mpvi" t)
(declare-function mpvi-export "mpvi" t)
(declare-function mpvi-log "mpvi" t)
(declare-function websocket-server "websocket" t)
(declare-function websocket-server-close "websocket" t)
(declare-function websocket-frame-text "websocket" t)

;;;###autoload
(defun mpvi-websocket-start ()
  "Start a WebSocket server to listen for external programs."
  (interactive)
  (unless (require 'websocket nil t)
    (user-error "You should install package `websocket' first"))
  (ignore-errors (mpvi-websocket-stop))
  (setq mpvi-websocket-server
        (websocket-server
         mpvi-websocket-port
         :host "127.0.0.1"
         :on-message #'mpvi-websocket-message
         :on-open (lambda (_) (mpvi-log "[mpvi-websocket] client connected"))
         :on-close (lambda (_) (mpvi-log "[mpvi-websocket] client disconnected"))))
  (when (called-interactively-p 'any)
    (message "mpvi WebSocket server started on ws://127.0.0.1:%s" mpvi-websocket-port)))

;;;###autoload
(defun mpvi-websocket-stop ()
  "Stop the running WebSocket server."
  (interactive)
  (when mpvi-websocket-server
    (websocket-server-close mpvi-websocket-server)
    (setq mpvi-websocket-server nil))
  (when (and (null mpvi-websocket-server) (called-interactively-p 'any))
    (message "mpvi WebSocket server stopped")))

(defun mpvi-websocket-message (_ws frame)
  "Deal with message FRAME of websocket."
  (condition-case err
      (when-let* ((text (websocket-frame-text frame))
                  (pair (split-string text))
                  (url (cadr pair))
                  (type (intern (car pair))))
        (mpvi-log "[mpvi-websocket] received: %s | %s" type url)
        (cond ((memq type '(play playlist))
               (mpvi-start url type))
              ((eq type 'emms)
               (mpvi-add-emms url))
              ((eq type 'export)
               (select-frame-set-input-focus (selected-frame))
               (mpvi-export url))))
    (error (message "%s" err))))

;;;###autoload
(defun mpvi-install-browser-extension ()
  "Generate a browser extension for sending links to mpvi via WebSocket."
  (interactive)

  (let* ((target-dir (expand-file-name "mpvi-extension" (read-directory-name "Extenson save to: " nil nil t)))
         (gen (lambda (file content) (with-temp-file (expand-file-name file target-dir) (insert content)))))
    (unless (file-directory-p target-dir)
      (make-directory target-dir t))

    ;; 1. Generate manifest.json
    (let* ((manifest-v3 `(("manifest_version" . 3)
                          ("name" . "MPVi in Emacs")
                          ("version" . "1.0.0")
                          ("description" . "Send video links to mpvi in Emacs.")
                          ("permissions" . ("contextMenus","notifications","storage"))
                          ("host_permissions" . ("ws://localhost/*"))
                          ("options_ui" . (("page" . "options.html")))
                          ("background" . (("service_worker" . "background.js")))))
           (manifest-content (json-encode manifest-v3)))
      (funcall gen "manifest.json" manifest-content))

    ;; 2. Generate background.js
    (let ((background-script
           (format "
chrome.runtime.onInstalled.addListener(() => {
  chrome.contextMenus.create({
    id: 'mpvi-parent',
    title: 'MPVi in Emacs',
    contexts: ['link', 'video', 'page']
  });
  chrome.contextMenus.create({
    id: 'mpvi-play',
    parentId: 'mpvi-parent',
    title: 'play!',
    contexts: ['link', 'video', 'page']
  });
  chrome.contextMenus.create({
    id: 'mpvi-sp-1',
    parentId: 'mpvi-parent',
    type: 'separator',
    contexts: ['link', 'video', 'page']
  });
  chrome.contextMenus.create({
    id: 'mpvi-playlist',
    parentId: 'mpvi-parent',
    title: 'add to playlist',
    contexts: ['link', 'video', 'page']
  });
  chrome.contextMenus.create({
    id: 'mpvi-emms',
    parentId: 'mpvi-parent',
    title: 'add to EMMS',
    contexts: ['link', 'video', 'page'],
  });
  chrome.contextMenus.create({
    id: 'mpvi-export',
    parentId: 'mpvi-parent',
    title: 'export...',
    contexts: ['link', 'video', 'page'],
  });
});
function showNotification(title, message) {
  chrome.notifications.create('mpvi-notification-' + Math.trunc(Math.random() * 1000), {
    type: 'basic',
    iconUrl: 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII=',
    title: title,
    message: message,
    priority: 1
  });
}
function sendToMpvi(url, type) {
  chrome.storage.sync.get({ port: %d }, (items) => {
    const socket = new WebSocket(`ws://localhost:${items.port}`);
    socket.onopen = () => {
      socket.send(type + ' ' + url);
      socket.close();
    };
    socket.onerror = () => {
      showNotification('play with mpvi', 'Could not connect to mpvi. Ensure the WebSocket server is running with `M-x mpvi-websocket-start` in Emacs.');
    };
    socket.onclose = () => {};
  });
}
chrome.contextMenus.onClicked.addListener((info, tab) => {
  const urlToSend = info.linkUrl || info.srcUrl || info.pageUrl;
  if (urlToSend && info.menuItemId.startsWith('mpvi-')) {
    sendToMpvi(urlToSend, info.menuItemId.substr(5));
  }
});
" mpvi-websocket-port)))
      (funcall gen "background.js" background-script))

    ;; 3. Generate options.html
    (let ((options-html
           "<html><body>
<p><label for=port style='margin-right:1em;'>WebSocket Port:</label><input id=portInput type=number/></p>
<p><button id=saveBtn>Save</button><i id=info style='color:red;margin-left:1em'></i></p>
<script src='options.js'></script>
</body></html>")
          (options-script
           (format "
chrome.storage.sync.get({ port: %d }, (items) => {
  portInput.value = items.port;
});
saveBtn.addEventListener('click', () => {
  const port = parseInt(portInput.value, 10);
  if (port >= 1024 && port <= 65535) {
    chrome.storage.sync.set({ port: port }, () => {
      info.innerText = 'Settings saved!';
      setTimeout(() => { info.innerText = ''; }, 2000);
    });
  } else {
    info.innerText = 'Invalid port number.';
  }
});
" mpvi-websocket-port)))
      (funcall gen "options.html" options-html)
      (funcall gen "options.js" options-script))

    ;; 4. Provide instructions to the user
    (message "Browser extension generated in: %s" (abbreviate-file-name target-dir))
    (with-current-buffer (get-buffer-create "*MPVI Extension Instructions*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Browser Extension Generated!\n\nLocation: %s\n\n" target-dir))
        (insert "To install the extension:\n\n")
        (insert "1. Open your browser's extension management page:\n")
        (insert "   - Chrome/Edge: chrome://extensions\n")
        (insert "   - Firefox: about:addons\n\n")
        (insert "2. Enable \"Developer mode\" (usually a toggle in the side corner).\n\n")
        (insert "3. Click \"Load unpacked\" and select the following directory:\n")
        (insert (format "   %s\n\n" (file-name-as-directory target-dir)))
        (insert "4. The \"MPVi in Emacs\" option should now appear in your right-click menu.\n\n")
        (insert "Remember to active the WebSocket server with `M-x mpvi-websocket-start`.\n")
        (special-mode)
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))

(provide 'mpvi-websocket)

;;; mpvi-websocket.el ends here
