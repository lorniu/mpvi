[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![MELPA](https://melpa.org/packages/mpvi-badge.svg)](https://melpa.org/#/mpvi)

# Watch video and take interactive video notes in Emacs

This enhances Emacs by integrating the powerful mpv media player with EMMS and Org mode.
This allows for a seamless experience of watching videos, taking timestamp notes, and control mpv directly within Emacs.

A key feature is adding full Windows support to EMMS via a PowerShell bridge, a feature not available by default.

### Features

- Play local files and remote URLs with MPV player
- Control the running MPV player in Emacs, various ways
- Many utilities with videos are integrated, like Download/Clip/Convert/Screenshot/OCR
- Integrate with Org Mode deeply, making take interactive video notes easily
- Add full support for EMMS on Windows, which makes this can work on all platforms
- With the help of browser extension, play/process webpage videos through mpvi directly

## Installation

Except the required [mpv](https://mpv.io/) and [EMMS](https://www.gnu.org/software/emms/), there are some optional dependencies (for enhanced features):
+ [yt-dlp](https://github.com/yt-dlp/yt-dlp): For playing/downloading online videos
+ [ffmpeg](https://ffmpeg.org/): For clipping and converting media
+ [tesseract](https://github.com/tesseract-ocr/tesseract): For OCR on screenshots
+ [biliass](https://github.com/yutto-dev/biliass): danmaku file converter, used for some video sites like bilibili.com

Ensure mpv and any desired optional tools (yt-dlp, ffmpeg, etc.) are installed and available in your system's PATH:
```shell
# For Arch Linux User

yay -S mpv ffmpeg yt-dlp tesseract xclip
# yay -S biliass-git

# For macOS User

brew install mpv ffmpeg yt-dlp tesseract tesseract-lang
# pipx install biliass

# For Windows User

winget install mpv ffmpeg yt-dlp Tesseract-OCR
# pip install biliass
```

Then install this package from MELPA and start to use:
```emacs-lisp
(use-package mpvi :ensure t)

(use-package mpvi
  :ensure t
  :config
  ;; M-x customize-group mpvi
  (setq mpvi-mpv-ontop-p t)
  (setq mpvi-mpv-border-p t)
  (setq mpvi-cmds-on-init
        '(((set_property autofit "40%x85%"))
          ((set_property geometry "-3%+8%")))))
```

You'd better config `yt-dlp` before use, e.g., edit `~/.config/yt-dlp/config`:
```
--cookies-from-browser edge
--format "bestvideo+bestaudio/best"
```

Also, you can config `mpv` globally by editing `~/.config/mpv/mpv.conf`, e.g.:
```
autofit=40%x85%
geometry=50%:50%
```

## Usage

### Commands to open and operate the video

To start a video or playlist, use commands below:
- `mpvi-play` open a video or playlist, no matter local or remote. Prefer the file or url under cursor.
- `mpvi-add-playlist` add file, url or playlist to MPV player for playing later.
- `mpvi-add-emms` add file, url or playlist to EMMS for playing later.

Also you can redirect videos on a webpage to mpvi directly, steps:
1. Install the Browser Extension (`M-x mpvi-install-browser-extension`)
2. Start the WebSocket Server (`M-x mpvi-websocket-start`)
3. Send Video to mpvi (right-click a video link on a webpage and select context menu under `[MPVi in Emacs]`, such as `[play]` for playing it with MPV player and `[export]` for transforming and downloading it)

When the player is running, control it with commands below:
- `mpvi-next` switch in playlist, smartly
- `mpvi-speed / mpvi-time / mpvi-volume / mpvi-geofit` tune
- `mpvi-toggle-fullscreen|ontop|mute|video|border|title-bar` toggle
- `mpvi-toggle|load|dalay|capture-subtitle` subtitle
- `mpvi-screenshot / mpvi-capture-ocr` screenshot/ocr
- `mpvi-browse` Handoff playing to system program such as browser
- `mpvi-cmd / mpvi-set` Interactive exec command / set property for current player

Use `mpvi-export` to export a video, that is:
- Download a remote video to local disk (through `yt-dlp`)
- Clip or transcode a local video (through `ffmpeg`)
- All functions are integrated into this single command, making it very flexible and convenient

### Take video notes in Org Mode with `mpvi-insert`

Insert a timestamp link to org buffer with command `mpvi-insert`:
- The format of link is `[[mpv:PATH#TIME][TIME]]` or `[[mpv:PATH#START-END][START -> END]]`
- Put cursor on the link, and invoke `mpvi-insert` again to update it (with `C-u` prefix, update the END)
- The timestamp link can be edited directly, like a normal link
- Click the timestamp link to play back the video. If end time exists, it will play as A-B loop

When cursor is on the link, keys like `,h`, `,a`, `,b`, `,v` `,e` can be used. See `mpvi-org-link-map` for details.

Another command `mpvi-insert-screenshot` is used to insert a screenshot as attachment to the org buffer.

### Interactively control playing with `mpvi-control` or `mpvi-seek`

Command `mpvi-control` can popup a dedicated buffer as a control panel.

Except showing playing state, various keybinds can be used in the control panel:
- `Space` toggle pause `m` mute `M` video `T` ontop `f` fullscreen
- `w` window position/size `L` A-B Loop
- `n p N P M-n M-p < > ← →` change time position `C-l` revert time position
- `j k [ ] { }` change speed `l Backspace` revert speed
- `0 9 ↑ ↓` change volume
- `s C-s` screenshot `v z t C-t` subtitle
- `r` ocr `e` export `c` playlist `o` browse
- `i` insert/update video link `I` update end time `C-i` insert screenshot
- `/ g` prompt for seek
- `` ` `` exec command `x` set property
- `q C-q C-g` quit

Another interactive control command is `mpvi-seek`:
- It is similar with `mpvi-control` and almost share the same keybinds,
- but it uses a minibuffer to facilitate input and adjust playback progress.
- It is more suitable for situations that require instant action.

See `mpvi-control-map` and `mpvi-seek-map` for more keybind details.

## Miscellaneous

Related packages:
- https://github.com/lorniu/bilibili.el, extension for watching Bilibili in Emacs

Issues and PRs are welcome. Be happy.
