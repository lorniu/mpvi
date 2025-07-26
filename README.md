[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![MELPA](https://melpa.org/packages/mpvi-badge.svg)](https://melpa.org/#/mpvi)

# Watch video and take interactive video notes in Emacs

This enhances Emacs by integrating the powerful mpv media player with EMMS and Org mode.
This allows for a seamless experience of watching videos, taking timestamp notes, and control mpv directly within Emacs.

A key feature is adding full Windows support to EMMS via a PowerShell bridge, a feature not available by default.

### Features

- Play local files and remote URLs with mpv, controlled from Emacs
- Operate videos interactively in Emacs. Such as Download/Clip/Convert/Screenshot/OCR and so on
- Integrate with Org Mode deeply, making take video notes easily (timestamp link)
- Adds full support for EMMS on Windows, which can be used independently

## Installation

Except the required [mpv](https://mpv.io/) and [EMMS](https://www.gnu.org/software/emms/), there are some optional dependencies (for enhanced features):
+ [yt-dlp](https://github.com/yt-dlp/yt-dlp): For playing/downloading online videos
+ [ffmpeg](https://ffmpeg.org/): For clipping and converting media
+ [tesseract](https://github.com/tesseract-ocr/tesseract): For OCR on screenshots
+ [biliass](https://github.com/yutto-dev/biliass): danmaku file converter, used for some video sites like bilibili.com

Ensure mpv and any desired optional tools (yt-dlp, ffmpeg, etc.) are installed and available in your system's PATH:
```shell
# For Arch Linux User
yay -S mpv ffmpeg yt-dlp tesseract xclip biliass-git

# For macOS User
brew install mpv ffmpeg yt-dlp tesseract tesseract-lang
pipx install biliass

# For Windows User
winget install mpv ffmpeg yt-dlp Tesseract-OCR
pip install biliass
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

Play a video with command `mpvi-open` or through `emms`:
- `mpvi-open` accepts a local video file or remote video url
- `mpvi-emms-add` is used to add a file or url to the EMMS playlist

Then the playing video can be controled with commands below:
- `mpvi-speed / mpvi-position / mpvi-volume` tune
- `mpvi-toggle-fullscreen|ontop|mute|border|title-bar` toggle
- `mpvi-toggle|load|dalay|capture-subtitle` subtitle
- `mpvi-switch-playlist` playlist
- `mpvi-screenshot / mpvi-capture-ocr` screenshot/ocr
- `mpvi-browse` Handoff playing to system program such as browser

Use `mpvi-export` to export a video, that is:
- Download a remote video to local disk (through `yt-dlp`)
- Clip or transcode a local video (through `ffmpeg`)
- All functions are integrated into this single command, making it very flexible and convenient

### Take video notes in Org Mode with `mpvi-insert`

Insert a timestamp link to org buffer with command `mpvi-insert`:
- The format of link is `[[mpv:PATH#TIME][TIME]]` or `[[mpv:PATH#START-END][START -> END]]`
- Put cursor on the link, and invoke `mpvi-insert` again to update it (with `C-u` prefix, update the END)
- Click the timestamp link to  play back the video

When cursor is on the link, keys like `,h`, `,a`, `,b`, `,v` `,e` can be used. See `mpvi-org-link-map` for details.

Another command `mpvi-insert-screenshot` is used to insert a screenshot as attachment to the org buffer.

### Interactively control playing with `mpvi-control` or `mpvi-seek`

Command `mpvi-control` can popup a dedicated buffer as a control panel.

Except showing playing state, various keybinds can be used in the panel:
- `Space` toggle pause
- `n p N P M-n M-p C-l . , ← →` change position
- `j k l [ ] { } Backspace` change speed
- `0 9 ↑ ↓` change volume
- `s C-s` screenshot `v z t C-t` subtitle
- `r` ocr `e` export `c` playlist `o` browse
- `m` mute `T` ontop `f` fullscreen
- `i C-i` video notes
- `/ g` prompt for seek
- `q C-q` quit

Another interactive control command is `mpvi-seek`:
- It is similar with `mpvi-control` and almost share the same keybinds,
- but it uses a minibuffer to facilitate input and adjust playback progress.
- It is more suitable for situations that require instant action.

See `mpvi-control-map` and `mpvi-seek-map` for more keybind details.

## Miscellaneous

Related packages:
- https://github.com/lorniu/bilibili.el, watching Bilibili in Emacs

Issues and PRs are welcome.

Have a nice day.
