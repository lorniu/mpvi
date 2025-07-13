[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![MELPA](https://melpa.org/packages/mpvi-badge.svg)](https://melpa.org/#/mpvi)

# Media Tool based on EMMS and MPV

[点击查看《中文版文档》](README-zh.md).

This enhances Emacs by integrating the powerful mpv media player with EMMS and Org mode.
This allows for a seamless experience of watching videos, taking timestamp notes, and managing media directly within Emacs.

A key feature is adding full Windows support to EMMS via a PowerShell bridge, a feature not available by default.

### Features

- Play local files and remote URLs with mpv, controlled from Emacs
- Control/Manage videos interactively in Emacs. Such as Download/Clip/Convert/Screenshot/OCR and so on
- Adds full support for EMMS on Windows, which can be used independently
- Integrate with Org Mode deeply, making take video notes easily (with the help of timestamp link)

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
brew install mpv ffmpeg yt-dlp tesseract
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
  (setq mpvi-mpv-ontop-p t)
  (setq mpvi-mpv-border-p t)
  (setq mpvi-cmds-on-init
        '(((set_property autofit-larger "40%"))
          ((set_property geometry "-3%+8%")))))
```

You'd better config `yt-dlp` before use, e.g., edit `~/.config/yt-dlp/config`:
```
--cookies-from-browser edge
--format "bestvideo+bestaudio/best"
```

Also, you can config `mpv` globally by editing `~/.config/mpv/mpv.conf`, e.g.:
```
autofit-larger=80%
geometry=50%:50%
```

## Usage

Basic Commands:
- `M-x mpvi-open`: Play a local file or remote URL. While in the minibuffer:
  + `C-x <return>`: Add the file/URL to the EMMS playlist instead of playing
  + `C-x C-w`: Clip/download the file/URL
  + `C-x b`: Choose a video from your mpvi-favor-paths
- `M-x mpvi-clip`: Clip a video. Can be called on a file or an mpv link in Org mode
- `M-x mpvi-emms-add`: Add a file, directory, or URL to the EMMS playlist. Supports adding entire online playlists
- `M-x mpvi-switch-playlist`: switch video in playlist
- `M-x mpvi-toggle-ontop|mute|border|title-bar`: toggle xxx for mpv
- `M-x mpvi-toggle|delay|load-subtitle`: subtitle setup for mpv

When a video is playing, run `mpvi-seek` to open an interactive control interface in the minibuffer:
- `Space` Toggle play and pause
- `j/k/l` Change the playback speed
- `n/p/N/P/M-n/M-p/C-l` Seek to any position smartly
- `s/C-s/C-i` Multiple ways to take screenshots
- `r/C-r` OCR recognition of the current playback screen
- `t/C-t` Copy subtitle of the current playback screen
- `c/C-c` Download/clip/transcode current playing video
- `v/C-v` Switch playlist/category
- `o/C-o` Switch to system program (for example, browser) to continue the playing
- `i` Insert timestamp link into current buffer
- `q/C-q` quit minibuffer

In Org Mode buffer, a custom link with format `[[mpv:PATH#START-END][DESCRIPTION]]` is used to interactive with a video.
The link can be inserted through `mpvi-insert` or `i` in seek minibuffer for a playing video, or you can edit it directly like a normal org link.

The timestamp link is clickable and responses below shortcuts when cursor on it:
- `, ,` Play video in current link
- `, s` Enter `mpvi-seek` interface
- `, a` Change the start time in current link
- `, b` Change the end time in current link
- `, v` Preview the screenshot of current time position in current link
- `, c` Video download, transcode, clip and so on, **ALL IN ONE**
- `, h` show this help

Look the keymap definitions for more:
- `mpvi-open-map`
- `mpvi-seek-map`
- `mpvi-org-link-map`

## License

This package is licensed under the MIT License.
