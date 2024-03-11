[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![MELPA](https://melpa.org/packages/mpvi-badge.svg)](https://melpa.org/#/mpvi)

# Media Tool based on EMMS and MPV

What can do:
- Watch local, remote and living media (danmaku support for some)
- Manage playlist with EMMS, and control playing with MiniBuffer
- Integrate Download/Clip/Screenshot/OCR videos and so on
- Integrated with Org Mode, make taking video-notes easily
- Make MPV player in EMMS support Windows

I just wrote this for fun. Now release it for the ones who need it.

## Installation

- It's based on [EMMS](https://www.gnu.org/software/emms/), make sure it is installed
- Download and load this package `(require 'mpvi)`
- Install [mpv](https://mpv.io/) and [yt-dlp](https://github.com/yt-dlp/yt-dlp), they are the main dependencies.
- [Optional] Install dependencies you need:
  + [ffmpeg](https://ffmpeg.org/), used to clip video
  + [tesseract](https://github.com/tesseract-ocr/tesseract), used to OCR
  + [danmaku2ass](https://github.com/m13253/danmaku2ass), danmaku file converter, used when watching bilibili.com
  + [seam](https://github.com/Borber/seam), living video extractor, used when watching some live sites

For Arch Linux User, all dependencies with one command:
```sh
yay -S mpv ffmpeg yt-dlp tesseract xclip danmaku2ass-git seam-git
```

Windows User can install dependencies with `winget` or `scoop`:
```sh
winget install mpv yt-dlp ffmpeg Tesseract-OCR
```

## Usage

Core commands:
1. `mpvi-open`, open video (local or remote) with MPV
2. `mpvi-seek`, control opened MPV with minibuffer
3. `mpvi-insert`, insert timestamp link of video to current org buffer
4. `mpvi-clip`, download & clip & transcode videos via ffmepg/ytdlp
5. `mpvi-emms-add`, add video link/file to EMMS playlist

Command `mpvi-seek` is the most frequently used one. It integrates many functions through minibuffer:
- `i` Insert timestamp link into current buffer
- `Space` Toggle play and pause
- `j/k/l` Change the playback speed
- `n/p/N/P/M-n/M-p/C-l` Seek to any position smartly
- `s/C-s/C-i` Multiple ways to take screenshots
- `r/C-r` OCR recognition of the current playback screen
- `t/C-t` Copy subtitle of the current playback screen
- `c/C-c` Download/clip/transcode current playing video
- `v/C-v` Switch playlist/category
- `o/C-o` Switch to system program (for example, browser) to continue the playing
- `q/C-q` quit minibuffer

Timestamp link is link of format `[mpv:https://xxx.com#10-30]`. It's clickable and responses below shortcuts when cursor on it:
- `, ,` Play video in current link
- `, s` Enter `mpvi-seek` interface
- `, a` Change the start time in current link
- `, b` Change the end time in current link
- `, v` Preview the screenshot of current time position in current link
- `, c` Video download, transcode, clip and so on, **ALL IN ONE**.

Look the keymap definitions for more:
- `mpvi-open-map`
- `mpvi-seek-map`
- `mpvi-org-link-map`

## Miscellaneous

Thanks to similar projects in the community, you teach me a lot and give me so much inspiration.

Thanks to open source software like MPV/FFMPEG/EMACS, you make the world more wonderful.

Finally, thanks to all the platforms and authors who contributed great videos, you guys make me happier and more powerful. :)
