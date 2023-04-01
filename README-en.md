# Integrate Org with Video

## Installation

- Make sure `mpv.el` is installed
- Download and add this repo to your `load-path`, then `(require 'org-mpvi)`
- Install the dependencies:
  + [mpv](https://mpv.io/), required
  + [yt-dlp](https://github.com/yt-dlp/yt-dlp), for web video
  + [ffmpeg](https://ffmpeg.org/), used to clip video
  + [seam](https://github.com/Borber/seam), used to extract video url for living
  + [danmaku2ass](https://github.com/m13253/danmaku2ass), used to convert danmaku file
  + [tesseract](https://github.com/tesseract-ocr/tesseract), used to OCR

For Arch Linux User:
```sh
yay -S mpv ffmpeg yt-dlp tesseract xclip danmaku2ass-git seam-git
```

## Usage

Core commands:
1. `org-mpvi-open`, open local video or remote video with MPV
2. `org-mpvi-seek`, use minibuffer to control opened MPV
3. `org-mpvi-insert`, insert link with timestamp to current org buffer

Look the maps for the keybinds:
- `org-mpvi-open-map`
- `org-mpvi-seek-map`
- `org-mpvi-link-keymap` (mpv link at point)
