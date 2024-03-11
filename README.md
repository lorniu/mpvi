[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![MELPA](https://melpa.org/packages/mpvi-badge.svg)](https://melpa.org/#/mpvi)

# Media Tool based on EMMS and MPV

Knock knock, [English Help](README-en.md).

## 简介

将 MPV 跟 EMMS/ffmpeg/yt-dlp/Org-Mode 等有机结合，娱乐、学习两不误:
- 支持本地视频、网络视频、网络直播 (也支持音频、弹幕)
- 可以在 Emacs 中灵活地控制播放进度、播放速度等
- 可以方便地对视频进行下载、转码、截图、OCR 等操作
- 跟 Org Mode 深度集成，视频笔记、轻轻松松 (带时间戳的链接)
- 可以控制 EMMS 播放的视频和音频。为 EMMS 增加了 Windows 平台的支持

结合下面项目可以更方便观看 B 站视频:
- https://github.com/lorniu/bilibili.el

这是很久之前知识焦虑的时候，为了消化收藏夹中的视频而写。

最近几天将其翻新了一下。因为发现用它来拉片、看直播，简直不要太爽!

## 安装

基于 [EMMS](https://www.gnu.org/software/emms/)，请确保其已安装。然后下载并加载本包 `(require 'mpvi)`。


之后安装相关依赖。除了 [mpv](https://mpv.io/) 是必须的外，其他的是可选的:
- [yt-dlp](https://github.com/yt-dlp/yt-dlp)，如果要看网络视频，这个必须要安装
- [ffmpeg](https://ffmpeg.org/)，用来对视频进行剪辑之类的操作
- [tesseract](https://github.com/tesseract-ocr/tesseract)，如果需要文字识别 (OCR) 功能才安装
- [danmaku2ass](https://github.com/m13253/danmaku2ass)，安装这个后，看 B 站视频就有弹幕啦
- [seam](https://github.com/Borber/seam)，用来解析直播链接，要看直播的需要安装

对于 Arch Linux 用户，一键安装依赖:
```sh
yay -S mpv yt-dlp ffmpeg tesseract danmaku2ass-git seam-git xclip
```

对于 Windows 用户，danmaku2ass 和 seam 需要从 Github 下载，其他可以通过 winget 或 scoop 安装:
```sh
winget install mpv yt-dlp ffmpeg Tesseract-OCR
```

## 使用

核心命令：
1. `mpvi-open`，打开视频文件或网络链接
2. `mpvi-seek`，通过 minibuffer 的方式对播放的视频进行控制
3. `mpvi-insert`，在 org buffer 中插入当前播放视频的带时间戳的链接
4. `mpvi-clip`，借助 ffmepg/ytdlp 实现视频的下载、剪辑、转码
5. `mpvi-emms-add`，向 EMMS playlist 中添加视频路径或链接

其中 `mpvi-seek` 是最常用的，它通过 minibuffer 集成了很多功能。比如:
- `i` 在 buffer 中插入时间戳链接
- `Space` 切换暂停与播放
- `j/k/l` 调整播放速度
- `n/p/N/P/M-n/M-p/C-l` 等实现各种维度的播放进度调整
- `s/C-s/C-i` 等实现各种方式的截图
- `r/C-r` 对当前播放页面进行 OCR 识别并复制结果
- `t/C-t` 复制当前页面的字幕或弹幕
- `c/C-c` 下载、裁剪、转码当前播放视频
- `v/C-v` 切换网络视频 playlist/category 里的视频
- `o/C-o` 切换到系统默认程序 (比如浏览器) 打开当前播放视频
- `q/C-q` 退出 minibuffer

时间戳是一个 `[mpv:https://xxx.com#10-30]` 格式的链接，可以直接点击。光标置于其上，有如下快捷键:
- `, ,` 播放当前链接中的视频
- `, s` 进入到 `mpvi-seek` 界面
- `, a` 更改链接中视频的开始时间
- `, b` 更改链接中视频的结束时间
- `, v` 预览链接中视频时间戳位置的画面
- `, c` 视频的下载、转码、截取。**All In One**, 很好用

其他补充:
- 详细的快捷键参见 map 定义: `mpvi-open-map`, `mpvi-seek-map`, `mpvi-org-link-map`。不合意可自行扩展、重新绑定
- 如果纯粹看视频/直播，并通过 minibuffer 操控 MPV，并不需要 Org Mode。但进行插入、编辑操作则必须在 Org Mode Buffer 中
- 支持视频分 P 播放。比如，可以直接用 `mpvi-open` 打开 B 站播放列表 url 或专栏合集 url。也可以通过 `mpvi-emms-add` 一次将所有分 P 视频都添加到 EMMS 中，之后通过 EMMS 管理列表并控制播放
- yt-dlp 对播放列表 (playlist) 信息的返回不够详细不够友好。这导致打开视频链接的时候可能会卡顿一下，并且切换列表的时候不会显示分 P 标题。这个只能通过上游解决，我懒得去提 PR，有点难受
- 目前直播仅支持斗鱼和抖音。其他的可以仿照 `mpvi-ps.el` 中的代码自行扩展。我不看其他的，所以没加
- 最大的遗憾是，看直播的时候没弹幕。**这种实时弹幕，不知道有没有啥实现的思路**
- 本来想把 `danmaku2ass` 和解析直播链接也用 `elisp` 重新实现一下的。后面想想这简直是自寻烦恼，这种洁癖要不得，第三方的依赖不差这一两个，因此作罢
- 结合视频网站的 API 可以实时抓取其收藏夹、热门视频等，结合本包食用，特别香；通过 EMMS 对视频列表进行管理，特别方便；再结合 Org Mode 对视频进行分类、整理，更香；再加上带视频时间戳链接的笔记，完美。现在基本可以做到不打开网页看 B 站视频了 (https://github.com/lorniu/bilibili.el)

## 其他

这个项目的初衷是自己爽。现在整理出来，如果有人喜欢，我也会很开心。

欢迎大家沟通交流，互相学习、共同进步。

感谢社区中的类似项目，你们给我了更多灵感。

感谢 mpv/ffmpeg/emacs 等开源软件，你们让世界更美好。

最后，感谢所有贡献了优秀视频的平台和作者，你们让我更焦虑、也更强大（~~误~~）。
