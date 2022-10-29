# To launch Emacs while supporting fcitx (for Chinese etc input), we need to tell it a locale and xmodifiers
LC_CTYPE=zh_CN.UTF-8 XMODIFIERS=@im=fcitx emacs -fs
