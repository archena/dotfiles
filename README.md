# Introduction

A repository of my dotfiles.

No guarantees are given regarding the usefulness or safety of these files. Released freely for any use - no attribution expected.

http://github.com/archena

## Installation

```bash
cd ~
git clone git@github.com:archena/dotfiles.git
bash dotfiles/configure.sh
```

## How it works

The `configure.sh` script creates symlinks in your home directory to each of the files in the repository. For instance, `~/.inputrc` is a symlink to `~/dotfiles/.inputrc`.
