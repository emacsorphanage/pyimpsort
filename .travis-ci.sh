#!/usr/bin/env bash

# § [ Emacs setup ] ────────────────────────────────────────────────────────────
case "$EMACS" in
  emacs24)
    sudo add-apt-repository -y ppa:cassou/emacs
    sudo apt-get update -qq
    sudo apt-get -qq install emacs24 emacs24-el
    ;;
  emacs-snapshot)
    sudo add-apt-repository -y ppa:ubuntu-elisp/ppa
    sudo apt-get update -qq
    sudo apt-get install -qq  emacs-snapshot emacs-snapshot-el
    ;;
  *)
    echo "Unsupported EMACS='$EMACS'"
    exit 1
    ;;
esac

curl -fsSkL https://raw.github.com/cask/cask/master/go | python

# § [ Python setup ] ───────────────────────────────────────────────────────────
pip install -U pytest
# packages to simulate thirdparty modules
pip install -U django python-dateutil
