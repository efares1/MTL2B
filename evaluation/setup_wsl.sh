#!/bin/bash
# One-time installation of the evaluation tools in WSL Ubuntu.
# Run inside Ubuntu:   sudo bash setup_wsl.sh
set -e
apt-get update
apt-get install -y git bc ocaml ocaml-dune menhir wget gnupg
# Spot: Ubuntu package if available, otherwise the LRE Debian repository.
if ! apt-get install -y spot; then
  wget -q -O - https://www.lrde.epita.fr/repo/debian.gpg | gpg --dearmor > /usr/share/keyrings/lrde.gpg
  echo 'deb [signed-by=/usr/share/keyrings/lrde.gpg] http://www.lrde.epita.fr/repo/debian/ stable/' \
    > /etc/apt/sources.list.d/lrde.list
  apt-get update
  apt-get install -y spot
fi
ltl2tgba --version | head -1
dune --version
echo "Setup done."
