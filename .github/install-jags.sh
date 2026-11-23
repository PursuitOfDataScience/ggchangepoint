#!/usr/bin/env bash
# JAGS 5 on an Ubuntu runner, for rjags and so for mcp. Every workflow that
# installs the Suggests on Linux runs this before setup-r-dependencies.
#
# rjags 5-5 (CRAN, 2026-11-21) needs JAGS 5, and its configure refuses the
# JAGS 4.3.2 that Ubuntu's apt ships ("pkg-config found JAGS 4.3.2 but rjags
# requires JAGS 5.x.y"). Linux runners build rjags from source, since Posit
# has no binary of it either, so the plain `apt-get install jags` this
# replaces stopped every Linux job at dependency setup (all-engines,
# 2026-11-23).
#
# apt is used whenever it offers JAGS 5 or later, so this lapses by itself
# once the runner image catches up. Until then the release below is built
# from its source tarball into /usr/local, where pkg-config and the dynamic
# linker look without being told. Bump the version and checksum together.
set -euo pipefail

jags_version=5.0.2
jags_sha256=80143e23fc6feca01d4cce440fd8f6aa865a03333ed36baf9e0ce30a8f9e3aea

# `apt-get update` exits non-zero if ANY configured repository fails, and the
# runner image ships third-party lists this package has nothing to do with:
# on 2026-09-09 Chrome's index returned "Hash Sum mismatch" and took down
# every Linux job. So drop those, and let only the installs decide.
sudo rm -f /etc/apt/sources.list.d/google-chrome.list \
           /etc/apt/sources.list.d/microsoft-prod.list
sudo apt-get update -o Acquire::Retries=3 || true

candidate=$(apt-cache policy jags | awk '/Candidate:/ { print $2 }')
if [ -n "$candidate" ] && [ "$candidate" != "(none)" ] &&
   dpkg --compare-versions "$candidate" ge 5; then
  sudo apt-get install -y jags
else
  echo "apt offers JAGS ${candidate:-none}; building JAGS $jags_version"
  sudo apt-get install -y gfortran liblapack-dev libblas-dev
  work=$(mktemp -d)
  curl -fsSL --retry 5 --retry-all-errors -o "$work/jags.tar.gz" \
    "https://sourceforge.net/projects/mcmc-jags/files/JAGS/5.x/Source/JAGS-$jags_version.tar.gz/download"
  echo "$jags_sha256  $work/jags.tar.gz" | sha256sum -c -
  tar -xzf "$work/jags.tar.gz" -C "$work"
  # Shared libraries only (rjags links nothing else) and no debug symbols:
  # libtool otherwise compiles every file twice, once for a static library.
  (
    cd "$work/JAGS-$jags_version"
    ./configure --prefix=/usr/local --disable-static --quiet CXXFLAGS=-O2
    make -j"$(nproc)" --silent
    sudo make install --silent
  )
  sudo ldconfig
fi

echo "JAGS $(pkg-config --modversion jags) is installed"
