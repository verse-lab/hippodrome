#!/bin/sh
brew install autoconf automake cmake opam pkg-config sqlite gmp mpfr
brew cask install java
git clone git@github.com:andrecostea/infer.git
cd infer
./build-infer.sh java
sudo make install
export PATH=`pwd`/infer/bin:$PATH