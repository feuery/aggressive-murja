#!/bin/env bash
set -euo pipefail

apt-get update
apt-get install sbcl curl libssl-dev -q -y

curl https://beta.quicklisp.org/quicklisp.lisp -o ~/quicklisp.lisp
sbcl --load ~/quicklisp.lisp --load ./install_ql.lisp

current_dir=$(pwd)

mkdir ~/common-lisp
pushd ~/common-lisp
ln -s $current_dir
popd

ls -la ~/common-lisp

sbcl --load ./build_murja.lisp

mv murja_server ..

apt-get remove sbcl -q -y
