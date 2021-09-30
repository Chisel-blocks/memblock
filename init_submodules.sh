#!/bin/sh
#Init submodules in this dir, if any
DIR="$( cd "$( dirname $0 )" && pwd )"
git submodule sync
git submodule update --init


exit 0
