#!/bin/bash

python _buildtools/scons.py $1 valgrind=false tests=no install-all install_dir=`pwd`/binaries

echo "To run Hesperia, change to binary installation directory (binaries/bin) and start supercomponent --cid=111 and any binary like monitor --cid=111 for example."
