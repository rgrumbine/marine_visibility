#!/bin/bash

module list
if [ $? -ne 0 ] ; then
  echo On a system without the module software
  export BASE=${BASE:-/u/Robert.Grumbine/save/}
  export MMAB_VER=v3.4.3
else
#on a system with module software, such as wcoss
  module purge
  module load ./fog.modulefile
  module list
fi

export FC=ftn

for d in omb_gblvsby.fd
do
  cd $d
  make
  cd ..
done

mv omb_gblvsby.fd/omb_gblvsby ../exec
