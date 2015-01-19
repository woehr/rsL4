#!/bin/sh
### Note: This script is to be invoked through the top level build script
### Arguments
#### Location of seL4 build files
buildFiles=`pwd`/$1
#### Location of Google's repo tool
repo=`pwd`/$2
#### Root of the virtualenv with tempita installed
venv=`pwd`/$3
#### Place to build seL4
buildDir=`pwd`/$4

# Activate the virtualenv so we can build
source $venv/bin/activate
pip install --upgrade distribute tempita

# Change to working directory
mkdir -p $buildDir
cd $buildDir

# Init repo and get source
python $repo init -u http://github.com/jmwoehr/rsL4-manifest.git
python $repo sync

# Copy in our build files
cp -f $buildFiles/Makefile $buildDir/Makefile
cp -f $buildFiles/Kbuild $buildDir/Kbuild
cp -f $buildFiles/Kconfig $buildDir/Kconfig
cp -Rf $buildFiles/configs $buildDir/configs

# Apply patches
pushd $buildDir/tools/elfloader/src/arch-arm/
patch < $buildFiles/elfloader-mmu-workaround.patch
popd

# Build
make bbb_defconfig
make

# Deactivate the virtualenv
deactivate

