#!/bin/bash

# installing and compiling de correct erlang version...
DEPS="gcc libssl-dev make automake autoconf libncurses5-dev unzip"
ERL_V="17.0"

echo -n "Installing kerl... "
if [ "$(uname)" == "Darwin" ]; then
    brew install kerl &> /dev/null
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    echo -n "Installing kerl dependencies... "
    sudo apt install -y $DEPS
    curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl &> /dev/null
    chmod a+x kerl &> /dev/null
    mkdir ~/bin &> /dev/null
    mv kerl ~/bin &> /dev/null
    export PATH=$PATH:~/bin
    # echo "export PATH=$PATH:~/bin" >> .bashrc
fi
if [ "$?" -ne 0 ]; then
    echo "Error" >&2
    exit 1
fi
echo "Done"

echo -n "Building erlang $ERL_V, please wait... "
kerl build $ERL_V $ERL_V &> /dev/null
echo "Done"

echo -n "Installing erlang $ERL_V, please wait... "
kerl install $ERL_V ~/kerl/$ERL_V &> /dev/null
echo "Done"

echo -n "Activating erlang $ERL_V, please wait... "
. ~/kerl/$ERL_V/activate
if [ "$?" -ne 0 ]; then
    echo "Error" >&2
    exit 1
fi
echo "Done"

echo " - To check the active erlang distribution type: kerl active"
echo " - To activate erlang $ERL_V please type: . ~/kerl/$ERL_V/activate"

# installing rebar3 and building it with R17...
git clone https://github.com/erlang/rebar3.git &> /dev/null
cd rebar3
./bootstrap
./rebar3 local install
export PATH=/Users/luis/.cache/rebar3/bin:$PATH
cd ..

# installing QC
echo -n "Downloading Quviq's QuickCheck R17... "
wget http://quviq-licencer.com/downloads/eqcR17.zip &> /dev/null
if [ "$?" -ne 0 ]; then
    echo "Error" >&2
    exit 1
fi
echo "Done"

echo -n "Extracting QuickCheck R17... "
unzip eqcR17.zip &> /dev/null
if [ "$?" -ne 0 ]; then
    echo "Error" >&2
    exit 1
fi
echo "Done"

echo -n "Please type your registration key for QuickCheck R17: "
read key
echo ""
echo "Your registration key is $key."

echo "Installing QuickCheck R17... "
cd *Quviq* &> /dev/null
mkdir ~/.QC
erl -noshell -eval "eqc_install:install(\"~/.QC\")."
if [ "$?" -ne 0 ]; then
    echo "Error" >&2
    exit 1
fi

echo "Registering your QuickCheck key... "
erl -noshell -eval "eqc:registration(\"$key\")."
if [ "$?" -ne 0 ]; then
    echo "Error" >&2
    exit 1
fi

cd ..

# Downloading and installing jsongen
echo -n "Downloading jsongen... "
git clone https://github.com/fredlund/jsongen.git &> /dev/null
if [ "$?" -ne 0 ]; then
    echo "Error" >&2
    exit 1
fi
echo "Done"

echo "Building jsongen... "
cd jsongen
make compile
if [ "$?" -ne 0 ]; then
    echo "Error" >&2
    exit 1
fi
cd ..
