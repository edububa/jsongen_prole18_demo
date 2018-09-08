#!/bin/bash -x

# installing and compiling de correct erlang version...
DEPS="gcc libssl-dev make automake autoconf libncurses5-dev"

echo -n "Installing kerl dependencies... "
sudo apt install -y $DEPS

echo -n "Installing kerl... "
if [ "$(uname)" == "Darwin" ]; then
    brew install kerl &> /dev/null
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl &> /dev/null
    chmod a+x kerl &> /dev/null
    mkdir ~/bin &> /dev/null
    mv kerl ~/bin &> /dev/null
    export PATH=$PATH:~/bin
    echo "export PATH=$PATH:~/bin" >> .bashrc
fi
if [ "$?" -ne 0 ]; then
    echo "Error" >&2
    exit 1
fi
echo "Done"

echo -n "Building erlang 17.0, please wait... "
kerl build 17.0 17.0 &> /dev/null
if [ "$?" -ne 0 ]; then
    echo "Error" >&2
    exit 1
fi
echo "Done"

echo -n "Installing erlang 17.0, please wait... "
kerl install 17.0 &> ~/kerl/17.0
if [ "$?" -ne 0 ]; then
    echo "Error" >&2
    exit 1
fi
echo "Done"

echo -n "Activating erlang 17.0, please wait... "
. ~/kerl/17.0/activate
if [ "$?" -ne 0 ]; then
    echo "Error" >&2
    exit 1
fi
echo "Done"

echo "\t- To check the active erlang distribution type: kerl active"
echo "\t- To activate erlang 17.0 please type: . ~/kerl/17.0/activate"

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
erl -noshell -eval "eqc_install:install()."
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

# Downloading and installing jsongen
echo -n "Downloading jsongen... "
git clone https://github.com/fredlund/jsongen.git &> /dev/null
if [ "$?" -ne 0 ]; then
    echo "Error" >&2
    exit 1
fi
echo "Done"

echo -n "Building jsongen... "
make compile &> /dev/null
if [ "$?" -ne 0 ]; then
    echo "Error" >&2
    exit 1
fi
echo "Done"
