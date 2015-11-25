# Getting Started on Ubuntu

First, add the OpenDaVINCI repository key:

    $ wget -O - -q http://opendavinci.cse.chalmers.se/opendavinci.cse.chalmers.se.gpg.key | sudo apt-key add -

Next, add the OpenDaVINCI repository:

    $ sudo sh -c 'echo "deb http://opendavinci.cse.chalmers.se/ubuntu/ trusty main" >> /etc/apt/sources.list'

Update the package list:

    $ sudo apt-get update

For the Odroid system, install the packages as follows:

    $ sudo apt-get install opendavinci-odlib opendavinci-odtools opendavinci-odsupercomponent opendavinci-oddatastructuregenerator libautomotivedata

For the x86-64 developer system, install the packages as follows:

    $ sudo apt-get install opendavinci-odlib opendavinci-odtools opendavinci-odsupercomponent opendavinci-oddatastructuregenerator opendavinci-odcockpit odsimulation-odsimlib odsimulation-odsimtools libautomotivedata
