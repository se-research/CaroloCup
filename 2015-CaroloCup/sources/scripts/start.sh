#!/bin/bash

home=/home/odroid/CaroloCup/2014-CaroloCup/Legendary/project/scripts
bin=/opt/msv/bin/
caroloCup=$bin/2013/DIT-168/project-template/
pidfile=${0}.pid
serialPort=/dev/ttyACM0
started=0

# Port setting
stty -F $serialPort raw speed 9600

#ifconfig lo multicast
#route add -net 224.0.0.0 netmask 240.0.0.0 dev lo

#Loop
cd $home
while read -n10 line
do
   echo $line
   if [[ $line == "0" ]]; then
    if [[ $started == 1 ]]; then
	    echo "STOP"
	    # Stop and kill the processes
	    for pid in $(tac $pidfile); do
	      kill -SIGINT $pid
	      sleep 1
	    done
	    started=0
    fi
  elif [[ $line == "1" ]]; then
    if [[ $started == 0 ]]; then
	    echo "START LANEFOLLOWING"
#cp configuration1 configuration
	    killall supercomponent
	    # Start the proceses
	    cd /opt/msv/bin/
	    nohup ./supercomponent --cid=111 &
	    echo "$!" > $pidfile

        nohup ${caroloCup}/proxy --cid=111 --freq=20 &
        echo "$!" >> $pidfile

        nohup ${caroloCup}/lanedetector --cid=111 --freq=20 &
	    echo "$!" >> $pidfile

#Note that this is only lanedriver!
	    nohup ${caroloCup}/lanedriver --cid=111 --freq=40 &
	    echo "$!" >> $pidfile
	    started=1
    fi

    # Change back to 2 for parking
  elif [[ $line == "20" ]]; then
    if [[ $started == 0 ]]; then
	    echo "START PARKING"
	    cp configuration2 configuration
	    killall supercomponent
	    # Start the proceses
	    nohup ${bin}/supercomponent --cid=111 --freq=20 &
	    echo "$!" > $pidfile

	    nohup ${caroloCup}/Sensors --cid=111 --freq=40 &
	    echo "$!" >> $pidfile

	    nohup ${caroloCup}/2013-CaroloCup-driver --cid=111 --freq=40 &
	    echo "$!" >> $pidfile
	    started=1
    fi
   fi
done < $serialPort

