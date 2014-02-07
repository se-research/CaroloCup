#!/bin/bash

bin=/opt/Legendary/bin/
caroloCup=$bin/2013/CaroloCup/
pidfile=${0}.pid
serialPort=/dev/ttyACM0
started=0

# Port setting
stty -F $serialPort raw speed 115200

#Loop
while read -n10 line
do
   echo $line
   if [[ $line == "0" ]]; then
    if [[ $started == 1 ]]; then
	    echo "STOP"
	    # Stop and kill the processes
	    for pid in $(tac $pidfile); do
	      kill -SIGINT $pid
	    done
	    started=0
    fi
  elif [[ $line == "1" ]]; then
    if [[ $started == 0 ]]; then
	    echo "START LANEFOLLOWING"
	    cp configuration1 configuration
	    # Start the proceses
	    nohup ${bin}/supercomponent --cid=111 --freq=20 &
	    echo "$!" > $pidfile

	    nohup ${caroloCup}/2013-CaroloCup-lanedetector --cid=111 --freq=30 &
	    echo "$!" >> $pidfile

	    nohup ${caroloCup}/2013-CaroloCup-driver --cid=111 --freq=40 &
	    echo "$!" >> $pidfile
	    started=1
    fi
  elif [[ $line == "2" ]]; then
    if [[ $started == 0 ]]; then
	    echo "START PARKING"
	    cp configuration2 configuration
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
