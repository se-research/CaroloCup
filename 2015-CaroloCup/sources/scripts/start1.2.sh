#!/bin/bash

home=/home/odroid/CaroloCup/2014-CaroloCup/Legendary/project/scripts
bin=/opt/msv/bin/
caroloCup=$bin/2013/DIT-168/project-template/
pidfile=${0}.pid
serialPort=/dev/ttyACM1
started=0

# Port setting
stty -F $serialPort raw speed 9600

#ifconfig lo multicast
#route add -net 224.0.0.0 netmask 240.0.0.0 dev lo

#Loop
cd $home
while read -n2 line #OPTIONS: -n 10 (Legendary car), -n 1 (Wooden car)
do

   echo $line
   echo "test"
   if [[ $line == "00" ]]; then #STOP button
    if [[ $started == 1 || $started=2 ]]; then
	    echo "STOP"
	    # Stop and kill the processes from the pidfile
	   # for pid in $(tac $pidfile); do
	    #  kill -SIGINT $pid
	    #  sleep 1
	  #  done

	   #Kill all processes
	   killall supercomponent
           echo "Supercomponent is stopped"

	   killall proxy
	   echo "Proxy is stopped"

	   killall driver
 	   echo "Driver is stopped"

	   killall lanedriver
	   echo "LaneDriver is stopped"

	   killall lanedetector
	   echo "LaneDetetor is stopped"
	    started=0
    fi
  elif [[ $line == "11" ]]; then #LaneFollowing
    if [[ $started == 0 ]]; then
	    echo "START LANEFOLLOWING"
#cp configuration1 configuration
	    #killall supercomponent
	    # Start the processes
	    
        nohup ${caroloCup}/lanedetector --cid=222 --freq=20 &
	    echo "$!" >> $pidfile

#Note that this is only lanedriver!
	    nohup ${caroloCup}/lanedriver --cid=222 --freq=40 &
	    echo "$!" >> $pidfile
	    started=1
    fi

    # Change back to 2 for parking
  elif [[ $line == "44" ]]; then #READY! button
    if [[ $started == 0 ]]; then
	   # echo "START PARKING"
	    # cp configuration2 configuration
	    #killall supercomponent
	    # Start the processes
	echo "GETTING READY"
	killall supercomponent

	    cd /opt/msv/bin/
	    echo "Starting"
	    nohup ./supercomponent --cid=222 &
	    echo "$!" >> $pidfile
	    echo "Supercomponent has started"

	    cd /opt/msv/bin/2013/DIT-168/project-template/ 
	    nohup ./proxy --cid=222 --freq=60 &
	    echo "$!" >> $pidfile
	    echo "Proxy has started"
	    sleep 10 # Waits n seconds before start Driver component 
	    echo "Driver is ready"
	    started=2

	fi
   elif [[ $line == "22" ]]; then #PARKING button
	if [[ $started == 2 ]]; then
	    
	    cd /opt/msv/bin/2013/DIT-168/project-template/ 
	    nohup ./driver --cid=222 --freq=40 &
	    echo "$!" >> $pidfile
	    echo "Driver has started"
	    started=1

fi
   fi
done < $serialPort

