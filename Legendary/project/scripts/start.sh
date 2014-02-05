#!/bin/bash

bin=/opt/Legendary/bin/
caroloCup=$bin/2013/CaroloCup/
pidfile=${0}.pid
serialPort=/dev/ttyS2

while read -r line < $serialPort; do
  if [[ $line == "RED" ]]; then

    # Stop and kill the processes
    for pid in $(cat $pidfile); do
      kill -9 $pid
    done

  elif [[ $line == "YELLOW" ]]; then

    # Start the proceses
    ${bin}/supercomponent --cid=111 --freq=20
    echo "$!" > $pidfile

    ${caroloCup}/2013-CaroloCup-lanedetector --cid=111 --freq=30
    echo "$!" >> $pidfile

    ${caroloCup}/2013-CaroloCup-driver --cid=111 --freq=40
    echo "$!" >> $pidfile

  else
    print "Bad input from serial port:'$lien'"
  fi
done
