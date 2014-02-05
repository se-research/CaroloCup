#!/bin/bash

bin=/opt/Legendary/bin/
caroloCup=$bin/2013/CaroloCup/
pidfile=${0}.pid
serialPort=/dev/ttyACM0
state="idle"

while read -r line < $serialPort; do
  if grep "RED" <<< "$line" > /dev/null && [[ $state == "started" ]]; then

    # Stop and kill the processes
    for pid in $(cat $pidfile); do
      kill $pid
    done

    echo "Switching to Idle state"
    state="idle"

  elif grep "YELLOW" <<< "$line" > /dev/null && [[ $state == "idle" ]]; then

    # Start the proceses
    ${bin}/supercomponent --cid=111 --freq=20
    echo "$!" > $pidfile

    ${caroloCup}/2013-CaroloCup-lanedetector --cid=111 --freq=30
    echo "$!" >> $pidfile

    ${caroloCup}/2013-CaroloCup-driver --cid=111 --freq=40
    echo "$!" >> $pidfile

    echo "Switching to Started state"
    state="started"

  else
    echo "Bad input from serial port: '$line'"
    echo "Current State: '$state'"
  fi
done
