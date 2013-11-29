#!/bin/sh

    if ps aux | grep "[n]ode1" > /dev/null
        then
            echo 1 
        else
	    echo 0 
    fi
