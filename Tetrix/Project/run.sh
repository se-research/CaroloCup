#!/bin/sh

if [ $# -eq 0 ]; then
    echo -e "\033[1m--> Starting Erlang Shell: \033[0m";
    echo -e "\033[1m-------------------------------------------------------------------------------- \033[0m"
    echo ""
    if ps aux | grep "[r]un_python" > /dev/null
        then
            echo "Tetrix monitor is already running"
        else
	    ./Monitor/run_python &
	    echo "Activated Tetrix Monitor"
    fi
    sudo erl -pa ebin/ -sname node1 -setcookie nodes
else
    if [ $1 = clean ]; then
        ./Monitor/run_python clean
	cd "c_source"
	if make clean; then
	    echo -e "\033[1mCleaning Binaries Complete ! \033[0m";
	fi
	cd ".."
	echo -e "\033[1m-------------------------------------------------------------------------------- \033[0m"
    elif [ $1 = compile_c ]; then
	echo -e "\033[1m-------------------------------------------------------------------------------- \033[0m"
	echo -e "\033[1m--> Compiling C and C++ Modules: \033[0m"
	echo ""
	
	cd "c_source"
	if make; then
	    
	    echo -e "\033[1mG++ Compilation Complete ! \033[0m";
	else
	    echo -e "\033[1mG++ Compilation Failed ! \033[0m";
	fi
	echo -e "\033[1m-------------------------------------------------------------------------------- \033[0m"
	cd ".."
    elif [ $1 = compile_erlang ]; then
	echo -e "\033[1m-------------------------------------------------------------------------------- \033[0m"
	echo -e "\033[1m--> Compiling Erlang Modules: \033[0m"

	echo ""
        ./Monitor/run_python compile
	if rebar compile; then

	    echo -e "\033[1mErlang Compilation Complete ! \033[0m";
	    echo -e "\033[1m-------------------------------------------------------------------------------- \033[0m";
	else
	    echo -e "\033[1mErlang Compilation Failed ! \033[0m";
	fi
	echo -e "\033[1m-------------------------------------------------------------------------------- \033[0m";
    elif [ $1 = compile_all ]; then
	echo -e "\033[1m-------------------------------------------------------------------------------- \033[0m"
	echo -e "\033[1m--> Compiling C and C++ Modules: \033[0m"
	echo ""

        ./Monitor/run_python compile
	cd "c_source"
	if make; then

	    echo -e "\033[1mG++ Compilation Complete ! \033[0m";
	else
	    echo -e "\033[1mG++ Compilation Failed ! \033[0m";
	fi

	echo -e "\033[1m-------------------------------------------------------------------------------- \033[0m"
	echo -e "\033[1m--> Compiling Erlang Modules: \033[0m"

	echo ""

	cd ".."
	if rebar compile; then

	    echo -e "\033[1mErlang Compilation Complete ! \033[0m";
	    echo -e "\033[1m-------------------------------------------------------------------------------- \033[0m";
	else
	    echo -e "\033[1mErlang Compilation Failed ! \033[0m";
	    echo -e "\033[1m-------------------------------------------------------------------------------- \033[0m";
	fi
    fi
fi


