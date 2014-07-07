#!/bin/sh

if [ $# -eq 0 ]; then
    echo -e "\033[1m--> Starting Erlang Shell: \033[0m";
    echo -e "\033[1m-------------------------------------------------------------------------------- \033[0m"
    echo ""
    sudo erl -pa ebin/ -sname node1 -setcookie nodes;
else
    if [ $1 = clean ]; then
	cd "ebin"
	rm *.beam
	echo -e "\033[1m-------------------------------------------------------------------------------- \033[0m"
	echo -e "\033[1m--> Cleaned Monitor App Beam files:-------"
    elif [ $1 = compile_erlang ]; then
	echo -e "\033[1m-------------------------------------------------------------------------------- \033[0m"
	echo -e "\033[1m--> Compiling Monitor App Erlang Modules: \033[0m"

	echo ""

	if rebar compile; then

	    echo -e "\033[1mErlang Compilation of Monitor App Complete ! \033[0m";
	    echo -e "\033[1m-------------------------------------------------------------------------------- \033[0m";
	else
	    echo -e "\033[1mErlang Compilation Failed ! \033[0m";
	fi
	echo -e "\033[1m-------------------------------------------------------------------------------- \033[0m";
    elif [ $1 = compile_all ]; then
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


