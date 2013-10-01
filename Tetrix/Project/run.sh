#!/bin/sh
echo -e "\033[1m-------------------------------------------------------------------------------- \033[0m"
echo -e "\033[1m--> Compiling C and C++ Modules: \033[0m"
echo ""

cd "c_src"
if g++ -fPIC -shared -o cbin/imgproc_nif.so imgproc_nif.cpp camera_functions.cpp -lopencv_core -lopencv_imgproc -lopencv_highgui -lueye_api; then

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
echo -e "\033[1m--> Starting Erlang Shell: \033[0m";
echo ""
erl -pa ebin/;
else
echo -e "\033[1mErlang Compilation Failed ! \033[0m";
fi
