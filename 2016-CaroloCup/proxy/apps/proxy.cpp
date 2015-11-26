/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "Proxy.h"
#include <iostream>

int32_t main(int32_t argc, char **argv) {
    int32_t result = 0;
    try{
    automotive::miniature::Proxy p(argc, argv);
    result = p.runModule();
    }
    catch(std::string &exception) {
        std::cerr << "Error while creating serial port: " << exception << std::endl;
    }
    return result;
}
