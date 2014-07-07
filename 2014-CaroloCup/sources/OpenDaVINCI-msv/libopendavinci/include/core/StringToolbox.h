/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_STRINGTOOLBOX_H_
#define OPENDAVINCI_CORE_STRINGTOOLBOX_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {

	using namespace std;

    /**
     * This class can be used to perform some useful operations on strings (i.e. remove leading
     * and trailing blanks for example).
     */
    class StringToolbox {
        public:
            virtual ~StringToolbox() {};

            /**
             * This method removes leading and trailing blanks.
             *
             * @param s String to be trimmed.
             */
            static void trim(string &s) {
	            string::size_type pos = s.find_last_not_of(' ');
	            if(pos != string::npos) {
		            s.erase(pos + 1);

		            pos = s.find_first_not_of(' ');
		            if(pos != string::npos) {
			            s.erase(0, pos);
		            }
	            } else {
		            s.erase(s.begin(), s.end());
	            }
            };
    };
}

#endif /*OPENDAVINCI_CORE_STRINGTOOLBOX_H_*/
