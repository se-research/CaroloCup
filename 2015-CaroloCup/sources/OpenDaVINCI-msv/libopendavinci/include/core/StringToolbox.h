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

            /**
             * This method compares two strings while ignoring case.
             *
             * @param s1 String 1
             * @param s2 String 2
             * @return true if s1 equals s2
             */
            static bool equalsIgnoreCase(const string &s1, const string &s2) {
                if (s1.size() != s2.size()) {
                    return false;
                }
                for (string::const_iterator c1 = s1.begin(), c2 = s2.begin(); c1 != s1.end(); ++c1, ++c2) {
                    if (tolower(*c1) != tolower(*c2)) {
                        return false;
                    }
                }
                return true;
            };
    };
}

#endif /*OPENDAVINCI_CORE_STRINGTOOLBOX_H_*/
