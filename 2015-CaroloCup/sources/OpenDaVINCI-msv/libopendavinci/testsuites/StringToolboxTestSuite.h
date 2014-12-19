/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CORE_STRINGTOOLBOXTESTSUITE_H_
#define CORE_STRINGTOOLBOXTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <string>

#include "core/StringToolbox.h"

using namespace std;

class StringToolboxTest : public CxxTest::TestSuite {
    public:
        void testTrim() {
            string s1 = " ABC";
            core::StringToolbox::trim(s1);
            string ref_s1 = "ABC";
            TS_ASSERT(ref_s1.compare(s1) == 0); 

            string s2 = "ABC ";
            core::StringToolbox::trim(s2);
            string ref_s2 = "ABC";
            TS_ASSERT(ref_s2.compare(s2) == 0); 

            string s3 = " ABC ";
            core::StringToolbox::trim(s3);
            string ref_s3 = "ABC";
            TS_ASSERT(ref_s3.compare(s3) == 0); 

            string s4 = "ABC";
            core::StringToolbox::trim(s4);
            string ref_s4 = "ABC";
            TS_ASSERT(ref_s4.compare(s4) == 0); 

            string s5 = "";
            core::StringToolbox::trim(s5);
            string ref_s5 = "";
            TS_ASSERT(ref_s5.compare(s5) == 0); 
        }

        void testCompareIgnoreCase() {
            string s1 = "ABC";
            string s2 = "ABC";
            TS_ASSERT(core::StringToolbox::equalsIgnoreCase(s1, s2)); 

            string s3 = "abc";
            string s4 = "ABC";
            TS_ASSERT(core::StringToolbox::equalsIgnoreCase(s3, s4)); 
            TS_ASSERT(core::StringToolbox::equalsIgnoreCase(s4, s3)); 

            string s5 = "abc";
            string s6 = "abc";
            TS_ASSERT(core::StringToolbox::equalsIgnoreCase(s5, s6)); 

            string s7 = "abc";
            string s8 = "abd";
            TS_ASSERT(!core::StringToolbox::equalsIgnoreCase(s7, s8)); 

            string s9 = "abc";
            string s10 = "abc ";
            TS_ASSERT(!core::StringToolbox::equalsIgnoreCase(s9, s10)); 
        }
};

#endif /*CORE_STRINGTOOLBOXTESTSUITE_H_*/
