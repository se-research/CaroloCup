/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_TOOLS_SPLITTER_SPLITTER_H_
#define OPENDAVINCI_TOOLS_SPLITTER_SPLITTER_H_

#include <string>

namespace tools {
    namespace splitter {

        using namespace std;

        /**
         * This class can be used to split a given file.
         */
        class Splitter {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 *
                 * @param obj Reference to an object of this class.
                 */
                Splitter(const Splitter &/*obj*/);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 *
                 * @param obj Reference to an object of this class.
                 * @return Reference to this instance.
                 */
                Splitter& operator=(const Splitter &/*obj*/);

            public:
                Splitter();

                virtual ~Splitter();

                /**
                 * This method processes the given source file and splits it between
                 * and including start and end.
                 *
                 * @param source Recording file to be split.
                 * @param memorySegmentSize Size of one memory segment to be used in recorder and player.
                 * @param start Start container to be split.
                 * @param end End container (including) in the splitting.
                 */
                void process(const string &source, const uint32_t &memorySegmentSize, const uint32_t &start, const uint32_t &end);
        };

    } // splitter
} // tools

#endif /*OPENDAVINCI_TOOLS_SPLITTER_SPLITTER_H_*/
