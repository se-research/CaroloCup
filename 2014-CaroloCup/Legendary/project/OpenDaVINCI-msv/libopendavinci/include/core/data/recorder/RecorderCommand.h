/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_DATA_RECORDER_RECORDERCOMMAND_H_
#define OPENDAVINCI_DATA_RECORDER_RECORDERCOMMAND_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/data/SerializableData.h"

namespace core {
    namespace data {
        namespace recorder {

            using namespace std;

            /**
             * This class can be used to remotely control the recorder.
             */
            class OPENDAVINCI_API RecorderCommand : public core::data::SerializableData {
                public:
                    enum COMMAND {
                        RECORD,
                        STOP
                    };

                public:
                    RecorderCommand();

                    virtual ~RecorderCommand();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    RecorderCommand(const RecorderCommand &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    RecorderCommand& operator=(const RecorderCommand &obj);

                    /**
                     * This method returns the command.
                     *
                     * @return Command.
                     */
                    COMMAND getCommand() const;

                    /**
                     * This method sets the command.
                     *
                     * @param c Command to be set.
                     */
                    void setCommand(const COMMAND &c);

                    virtual ostream& operator<<(ostream &out) const;
                    virtual istream& operator>>(istream &in);

                    virtual const string toString() const;

                private:
                    COMMAND m_command;
            };

        }
    }
} // core::data::recorder

#endif /*OPENDAVINCI_DATA_RECORDER_RECORDERCOMMAND_H_*/
