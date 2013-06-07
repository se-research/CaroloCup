/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_RECORDER_RECORDERCOMMAND_H_
#define HESPERIA_DATA_RECORDER_RECORDERCOMMAND_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"

namespace hesperia {
    namespace data {
        namespace recorder {

            using namespace std;

            /**
             * This class can be used to remotely control the recorder.
             */
            class HESPERIA_API RecorderCommand : public core::data::SerializableData {
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
} // hesperia::data::recorder

#endif /*HESPERIA_DATA_RECORDER_RECORDERCOMMAND_H_*/
