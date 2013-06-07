/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DATA_PLAYER_PLAYERCOMMAND_H_
#define HESPERIA_DATA_PLAYER_PLAYERCOMMAND_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/SerializableData.h"

namespace hesperia {
    namespace data {
        namespace player {

            using namespace std;

            /**
             * This class can be used to remotely control the player.
             */
            class HESPERIA_API PlayerCommand : public core::data::SerializableData {
                public:
                    enum COMMAND {
                        PLAY,
                        PAUSE,
                        REWIND,
                        STEP_FORWARD
                    };

                public:
                    PlayerCommand();

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    PlayerCommand(const PlayerCommand &obj);

                    virtual ~PlayerCommand();

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    PlayerCommand& operator=(const PlayerCommand &obj);

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
} // hesperia::data::player

#endif /*HESPERIA_DATA_PLAYER_PLAYERCOMMAND_H_*/
