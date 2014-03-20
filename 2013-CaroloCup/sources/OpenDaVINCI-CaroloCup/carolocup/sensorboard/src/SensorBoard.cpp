/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <iostream>

#include "core/data/Container.h"
#include "core/io/ContainerConference.h"

#include "SensorDetectionData.h"
#include "SensorBoardData.h"

#include "SensorBoard.h"


namespace carolocup {

    using namespace std;
    using namespace core::base;
    using namespace core::data;

	double sensorValues[3];
	int counter = 0;
	bool hole_pre = false, hole_new, parked = false, start = true;
	double time_pre = 0;

    SensorBoard::SensorBoard(const int32_t &argc, char **argv) :
        ConferenceClientModule(argc, argv, "SensorBoard"),
        m_fifo()
		{}

    SensorBoard::~SensorBoard() {}

    void SensorBoard::setUp() {
        // This method will be call automatically _before_ running body().
    }

    void SensorBoard::tearDown() {
        // This method will be call automatically _after_ return from body().
    }

    bool SensorBoard::processContainerExample(Container &c) {
        bool retVal = false;

		if (c.getDataType() == Container::USER_DATA_0) {

			SensorBoardData data = c.getData<SensorBoardData>();
			//cout << "Received Container from user-defined data type " << (uint32_t)c.getUserTypeID() << ", content: " << data.toString() << endl;
			//cout <<"IR1 "<<data.getDistance(1)<<endl;
			//cout <<"IR2 "<<data.getDistance(2)<<endl;
			/*cout <<"IR3 "<<data.getDistance(3)<<endl;
			cout <<"IR4 "<<data.getDistance(4)<<endl;
			cout <<"IR5 "<<data.getDistance(5)<<endl;*/

			double average = 0;
			double sum = 0;
			double zero = 0.001;
			double dist = data.getDistance(1);
			cout <<"IR1 "<<dist<<endl;
			
			if (!((dist < zero) && (dist > (0 - zero)))) {
				SensorDetectionData theSensor;
				theSensor.setNumericalValue(0);
				
				if (start) {
					for (int j = 0; j < 3; j++) {
						sensorValues[j] = dist;
					}
				} else {
					sensorValues[0] = sensorValues[1];
					sensorValues[1] = sensorValues[2];
					sensorValues[2] = dist;
				}
				

				for (int i = 0; i < 3; i++) {
					sum = sum + sensorValues[i];
				}
				average = sum/3;

				if ((average < 0) && !start)  {
					hole_new = true;
					cout << "Hole true" << endl;
				} else if (average > 0) {
					start = false;
					hole_new = false;
					cout << "Hole false" << endl;
				}

				if (!hole_pre && hole_new && !start) {
					TimeStamp t;
					time_pre = t.toMicroseconds() / (1000.0 * 1000.0);
					cout << "Found hole" << endl;
				}
				if (hole_pre && !hole_new) {
					TimeStamp time_cur;
					double time = time_cur.toMicroseconds() / (1000.0 * 1000.0);
					time = time - time_pre;
					cout << "Space: " << time << endl;
					if (time >= 0.55) { // Random
						theSensor.setNumericalValue(1);
						parked = true;
					}
					cout << "Finished hole" << endl;
				}
				hole_pre = hole_new;
				Container con(Container::USER_DATA_2, theSensor);
				getConference().send(con);
			}
		}
	
	return retVal;
	}


    // This method will do the main data processing job.
    ModuleState::MODULE_EXITCODE SensorBoard::body() {
        // Automatically receive _all_ sent data in the FIFO manner.
    	addDataStoreFor(m_fifo);
		
    	while (getModuleState() == ModuleState::RUNNING) {
    		// Process _all_ received entries.
    		while (!m_fifo.isEmpty()) {
    			Container c = m_fifo.leave();

                // Example for processing the received container.
                processContainerExample(c);
    		}
    	}

    	return ModuleState::OKAY;
    }

} // carolocup

