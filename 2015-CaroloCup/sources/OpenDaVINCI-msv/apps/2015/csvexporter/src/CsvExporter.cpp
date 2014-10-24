/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */
#include <iostream>
#include "core/data/Container.h"
#include "core/io/ContainerConference.h"
#include "core/io/URL.h"
#include "core/io/StreamFactory.h"
#include "LaneDetectionData.h"
#include "CsvExporter.h"

namespace msv{
	using namespace core::base;
	using namespace core::data;
	using std::cout;
	using namespace core::io;

	CsvExporter::CsvExporter(const int32_t &argc, char **argv):
		ConferenceClientModule(argc, argv, "csveporter"),
		frame_count(0),
		m_debug(false),
		m_fifo(),
		m_out(NULL){

		stringstream recordingURL;
		recordingURL << "file://" << "lane_csv_" << TimeStamp().getYYYYMMDD_HHMMSS() << ".csv";
		URL _url(recordingURL.str());
		m_out = &(StreamFactory::getInstance().getOutputStream(_url));

	}

	CsvExporter::~CsvExporter(){
		cout << "Clearing queue... ";
		if (m_out != NULL) {
		    m_out->flush();
		}
		cout << "done." << endl;
	}

	void CsvExporter::setUp(){

	}

	ModuleState::MODULE_EXITCODE CsvExporter::body(){

		addDataStoreFor(m_fifo);
		LaneDetectionData laneData;
		Lines lines;
		cout<< "waiting for data....."<< endl;
		while (getModuleState() == ModuleState::RUNNING) {
			while (!m_fifo.isEmpty()) {
			    Container c = m_fifo.leave();
			    if(c.getDataType()==Container::USER_DATA_1){

			    	laneData = c.getData<LaneDetectionData> ();
			    	lines=laneData.getLaneDetectionData();
			    	cout<< " I received something yeah!! \n";

			    	if (m_out != NULL) {

			    		/*------------------------------------DATA FORMAT(per line)----------------------------------------------------------------------
			    		 * -------------------------------------------------------------------------------------------------------------------
			    		 * frame-counter ; leftLine.x ; leftLine.y ; leftLine.z ; leftLine.w ; rightLine.x ; rightLine.y ; rightLine.z ; rightLine.w ; dashedLine.x ; dashedLine.y ; dashedLine.z ; dashedLine.w ; goalLine.p1 ; goalLine.p2 ; goalLine.slope ; currentLine.p1 ; currentLine.p2 ; currentLine.slope ; pGain ; intGain ; derGain ; speed ; width ; height ; startLineHeight ; stopLineHeight
			    		 */
			    		//frame-counter
			    		(*m_out) << laneData.getFrameCount()<<";";
			    		//leftLine.x ; leftLine.y ; leftLine.z ; leftLine.w
			    		(*m_out) << lines.leftLine[0]<<";" << lines.leftLine[1]<<";"<< lines.leftLine[2]<<";"<< lines.leftLine[3]<<";";
			    		//rightLine.x ; rightLine.y ; rightLine.z ; rightLine.w
			    		(*m_out) << lines.rightLine[0]<<";" << lines.rightLine[1]<<";"<< lines.rightLine[2]<<";"<< lines.rightLine[3]<<";";
			    		//dashedLine.x ; dashedLine.y ; dashedLine.z ; dashedLine.w
			    		(*m_out) << lines.dashedLine[0]<<";" << lines.dashedLine[1]<<";"<< lines.dashedLine[2]<<";"<< lines.dashedLine[3]<<";";
			    		// goalLine.p1 ; goalLine.p2 ; goalLine.slope
			    		(*m_out) << lines.goalLine.p1<<";" << lines.goalLine.p2<<";"<< lines.goalLine.slope<<";";
			    		//currentLine.p1 ; currentLine.p2 ; currentLine.slope
			    		(*m_out) << lines.currentLine.p1<<";" << lines.currentLine.p2<<";"<< lines.currentLine.slope<<";";

			    		(*m_out) << lines.pGain<<";" << lines.intGain<<";" << lines.derGain <<";" <<lines.speed <<";" << lines.width<<";" << lines.height <<";";

			    		(*m_out) << lines.startLineHeight<<";" << lines.stopLineHeight;

			    		//go to new line
			    		(*m_out) << "\n";
			    	}

			    }
			}


		}

		if (m_out != NULL) {
		    m_out->flush();
		}

		return ModuleState::OKAY;
	}

	void CsvExporter::tearDown(){

	}
}
