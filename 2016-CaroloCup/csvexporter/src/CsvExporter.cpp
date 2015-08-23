/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */
#include <iostream>
#include "core/data/Container.h"
#include "core/io/conference/ContainerConference.h"
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
		TimeTriggeredConferenceClientModule(argc, argv, "csveporter"),
		frame_count(0),
		m_debug(false),
		m_fifo(),
		m_out(NULL){

		stringstream recordingURL;
		recordingURL << "file://" << "lane_csv_" << TimeStamp().getYYYYMMDD_HHMMSS() << ".csv";
		URL _url(recordingURL.str());
		m_out = StreamFactory::getInstance().getOutputStream(_url);

	}

	CsvExporter::~CsvExporter(){
		cout << "Clearing queue... ";
		if (m_out.isValid()) {
		    m_out->flush();
		}
		cout << "done." << endl;
	}

	void CsvExporter::setUp(){

	}

	coredata::dmcp::ModuleExitCodeMessage::ModuleExitCode CsvExporter::body(){

		addDataStoreFor(m_fifo);
		LaneDetectionData laneData;
		Lines lines;
		cout<< "waiting for data....."<< endl;
		while (getModuleStateAndWaitForRemainingTimeInTimeslice() == coredata::dmcp::ModuleStateMessage::RUNNING) {
			while (!m_fifo.isEmpty()) {
			    Container c = m_fifo.leave();
			    if(c.getDataType()==Container::USER_DATA_1){

			    	laneData = c.getData<LaneDetectionData> ();
			    	lines=laneData.getLaneDetectionData();


			    	if (m_out.isValid()) {
			    		cout<< " writing data for frame: " <<laneData.getFrameCount() << "\n";

			    		/*------------------------------------DATA FORMAT(per line)----------------------------------------------------------------------
			    		 * -------------------------------------------------------------------------------------------------------------------
			    		 * frame-counter ; leftLine.x ; leftLine.y ; leftLine.z ; leftLine.w ; rightLine.x ; rightLine.y ; rightLine.z ; rightLine.w ; dashedLine.x ; dashedLine.y ; dashedLine.z ; dashedLine.w ; goalLine.p1.x ; goalLine.p1.y ; goalLine.p2.x ; goalLine.p2.y ; goalLine.slope ; currentLine.p1.x ; currentLine.p1.y ; currentLine.p2.x ; currentLine.p2.y ; currentLine.slope ; pGain ; intGain ; derGain ; speed ; width ; height ; startLineHeight ; stopLineHeight;classification
			    		 */
			    		//frame-counter
			    		(*m_out) << laneData.getFrameCount()<<";";

			    		//leftLine.x ; leftLine.y ; leftLine.z ; leftLine.w
			    		(*m_out) << lines.leftLine[0]<<";" << lines.leftLine[1]<<";"<< lines.leftLine[2]<<";"<< lines.leftLine[3]<<";";

			    		//rightLine.x ; rightLine.y ; rightLine.z ; rightLine.w
			    		(*m_out) << lines.rightLine[0]<<";" << lines.rightLine[1]<<";"<< lines.rightLine[2]<<";"<< lines.rightLine[3]<<";";

			    		//dashedLine.x ; dashedLine.y ; dashedLine.z ; dashedLine.w
			    		(*m_out) << lines.dashedLine[0]<<";" << lines.dashedLine[1]<<";"<< lines.dashedLine[2]<<";"<< lines.dashedLine[3]<<";";

			    		// goalLine.p1.x ; goalLine.p1.y ; goalLine.p2.x ; goalLine.p2.y ; goalLine.slope
			    		(*m_out) << lines.goalLine.p1.x<<";"<<lines.goalLine.p1.y<<";" << lines.goalLine.p2.x<<";"<< lines.goalLine.p2.y<<";"<< lines.goalLine.slope<<";";

			    		//currentLine.p1.x ; currentLine.p1.y ; currentLine.p2.x ; currentLine.p2.y ; currentLine.slope
			    		(*m_out) << lines.currentLine.p1.x<<";"<<lines.currentLine.p1.y<<";" << lines.currentLine.p2.x<<";"<< lines.currentLine.p2.y<<";"<< lines.currentLine.slope<<";";

			    		// pGain ; intGain ; derGain ; speed ; width ; height ; startLineHeight ; stopLineHeight
			    		(*m_out) << lines.pGain<<";" << lines.intGain<<";" << lines.derGain <<";" <<lines.speed <<";" << lines.width<<";" << lines.height <<";";

			    		// startLineHeight ; stopLineHeight
			    		(*m_out) << lines.startLineHeight<<";" << lines.stopLineHeight<<";";

			    		//classification
			    		(*m_out) << laneData.getClassification();

			    		//go to new line
			    		(*m_out) << "\n";
			    	}

			    }
			}


		}

		if (m_out.isValid()) {
		    m_out->flush();
		}

		return coredata::dmcp::ModuleExitCodeMessage::OKAY;
	}

	void CsvExporter::tearDown(){

	}
}
