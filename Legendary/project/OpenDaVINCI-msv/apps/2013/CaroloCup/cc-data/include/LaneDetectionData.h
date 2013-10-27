/*
* Mini-Smart-Vehicles.
*
* This software is open source. Please see COPYING and AUTHORS for further information.
*/

#ifndef LANEDETECTIONDATA_H_
#define LANEDETECTIONDATA_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/data/SerializableData.h"

#include <opencv/cv.h>

namespace carolocup {

	using namespace std;
	using namespace cv;

  class Lines {
    public:
      Lines (Vec4i l, Vec4i d, Vec4i r) :
        leftLine(l) ,
        rightLine(r) ,
        dashedLine(d) {}
      virtual ~Lines () {}

      Vec4i leftLine;
      Vec4i rightLine;
      Vec4i dashedLine;
      int pGain;
      int intGain;
      int derGain;
      int speed;
  };


  /**
   * This is an example how you can send data from one component to another.
   */
  class LaneDetectionData : public core::data::SerializableData {
    public:
      virtual ~LaneDetectionData();

      LaneDetectionData();

      /**
       * Copy constructor.
       *
       * @param obj Reference to an object of this class.
       */
      LaneDetectionData(const LaneDetectionData &obj);

      /**
       * Assignment operator.
       *
       * @param obj Reference to an object of this class.
       * @return Reference to this instance.
       */
      LaneDetectionData& operator=(const LaneDetectionData &obj);

      /**
       * This method returns the example data.
       *
       * @return example data.
       */
      Lines getLaneDetectionData() const;

      /**
       * This method sets the example data.
       *
       * @param e Example data.
       */
      void setLaneDetectionData(const Lines &e);

      virtual ostream& operator<<(ostream &out) const;
      virtual istream& operator>>(istream &in);
      virtual const string toString() const;
    private:
      Lines m_lines;
  };

} // msv

#endif /*LANEDETECTIONDATA_H_*/
