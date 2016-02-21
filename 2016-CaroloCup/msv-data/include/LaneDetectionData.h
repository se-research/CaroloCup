/*
* Mini-Smart-Vehicles.
*
* This software is open source. Please see COPYING and AUTHORS for further information.
*/

#ifndef LANEDETECTIONDATA_H_
#define LANEDETECTIONDATA_H_

// core/opendavinci.h must be included to setup platform-dependent header files and configurations.
#include "opendavinci/odcore/opendavinci.h"

#include "opendavinci/odcore/data/SerializableData.h"

//#include <opencv/cv.h>
#include "opencv2/opencv.hpp"

namespace msv
{

using namespace std;
using namespace cv;


enum RoadState{
  NOT_SET,
  NORMAL,
  INTERSECTION
};

class  CustomLine
{
public:
    CustomLine () :
        center(),
        p1(),
        p2(),
        slope(0),
        polygonIndex(-1) // This is a mapping to the polygon that the CustomLine was derived from.
    {}
    virtual ~CustomLine () {}

    void reset() {
        center = Point(0, 0),
        p1 = Point(0, 0),
        p2 = Point(0, 0),
        slope = 0;
        polygonIndex = -1;
    }

    bool operator < (const CustomLine &other ) const
    {
        return max(p1.y, p2.y) > max(other.p1.y, other.p2.y);
        //return slope < other.slope;
    }

    bool operator == (const CustomLine &other ) const
    {
        if ((p1.y == other.p1.y) && (p1.x == other.p1.x))
            {
                return true;
            }
        return false;
    }

    Point center, p1, p2;
    float slope;
    int polygonIndex;
};

class Lines
{
public:
    Lines () :
        leftLine(Vec4i(0, 0, 0, 0)) ,
        rightLine(Vec4i(0, 0, 0, 0)) ,
        dashedLine(Vec4i(0, 0, 0, 0)),
        goalLine(),
        goalLineLeft(),
        currentLine(),
        pGain(0),
        intGain(0),
        derGain(0),
        speed(0),
        width(0),
        height(0),
        startLineHeight(0),
        stopLineHeight(0)
	{}
    Lines (Vec4i l, Vec4i d, Vec4i r) :
        leftLine(l) ,
        rightLine(r) ,
        dashedLine(d),
        goalLine(),
        goalLineLeft(),
        currentLine(),
        pGain(0),
        intGain(0),
        derGain(0),
        speed(0),
        width(0),
        height(0),
        startLineHeight(0),
        stopLineHeight(0)
	{}
    virtual ~Lines () {}
    void setGoalLine(const CustomLine &goal)
    {
        goalLine = goal;
    }
    void setGoalLineLeft(const CustomLine &goal)
    {
        goalLineLeft = goal;
    }
    void setCurrentLine(const CustomLine &curr)
    {
        currentLine = curr;
    }



    Vec4i leftLine;
    Vec4i rightLine;
    Vec4i dashedLine;
    CustomLine goalLine;
    CustomLine goalLineLeft;
    CustomLine currentLine;
    int pGain;
    int intGain;
    int derGain;
    int speed;
    int width;
    int height;
    int startLineHeight;
    int stopLineHeight;


};

class LaneDetectorDataToDriver
{
public:
    LaneDetectorDataToDriver () :
        switchPointsLeftGoalLines0(),
        switchPointsRightGoalLines0(),
	switchPointsLeftGoalLines1(),
        switchPointsRightGoalLines1(),
        leftGoalLines0(),
        rightGoalLines0(),
	leftGoalLines1(),
        rightGoalLines1(),
	leftGoalLines2(),
        rightGoalLines2(),
	leftGoalLines3(),
        rightGoalLines3(),
        currentLine(),
		roadState(NORMAL),
		confidenceLevel(0),
    confidenceLevel_goalLine(0),
		noTrajectory(true)
    {}
    LaneDetectorDataToDriver (vector<int> spl, vector<int> spr, vector<CustomLine> lgl, vector<CustomLine> rgl, CustomLine c, bool nothing, int clgl):
        switchPointsLeftGoalLines0(spl[0]),
  switchPointsRightGoalLines0(spr[0]),  
  switchPointsLeftGoalLines1(spl[1]),
        switchPointsRightGoalLines1(spr[1]),
        leftGoalLines0(lgl[0]),
  rightGoalLines0(rgl[0]),
        leftGoalLines1(lgl[1]),
  rightGoalLines1(rgl[1]),
        leftGoalLines2(lgl[2]),
        rightGoalLines2(rgl[2]),
	leftGoalLines3(lgl[3]),
        rightGoalLines3(rgl[3]),
        currentLine(c),
    roadState(NORMAL),
    confidenceLevel(0),
    confidenceLevel_goalLine(clgl),
    noTrajectory(nothing)
    {}
    LaneDetectorDataToDriver (CustomLine lgl, CustomLine rgl, CustomLine c, bool nothing, int clgl):
        switchPointsLeftGoalLines0(),
  switchPointsRightGoalLines0(),  
  switchPointsLeftGoalLines1(),
        switchPointsRightGoalLines1(),
        leftGoalLines0(lgl),
  rightGoalLines0(rgl),
        leftGoalLines1(),
  rightGoalLines1(),
        leftGoalLines2(),
        rightGoalLines2(),
	leftGoalLines3(),
        rightGoalLines3(),
        currentLine(c),
    roadState(NORMAL),
    confidenceLevel(0),
    confidenceLevel_goalLine(clgl),
    noTrajectory(nothing)
    {}
    virtual ~LaneDetectorDataToDriver () {}

    void setRoadState(RoadState state){
    	roadState=state;
        }
    void setConfidence(int conf){
    	confidenceLevel = conf;
    }
    int switchPointsLeftGoalLines0;
    int switchPointsRightGoalLines0;
    int switchPointsLeftGoalLines1;
    int switchPointsRightGoalLines1;
    CustomLine leftGoalLines0;
    CustomLine rightGoalLines0;
    CustomLine leftGoalLines1;
    CustomLine rightGoalLines1;
    CustomLine leftGoalLines2;
    CustomLine rightGoalLines2;
    CustomLine leftGoalLines3;
    CustomLine rightGoalLines3;		
    CustomLine currentLine;
    RoadState roadState;
    int confidenceLevel;
    int confidenceLevel_goalLine;
    bool noTrajectory;
};

/**
 * This is an example how you can send data from one component to another.
 */
class LaneDetectionData : public odcore::data::SerializableData
{
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
    LaneDetectionData &operator=(const LaneDetectionData &obj);

    /**
     * This method returns the example data.
     *
     * @return example data.
     */
    Lines getLaneDetectionData() const;

    LaneDetectorDataToDriver getLaneDetectionDataDriver() const;

    /**
     * This method sets the example data.
     *
     * @param e Example data.
     */
    void setLaneDetectionData(const Lines &e, const LaneDetectorDataToDriver &dtd);

    uint32_t getFrameCount() const;
    void setFrameCount(const uint32_t &count);

    const string getClassification() const;

    void setClassification(const string &classfi);

    static int32_t ID();
    virtual int32_t getID() const;
    virtual const string getLongName() const;
    virtual const string getShortName() const;
    virtual ostream &operator<<(ostream &out) const;
    virtual istream &operator>>(istream &in);
    virtual const string toString() const;

private:
    uint32_t m_frame_count;
    Lines m_lines;
    string m_classification;// Inspection classification
    LaneDetectorDataToDriver m_dataToDriver;
};

} // msv

#endif /*LANEDETECTIONDATA_H_*/
