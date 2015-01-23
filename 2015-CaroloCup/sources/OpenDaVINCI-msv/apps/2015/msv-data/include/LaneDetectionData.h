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

//#include <opencv/cv.h>
#include "opencv2/opencv.hpp"

namespace msv
{

using namespace std;
using namespace cv;

class CustomLine
{
public:
    CustomLine () :
        p1(),
        p2(),
        slope(0),
        polygonIndex(-1) // This is a mapping to the polygon that the CustomLine was derived from.
    {}
    virtual ~CustomLine () {}

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

    Point p1, p2;
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

/**
 * This is an example how you can send data from one component to another.
 */
class LaneDetectionData : public core::data::SerializableData
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

    /**
     * This method sets the example data.
     *
     * @param e Example data.
     */
    void setLaneDetectionData(const Lines &e);

    uint32_t getFrameCount() const;
    void setFrameCount(const uint32_t &count);

    const string getClassification() const;

    void setClassification(const string &classfi);

    virtual ostream &operator<<(ostream &out) const;
    virtual istream &operator>>(istream &in);
    virtual const string toString() const;

private:
    uint32_t m_frame_count;
    Lines m_lines;
    string m_classification;// Inspection classification
};

} // msv

#endif /*LANEDETECTIONDATA_H_*/
