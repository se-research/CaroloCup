/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DECORATOR_THREED_RENDERER3D_H_
#define HESPERIA_CORE_DECORATOR_THREED_RENDERER3D_H_

#include <string>
#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/wrapper/StringComparator.h"

#include "hesperia/decorator/Renderer.h"

namespace hesperia {
    namespace decorator {
        namespace threeD {

            using namespace std;

            /**
             * This class decorates any data structure for rendering with OpenGL.
             */
            class OPENDAVINCI_API Renderer3D : public hesperia::decorator::Renderer {
                public:
                    Renderer3D();

                    virtual ~Renderer3D();

                public:
                    virtual void beginPainting();

                    virtual void endPainting();

                    virtual void setColor(const core::data::environment::Point3 &c);

                    virtual void setPointWidth(const double &width);

                    virtual void setLineWidth(const double &width);

                    virtual void drawText(const core::data::environment::Point3 &p, const string &text);

                    virtual void drawPoint(const core::data::environment::Point3 &p);

                    virtual void drawLine(const core::data::environment::Point3 &A, const core::data::environment::Point3 &B);

                    virtual void drawPolyLine(const vector<core::data::environment::Point3> &listOfPoints);

                    virtual void drawPolyLine(const vector<core::data::environment::Point3> &listOfPoints, const double &height);

                    virtual void drawImage(const core::wrapper::Image *image, const core::data::environment::Point3 &originPixelXY, const core::data::environment::Point3 &scalingPixelXY, const float &rotationZ);

                    virtual void drawTriangleSet(const hesperia::decorator::models::TriangleSet &ts);

                    virtual void drawListOfTriangleSets(const vector<hesperia::decorator::models::TriangleSet> &listOfTriangleSets);

                    virtual void drawTriangleSet(const hesperia::decorator::models::TriangleSet &ts, const core::data::environment::Point3 &position, const core::data::environment::Point3 &rotation);

                    virtual void drawListOfTriangleSets(const vector<hesperia::decorator::models::TriangleSet> &listOfTriangleSets, const core::data::environment::Point3 &position, const core::data::environment::Point3 &rotation);

                private:
                    vector<string> m_listOfImagesRegisteredAtTextureManager;
            };

        }
    }
} // hesperia::decorator::threeD

#endif /*HESPERIA_CORE_DECORATOR_THREED_RENDERER3D_H_*/
