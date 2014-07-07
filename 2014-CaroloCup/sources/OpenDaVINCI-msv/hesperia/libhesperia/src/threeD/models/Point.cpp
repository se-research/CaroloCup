/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

// The following include is necessary on Win32 platforms to set up necessary macro definitions.
#ifdef WIN32
#include <windows.h>
#endif

#include <GL/gl.h>

#include "hesperia/threeD/models/Point.h"

namespace hesperia {
    namespace threeD {
        namespace models {

            using namespace core::data::environment;

            Point::Point(const NodeDescriptor &nodeDescriptor, const Point3 &position, const Point3 &color, const float &width) :
                    Node(nodeDescriptor),
                    m_position(position),
                    m_color(color),
                    m_width(width) {}

            Point::Point(const Point &obj) :
                    Node(obj.getNodeDescriptor()),
                    m_position(obj.m_position),
                    m_color(obj.m_color),
                    m_width(obj.m_width) {}

            Point::~Point() {}

            Point& Point::operator=(const Point &obj) {
                setNodeDescriptor(obj.getNodeDescriptor());
                m_position = obj.m_position;
                m_color = obj.m_color;
                m_width = obj.m_width;

                return (*this);
            }

            void Point::render(RenderingConfiguration &renderingConfiguration) {
                if ((getNodeDescriptor().getName().size() == 0) || (renderingConfiguration.getNodeRenderingConfiguration(getNodeDescriptor()).hasParameter(NodeRenderingConfiguration::ENABLED))) {
                    glPushMatrix();
                    {
                        glPointSize(m_width);
                        glColor3d(m_color.getX(), m_color.getY(), m_color.getZ());

                        glBegin(GL_POINTS);
                        glVertex3d(m_position.getX(), m_position.getY(), m_position.getZ());
                        glEnd();

                        glPointSize(1);
                    }
                    glPopMatrix();
                }
            }

        }
    }
} // hesperia::threeD::models
