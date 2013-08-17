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

#include "hesperia/threeD/models/Line.h"

namespace hesperia {
    namespace threeD {
        namespace models {

            using namespace core::data::environment;

            Line::Line(const NodeDescriptor &nodeDescriptor, const Point3 &positionA, const Point3 &positionB, const Point3 &color, const float &width) :
                    Node(nodeDescriptor),
                    m_positionA(positionA),
                    m_positionB(positionB),
                    m_color(color),
                    m_width(width) {}

            Line::Line(const Line &obj) :
                    Node(obj.getNodeDescriptor()),
                    m_positionA(obj.m_positionA),
                    m_positionB(obj.m_positionB),
                    m_color(obj.m_color),
                    m_width(obj.m_width) {}

            Line::~Line() {}

            Line& Line::operator=(const Line &obj) {
                setNodeDescriptor(obj.getNodeDescriptor()),
                m_positionA = obj.m_positionA;
                m_positionB = obj.m_positionB;
                m_color = obj.m_color;
                m_width = obj.m_width;

                return (*this);
            }

            void Line::render(RenderingConfiguration &renderingConfiguration) {
                // Render if unnamed or not disabled.
                if ((getNodeDescriptor().getName().size() == 0) || (renderingConfiguration.getNodeRenderingConfiguration(getNodeDescriptor()).hasParameter(NodeRenderingConfiguration::ENABLED))) {
                    glPushMatrix();
                    {
                        glLineWidth(m_width);
                        glColor3d(m_color.getX(), m_color.getY(), m_color.getZ());

                        glBegin(GL_LINES);
                        glVertex3d(m_positionA.getX(), m_positionA.getY(), m_positionA.getZ());
                        glVertex3d(m_positionB.getX(), m_positionB.getY(), m_positionB.getZ());
                        glEnd();

                        glLineWidth(1);
                    }
                    glPopMatrix();
                }
            }

        }
    }
} // hesperia::threeD::models
