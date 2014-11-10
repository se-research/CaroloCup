/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef HESPERIA_SCENEGRAPHTESTSUITE_H_
#define HESPERIA_SCENEGRAPHTESTSUITE_H_

#include <sstream>

#include "cxxtest/TestSuite.h"

#include "hesperia/scenegraph/SceneNode.h"
#include "hesperia/scenegraph/SceneNodeDescriptor.h"
#include "hesperia/scenegraph/SceneNodeVisitor.h"

using namespace std;
using namespace hesperia::scenegraph;

class SceneNodePrettyPrinter : public SceneNodeVisitor {
    public:
        stringstream m_value;

    public:
        SceneNodePrettyPrinter() :
            m_value() {}

        virtual void visit(SceneNode *sceneNode) {
            if (sceneNode != NULL) {
                m_value << sceneNode->getSceneNodeDescriptor().getName() << "->";
            }
        }
};

class SceneGraphTest : public CxxTest::TestSuite {
    public:
        void testSceneGraph1() {
            SceneNode root(SceneNodeDescriptor("Root"));

            TS_ASSERT(root.getNumberOfChildren() == 0);
        }

        void testSceneGraph2() {
            SceneNode root(SceneNodeDescriptor("Root"));

            root.addChild(new SceneNode(SceneNodeDescriptor("Child 1")));

            TS_ASSERT(root.getNumberOfChildren() == 1);
            TS_ASSERT(root.getSceneNodeDescriptor().getName() == "Root");
        }

        void testSceneGraph3() {
            SceneNode root(SceneNodeDescriptor("Root"));
            SceneNode *child1 = new SceneNode(SceneNodeDescriptor("Child 1"));
            SceneNode *child2 = new SceneNode(SceneNodeDescriptor("Child 2"));

            root.addChild(child1);
            child1->addChild(child2);

            TS_ASSERT(root.getNumberOfChildren() == 1);
            TS_ASSERT(root.getSceneNodeDescriptor().getName() == "Root");
            TS_ASSERT(child1->getNumberOfChildren() == 1);
            TS_ASSERT(child1->getSceneNodeDescriptor().getName() == "Child 1");
            TS_ASSERT(child2->getNumberOfChildren() == 0);
            TS_ASSERT(child2->getSceneNodeDescriptor().getName() == "Child 2");
        }

        void testSceneGraph4() {
            SceneNode root(SceneNodeDescriptor("Root"));
            SceneNode *child1 = new SceneNode(SceneNodeDescriptor("Child 1"));
            SceneNode *child2 = new SceneNode(SceneNodeDescriptor("Child 2"));

            root.addChild(child1);
            child1->addChild(child2);

            TS_ASSERT(root.getNumberOfChildren() == 1);
            TS_ASSERT(root.getSceneNodeDescriptor().getName() == "Root");
            TS_ASSERT(child1->getNumberOfChildren() == 1);
            TS_ASSERT(child1->getSceneNodeDescriptor().getName() == "Child 1");
            TS_ASSERT(child2->getNumberOfChildren() == 0);
            TS_ASSERT(child2->getSceneNodeDescriptor().getName() == "Child 2");

            SceneNodePrettyPrinter snpp;
            root.accept(snpp);

            TS_ASSERT(snpp.m_value.str() == "Root->Child 1->Child 2->");
        }

        void testSceneGraph5() {
            SceneNode root(SceneNodeDescriptor("Root"));
            SceneNode *child1 = new SceneNode(SceneNodeDescriptor("Child 1"));
            SceneNode *child2 = new SceneNode(SceneNodeDescriptor("Child 2"));

            root.addChild(child1);
            child1->addChild(child2);

            TS_ASSERT(root.getNumberOfChildren() == 1);
            TS_ASSERT(root.getSceneNodeDescriptor().getName() == "Root");
            TS_ASSERT(child1->getNumberOfChildren() == 1);
            TS_ASSERT(child1->getSceneNodeDescriptor().getName() == "Child 1");
            TS_ASSERT(child2->getNumberOfChildren() == 0);
            TS_ASSERT(child2->getSceneNodeDescriptor().getName() == "Child 2");

            SceneNodePrettyPrinter snpp;
            root.accept(snpp);

            TS_ASSERT(snpp.m_value.str() == "Root->Child 1->Child 2->");

            root.deleteAllChildren();

            TS_ASSERT(root.getNumberOfChildren() == 0);
            TS_ASSERT(root.getSceneNodeDescriptor().getName() == "Root");

            SceneNodePrettyPrinter snpp2;
            root.accept(snpp2);

            TS_ASSERT(snpp2.m_value.str() == "Root->");
        }

        void testSceneGraph6() {
            SceneNode root(SceneNodeDescriptor("Root"));
            SceneNode *child1 = new SceneNode(SceneNodeDescriptor("Child 1"));
            SceneNode *child2 = new SceneNode(SceneNodeDescriptor("Child 2"));

            root.addChild(child1);
            child1->addChild(child2);

            TS_ASSERT(root.getNumberOfChildren() == 1);
            TS_ASSERT(root.getSceneNodeDescriptor().getName() == "Root");
            TS_ASSERT(child1->getNumberOfChildren() == 1);
            TS_ASSERT(child1->getSceneNodeDescriptor().getName() == "Child 1");
            TS_ASSERT(child2->getNumberOfChildren() == 0);
            TS_ASSERT(child2->getSceneNodeDescriptor().getName() == "Child 2");

            SceneNodePrettyPrinter snpp;
            root.accept(snpp);

            TS_ASSERT(snpp.m_value.str() == "Root->Child 1->Child 2->");

            child1->removeChild(child2);

            TS_ASSERT(root.getNumberOfChildren() == 1);
            TS_ASSERT(root.getSceneNodeDescriptor().getName() == "Root");
            TS_ASSERT(child1->getNumberOfChildren() == 0);
            TS_ASSERT(child1->getSceneNodeDescriptor().getName() == "Child 1");

            SceneNodePrettyPrinter snpp2;
            root.accept(snpp2);

            TS_ASSERT(snpp2.m_value.str() == "Root->Child 1->");
        }

};

#endif /*HESPERIA_SCENEGRAPHTESTSUITE_H_*/
