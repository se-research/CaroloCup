/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CORE_TREENODETESTSUITE_H_
#define CORE_TREENODETESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <string>

#include "core/base/TreeNode.h"

using namespace std;
using namespace core::base;

class TreeNodeTest : public CxxTest::TestSuite {
    public:
        string printNodes(TreeNode<string> *ptr, string current) {
            string retVal;
            if (ptr != NULL) {
                vector<TreeNode<string>* > listOfChildren = ptr->getChildren();

                if (listOfChildren.size() > 0) {
                    vector<TreeNode<string>* >::iterator it = listOfChildren.begin();
                    while (it != listOfChildren.end()) {
                        retVal = retVal + printNodes((*it++), current);
                    }
                }

                retVal += ptr->getValue();
            }
            return retVal;
        }

        void testSimpleTree() {
            TreeNode<string> *root = new TreeNode<string>();

            TreeNode<string> *child1 = new TreeNode<string>();
            root->addChild(child1);
            child1->setValue("Hello ");

            TreeNode<string> *child2 = new TreeNode<string>();
            root->addChild(child2);
            child2->setValue("World!");

            string retVal = printNodes(root, "");

            TS_ASSERT(retVal == "Hello World!");

            delete root;
        }
};

#endif /*CORE_TREENODETESTSUITE_H_*/
