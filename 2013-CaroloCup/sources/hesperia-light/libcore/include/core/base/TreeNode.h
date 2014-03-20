/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_BASE_TREENODE_H_
#define HESPERIA_CORE_BASE_TREENODE_H_

#include <stddef.h>
#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/macros.h"

namespace core {
    namespace base {

        using namespace std;

        /**
         * This class represents a generic tree data structure.
         */
        template<typename T>
        class HESPERIA_API TreeNode {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                TreeNode(const TreeNode &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                TreeNode& operator=(const TreeNode &);

            public:
                TreeNode() :
                    m_value(),
                    m_parent(NULL),
                    m_children() {}

                virtual ~TreeNode() {
                    typename vector<TreeNode<T>* >::iterator it = m_children.begin();
                    while (it != m_children.end()) {
                        TreeNode<T> *child = (*it++);
                        delete child;
                    }
                    m_children.clear();
                }

                /**
                 * This method sets the value for this tree node.
                 *
                 * @param value Value.
                 */
                void setValue(T value) {
                    m_value = value;
                }

                /**
                 * This method returns the value.
                 *
                 * @return Value for this node.
                 */
                T getValue() {
                    return m_value;
                }

                /**
                 * This method returns the parent node.
                 *
                 * @return Parental node.
                 */
                TreeNode<T>* getParent() {
                    return m_parent;
                }

                /**
                 * This method sets the parent node.
                 *
                 * @param parent Parental node.
                 */
                void setParent(TreeNode<T> *parent) {
                    m_parent = parent;
                }

                /**
                 * This method adds a child.
                 *
                 * @param child Child to be added.
                 */
                void addChild(TreeNode<T> *child) {
                    if (child != NULL) {
                        child->setParent(this);
                        m_children.push_back(child);
                    }
                }

                /**
                 * This method returns the list of children.
                 *
                 * @return List of children.
                 */
                vector<TreeNode<T>* > getChildren() {
                    return m_children;
                }

            private:
                T m_value;
                TreeNode<T> *m_parent;
                vector<TreeNode<T>* > m_children;
        };

    }
} // core::base

#endif /*HESPERIA_CORE_BASE_TREENODE_H_*/
