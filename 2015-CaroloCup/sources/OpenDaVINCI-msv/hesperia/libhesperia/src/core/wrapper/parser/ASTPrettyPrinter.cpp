/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <iostream>

#include "core/wrapper/parser/ASTNode.h"
#include "core/wrapper/parser/ASTPrettyPrinter.h"

namespace core {
    namespace wrapper {
        namespace parser {

            using namespace std;

            ASTPrettyPrinter::ASTPrettyPrinter() :
                    m_depth(0) {}

            ASTPrettyPrinter::~ASTPrettyPrinter() {}

            void ASTPrettyPrinter::visit(ASTNode *node) {
                if (node != NULL) {
                    if (node->getValue<string>() != "") {
                        for (uint32_t k = 0; k < m_depth; k++) {
                            clog << " ";
                        }
                        clog << "'" << node->getValue<string>() << "'" << endl;
                    } else {
                        vector<ASTNode*> list = node->getChildren();
                        vector<ASTNode*>::iterator it = list.begin();
                        while (it != list.end()) {
                            ASTNode *n = (*it++);
                            string key = n->getKey();
                            string value = n->getValue<string>();
                            for (uint32_t k = 0; k < m_depth; k++) {
                                clog << " ";
                            }
                            clog << "Depth: " << m_depth << ", Key: " << "'" << key << "'" << ", Value: " << "'" << value << "'" << endl;
                            m_depth++;
                            n->accept(*this);
                            m_depth--;
                        }
                    }
                }
            }

        }
    }
} // core::wrapper::parser
