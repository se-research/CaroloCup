/*
 * Copyright (c) Christian Berger.
 *
 * The Hesperia Framework.
 */

#include <boost/spirit/include/classic_parse_tree.hpp>

#include "TypesGrammar.h"

namespace types {

    using namespace core::wrapper::parser;

    SITGrammarTokenIdentifier::SITGrammarTokenIdentifier(const enum IDENTIFIERS &id) :
            m_id(id) {}

    SITGrammarTokenIdentifier::~SITGrammarTokenIdentifier() {}

    enum SITGrammarTokenIdentifier::IDENTIFIERS SITGrammarTokenIdentifier::getIdentifier() const {
        return m_id;
    }

    SITGrammar::SITGrammar(ParserTokenListener &ptl, ParserErrorListener &pel) :
            BoostSpiritGrammar(ptl, pel),
            expect(0),
            m_guard() {}

    SITGrammar::~SITGrammar() {}

    ASTNode* SITGrammar::getAST(const string &s) {
        // Reset lines counter.
        resetLines();

        // Parse the data and build an AST.
        ASTNode *root = NULL;
        tree_parse_info<> info = ast_parse(s.c_str(), *this, comment_p("/*", "*/"));

        // If the instance of the grammar could successfully parsed, build the AST.
        if (info.full == 1) {
            root = new ASTNode();
            buildAST(info.trees.begin(), root);
        }

        return root;
    }

} // types
