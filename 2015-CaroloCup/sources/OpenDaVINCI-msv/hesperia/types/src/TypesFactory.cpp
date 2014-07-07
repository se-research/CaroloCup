/*
 * Copyright (c) Christian Berger.
 *
 * The Hesperia Framework.
 */

#include <cerrno>
#include <exception>
#include <sstream>

#include "core/macros.h"
#include "core/base/Lock.h"
#include "core/exceptions/Exceptions.h"
#include "core/wrapper/parser/ASTNode.h"
#include "core/wrapper/parser/ASTPrettyPrinter.h"

#include "TypesFactory.h"
#include "TypesGrammar.h"
#include "TypesVisitor.h"

namespace types {

    using namespace std;
    using namespace core::base;
    using namespace core::data;
    using namespace core::exceptions;
    using namespace core::wrapper::parser;
    using namespace hesperia::data::situation;

    // Initialize singleton instance.
    Mutex TypesFactory::m_singletonMutex;
    TypesFactory* TypesFactory::m_singleton = NULL;

    TypesFactory::TypesFactory() {}

    TypesFactory::~TypesFactory() {}

    TypesFactory& TypesFactory::getInstance() {
        {
            Lock l(TypesFactory::m_singletonMutex);
            if (TypesFactory::m_singleton == NULL) {
                TypesFactory::m_singleton = new TypesFactory();
            }
        }

        return (*TypesFactory::m_singleton);
    }

    Situation TypesFactory::getSituation(const string &s) throw (InvalidArgumentException) {
        Situation situation;

        SITGrammarTokenListener sittl(situation);
        SITGrammarErrorListener sitel;

        SITGrammar sitGrammar(sittl, sitel);
        ASTNode *root = sitGrammar.getAST(s.c_str());
        if (root == NULL) {
            errno = 0;
            OPENDAVINCI_CORE_THROW_EXCEPTION(InvalidArgumentException, sitel.getLastError());
        } else {
//                ASTPrettyPrinter pp;
//                pp.visit(root);

            // Visit AST with situation visitor to create Situation data structure.
            SITSituationVisitor situationVisitor(situation);
            root->accept(situationVisitor);

            // Clean up AST since all data is now in an instance of Situation.
            delete root;
        }

        return situation;
    }

    TypesFactory::SITGrammarTokenListener::SITGrammarTokenListener(hesperia::data::situation::Situation &s) :
            m_situation(s) {}

    TypesFactory::SITGrammarTokenListener::~SITGrammarTokenListener() {}

    void TypesFactory::SITGrammarTokenListener::nextToken(const core::wrapper::parser::ParserToken &token) {
        try {
            const SITGrammarTokenIdentifier &tid = dynamic_cast<const SITGrammarTokenIdentifier&>(token.getExtendedData());
            clog << "SITGrammarTokenListener: " << tid.getIdentifier() << ", value: " << token.getValue() << " was not processed." << endl;
        } catch (...) {
            // Ignore std::bad_cast because this factory cannot use irregular data to construct a scenario.
        }
    }

    TypesFactory::SITGrammarErrorListener::SITGrammarErrorListener() :
            m_lastError() {}

    TypesFactory::SITGrammarErrorListener::~SITGrammarErrorListener() {}

    void TypesFactory::SITGrammarErrorListener::errorToken(core::wrapper::parser::ParserError &error) {
        stringstream s;
        s << "SITGrammarErrorListener caught an unknown parser error at line: " << error.getExtendedData().getLine() << " in context '" << error.getContext() << "'.";
        m_lastError = s.str();
    }

    const string TypesFactory::SITGrammarErrorListener::getLastError() const {
        return m_lastError;
    }

} // types
