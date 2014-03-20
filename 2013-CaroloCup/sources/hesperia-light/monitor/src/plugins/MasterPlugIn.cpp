/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "plugins/MasterPlugIn.h"
#include "core/base/Lock.h"

#include "plugins/PlugIn.h"
#include "core/base/KeyValueConfiguration.h"
#include "QtIncludes.h"


namespace plugins {

	using namespace std;
    using namespace core::base;
    using namespace hesperia::base;
    using namespace monitor;


    MasterPlugIn::MasterPlugIn(const string &name, const core::base::KeyValueConfiguration &kvc, DataStoreManager &dsm, QWidget* prnt) :
    	PlugIn(name,kvc,prnt),
    	m_fifoMultiplexer(){
    	m_fifoMultiplexer = new FIFOMultiplexer(dsm);

    }

    MasterPlugIn::~MasterPlugIn(){
    }

    FIFOMultiplexer*
    MasterPlugIn::getMultiplexer(){
    	return m_fifoMultiplexer;
    }

    void
    MasterPlugIn::setMultiplexer(monitor::FIFOMultiplexer* multiplexer) {
    	m_fifoMultiplexer = multiplexer;
    }
}
