/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */
#ifndef CSVEXPORTERSENSOR_H_
#define CSVEXPORTERSENSOR_H_

#include <memory>
#include "opendavinci/odcore/base/module/TimeTriggeredConferenceClientModule.h"
#include "opendavinci/odcore/base/FIFOQueue.h"

namespace msv
{
    using namespace std;

  class CsvExporterSensor : public odcore::base::module::TimeTriggeredConferenceClientModule
  {
  private:
    /**
     * "Forbidden" copy constructor. Goal: The compiler should warn
     * already at compile time for unwanted bugs caused by any misuse
     * of the copy constructor.
     *
     * @param obj Reference to an object of this class.
     */
    CsvExporterSensor (const CsvExporterSensor &/*obj*/);

    /**
     * "Forbidden" assignment operator. Goal: The compiler should warn
     * already at compile time for unwanted bugs caused by any misuse
     * of the assignment operator.
     *
     * @param obj Reference to an object of this class.
     * @return Reference to this instance.
     */
    CsvExporterSensor&
    operator= (const CsvExporterSensor &/*obj*/);

  public:
    /**
     * Constructor.
     *
     * @param argc Number of command line arguments.
     * @param argv Command line arguments.
     */
    CsvExporterSensor (const int32_t &argc, char **argv);

    virtual ~CsvExporterSensor();

    odcore::data::dmcp::ModuleExitCodeMessage::ModuleExitCode
    body ();

  private:
    uint32_t frame_count;
    bool m_debug;
    odcore::base::FIFOQueue m_fifo;
    shared_ptr<ostream> m_out;

    virtual void
    setUp ();

    virtual void
    tearDown ();

  };

}
#endif
