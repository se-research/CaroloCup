#include <iostream>
#include <cppunit/TestCase.h>
#include <cppunit/TestFixture.h>
#include <cppunit/ui/text/TextTestRunner.h>
#include <cppunit/extensions/HelperMacros.h>
#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/TestResult.h>
#include <cppunit/TestResultCollector.h>
#include <cppunit/TestRunner.h>
#include <cppunit/BriefTestProgressListener.h>
#include <cppunit/CompilerOutputter.h>
#include <cppunit/TextTestProgressListener.h>
#include <algorithm>
#include "camera_functions.hpp"

// Function definition
bool same_images(char*& img1, char*&img2);

using namespace CppUnit;
using namespace std;
using namespace cv;

class TestCameraFunctions : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(TestCameraFunctions);
	CPPUNIT_TEST(test_unique_images);
	CPPUNIT_TEST_SUITE_END();

protected:
	void test_unique_images();
};

void TestCameraFunctions::test_unique_images()
{
  // Initialize camera
  init_camera();

  int i;
  bool same = false;

  for (i = 0; i < 100; i++){

    char* img1;
    char* img2;
    get_image(img1);
    // sleep(100);
    get_image(img2);
    
    // Check if images are the same
    same = same_images(img1, img2);

    //If images are not same, exit the for loop, otherwise continue checking
    //if (same)
    //  break;
    
  }
  deinit_camera();
  
  CPPUNIT_ASSERT_MESSAGE("Matching uEye cam images", same == false);
}

bool same_images(char*& img1, char*&img2)
{
  if(strcmp(img1, img2) == 0){
    cout << "same images!" << endl;  
    return true;
  }
  else
    return false;
}

CPPUNIT_TEST_SUITE_REGISTRATION(TestCameraFunctions);

int main(int argc, char* argv[])
{

	// informs test-listener about testresults
	CPPUNIT_NS::TestResult testresult;
	
	// register listener for collecting the test-results
	CPPUNIT_NS::TestResultCollector collectedresults;
	testresult.addListener (&collectedresults);

	// register listener for per-test progress output
	CPPUNIT_NS::BriefTestProgressListener progress;
	testresult.addListener (&progress);
	
	// insert test-suite at test-runner by registry
	CPPUNIT_NS::TestRunner testrunner;
	testrunner.addTest (CPPUNIT_NS::TestFactoryRegistry::getRegistry().makeTest ());
	
	testrunner.run(testresult);

	// output results in compiler-format
	CPPUNIT_NS::CompilerOutputter compileroutputter(&collectedresults, std::cerr);
	compileroutputter.write ();	
	
	return 0;
}

