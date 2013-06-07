/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef CORE_IMAGETESTSUITE_H_
#define CORE_IMAGETESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <cstdlib>
#include <fstream>

#include "core/SharedPointer.h"
#include "core/wrapper/Image.h"
#include "core/wrapper/ImageFactory.h"

using namespace std;

class ImageTest : public CxxTest::TestSuite {
    public:
        void testLoadImage() {
#ifdef WIN32
            clog << endl << "This testcase must be executed from a directory two layers deeper than the root directory of the source tree!" << endl;
            fstream fin("testsuites/TestImage.png", ios::binary | ios::in);
#else
            clog << endl << "This testcase must be executed from the root directory of the source tree!" << endl;
            fstream fin("libhesperia/testsuites/TestImage.png", ios::binary | ios::in);
#endif
            TS_ASSERT(fin.good());
            core::SharedPointer<core::wrapper::Image> img(core::wrapper::ImageFactory::getInstance().getImage(fin));
            TS_ASSERT(img.isValid());
            TS_ASSERT(img->getWidth() == 1280);
            TS_ASSERT(img->getHeight() == 1024);
            TS_ASSERT(img->getFormat() == core::wrapper::Image::BGR_24BIT);

            fin.close();
        }

};

#endif /*CORE_IMAGETESTSUITE_H_*/
