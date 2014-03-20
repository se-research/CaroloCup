/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef CORE_MATRIXTESTSUITE_H_
#define CORE_MATRIXTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <iostream>
#include <string>

#include <opencv/cv.h>

#include "core/SharedPointer.h"
#include "core/wrapper/Matrix.h"
#include "core/wrapper/MatrixFactory.h"

using namespace std;

class MatrixTest : public CxxTest::TestSuite {
    public:
        void testMatrix() {
            core::SharedPointer<core::wrapper::Matrix> m3x4(core::wrapper::MatrixFactory::getInstance().createMatrix(3, 4));
            TS_ASSERT(m3x4.isValid());
            TS_ASSERT(m3x4->getWidth() == 3);
            TS_ASSERT(m3x4->getHeight() == 4);

            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    TS_ASSERT_DELTA(m3x4->get(col, row), 0, 1e-5);
                }
            }

            float value = 1;
            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    m3x4->set(col, row, value++);
                }
            }

            value = 1;
            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    TS_ASSERT_DELTA(m3x4->get(col, row), value, 1e-5);
                    value++;
                }
            }

            CvMat *matrix = NULL;
            TS_ASSERT(matrix == NULL);

            matrix = m3x4->getRawMatrix<CvMat*>();
            TS_ASSERT(matrix != NULL);

            TS_ASSERT(static_cast<uint32_t>(matrix->width) == m3x4->getWidth());
            TS_ASSERT(static_cast<uint32_t>(matrix->height) == m3x4->getHeight());

            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    TS_ASSERT_DELTA(m3x4->get(col, row), cvmGet(matrix, row, col), 1e-5);
                }
            }
        }

        void testErroneousAccess() {
            core::SharedPointer<core::wrapper::Matrix> m3x4(core::wrapper::MatrixFactory::getInstance().createMatrix(3, 4));
            TS_ASSERT(m3x4.isValid());
            TS_ASSERT(m3x4->getWidth() == 3);
            TS_ASSERT(m3x4->getHeight() == 4);

            bool failed = true;
            try {
                m3x4->get(5, 7);
                failed = true;
            } catch (string &s) {
                failed = false;
                TS_ASSERT(s == "Array index out of bounds exception.");
            }

            TS_ASSERT(!failed);
        }

        void testMatrixAdditionWithoutAssignment() {
            core::SharedPointer<core::wrapper::Matrix> m3x4(core::wrapper::MatrixFactory::getInstance().createMatrix(3, 4));
            TS_ASSERT(m3x4.isValid());
            TS_ASSERT(m3x4->getWidth() == 3);
            TS_ASSERT(m3x4->getHeight() == 4);

            core::SharedPointer<core::wrapper::Matrix> m3x4_2(core::wrapper::MatrixFactory::getInstance().createMatrix(3, 4));
            TS_ASSERT(m3x4_2.isValid());
            TS_ASSERT(m3x4_2->getWidth() == 3);
            TS_ASSERT(m3x4_2->getHeight() == 4);

            core::SharedPointer<core::wrapper::Matrix> m3x4_sum(core::wrapper::MatrixFactory::getInstance().createMatrix(3, 4));
            TS_ASSERT(m3x4_sum.isValid());
            TS_ASSERT(m3x4_sum->getWidth() == 3);
            TS_ASSERT(m3x4_sum->getHeight() == 4);

            float value = 1;
            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    m3x4->set(col, row, value);
                    m3x4_2->set(col, row, value);

                    m3x4_sum->set(col, row, 2*value);

                    value++;
                }
            }

            value = 1;
            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    TS_ASSERT_DELTA(m3x4->get(col, row), value, 1e-5);
                    TS_ASSERT_DELTA(m3x4_2->get(col, row), value, 1e-5);
                    TS_ASSERT_DELTA(m3x4->get(col, row), m3x4_2->get(col, row), 1e-5);

                    TS_ASSERT_DELTA(m3x4_sum->get(col, row), 2*value, 1e-5);
                    value++;
                }
            }

            (*m3x4) += (*m3x4_2);

            value = 0;
            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    TS_ASSERT_DELTA(m3x4->get(col, row), m3x4_sum->get(col, row), 1e-5);
                }
            }
        }

        void testMatrixAdditionWithAssignment() {
            core::SharedPointer<core::wrapper::Matrix> m3x4(core::wrapper::MatrixFactory::getInstance().createMatrix(3, 4));
            TS_ASSERT(m3x4.isValid());
            TS_ASSERT(m3x4->getWidth() == 3);
            TS_ASSERT(m3x4->getHeight() == 4);

            core::SharedPointer<core::wrapper::Matrix> m3x4_2(core::wrapper::MatrixFactory::getInstance().createMatrix(3, 4));
            TS_ASSERT(m3x4_2.isValid());
            TS_ASSERT(m3x4_2->getWidth() == 3);
            TS_ASSERT(m3x4_2->getHeight() == 4);

            core::SharedPointer<core::wrapper::Matrix> m3x4_sum;
            TS_ASSERT(!m3x4_sum.isValid());

            float value = 1;
            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    m3x4->set(col, row, value);
                    m3x4_2->set(col, row, value);

                    value++;
                }
            }

            value = 1;
            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    TS_ASSERT_DELTA(m3x4->get(col, row), value, 1e-5);
                    TS_ASSERT_DELTA(m3x4_2->get(col, row), value, 1e-5);
                    TS_ASSERT_DELTA(m3x4->get(col, row), m3x4_2->get(col, row), 1e-5);

                    value++;
                }
            }

            m3x4_sum = core::SharedPointer<core::wrapper::Matrix>((*m3x4) + (*m3x4_2));

            value = 0;
            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    TS_ASSERT_DELTA(m3x4->get(col, row) + m3x4_2->get(col, row), m3x4_sum->get(col, row), 1e-5);
                }
            }
        }

        void testMatrixSubtractWithoutAssignment() {
            core::SharedPointer<core::wrapper::Matrix> m3x4(core::wrapper::MatrixFactory::getInstance().createMatrix(3, 4));
            TS_ASSERT(m3x4.isValid());
            TS_ASSERT(m3x4->getWidth() == 3);
            TS_ASSERT(m3x4->getHeight() == 4);

            core::SharedPointer<core::wrapper::Matrix> m3x4_2(core::wrapper::MatrixFactory::getInstance().createMatrix(3, 4));
            TS_ASSERT(m3x4_2.isValid());
            TS_ASSERT(m3x4_2->getWidth() == 3);
            TS_ASSERT(m3x4_2->getHeight() == 4);

            float value = 1;
            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    m3x4->set(col, row, value*2);
                    m3x4_2->set(col, row, value);

                    value++;
                }
            }

            value = 1;
            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    TS_ASSERT_DELTA(m3x4->get(col, row), 2*value, 1e-5);
                    TS_ASSERT_DELTA(m3x4_2->get(col, row), value, 1e-5);

                    value++;
                }
            }

            (*m3x4) -= (*m3x4_2);

            value = 0;
            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    TS_ASSERT_DELTA(m3x4->get(col, row), m3x4_2->get(col, row), 1e-5);
                }
            }
        }

        void testMatrixSubtractWithAssignment() {
            core::SharedPointer<core::wrapper::Matrix> m3x4(core::wrapper::MatrixFactory::getInstance().createMatrix(3, 4));
            TS_ASSERT(m3x4.isValid());
            TS_ASSERT(m3x4->getWidth() == 3);
            TS_ASSERT(m3x4->getHeight() == 4);

            core::SharedPointer<core::wrapper::Matrix> m3x4_2(core::wrapper::MatrixFactory::getInstance().createMatrix(3, 4));
            TS_ASSERT(m3x4_2.isValid());
            TS_ASSERT(m3x4_2->getWidth() == 3);
            TS_ASSERT(m3x4_2->getHeight() == 4);

            core::SharedPointer<core::wrapper::Matrix> m3x4_sum;
            TS_ASSERT(!m3x4_sum.isValid());

            float value = 1;
            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    m3x4->set(col, row, 2*value);
                    m3x4_2->set(col, row, value);

                    value++;
                }
            }

            value = 1;
            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    TS_ASSERT_DELTA(m3x4->get(col, row), 2*value, 1e-5);
                    TS_ASSERT_DELTA(m3x4_2->get(col, row), value, 1e-5);

                    value++;
                }
            }

            m3x4_sum = core::SharedPointer<core::wrapper::Matrix>((*m3x4) - (*m3x4_2));

            value = 0;
            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    TS_ASSERT_DELTA(m3x4->get(col, row) - m3x4_2->get(col, row), m3x4_sum->get(col, row), 1e-5);
                }
            }
        }

        void testMatrixScalingWithoutAssignment() {
            core::SharedPointer<core::wrapper::Matrix> m3x4(core::wrapper::MatrixFactory::getInstance().createMatrix(3, 4));
            TS_ASSERT(m3x4.isValid());
            TS_ASSERT(m3x4->getWidth() == 3);
            TS_ASSERT(m3x4->getHeight() == 4);

            float value = 1;
            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    m3x4->set(col, row, value);

                    value++;
                }
            }

            value = 1;
            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    TS_ASSERT_DELTA(m3x4->get(col, row), value, 1e-5);

                    value++;
                }
            }

            (*m3x4) *= 3.0;

            value = 1;
            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    TS_ASSERT_DELTA(m3x4->get(col, row), value * 3.0, 1e-5);
                    value++;
                }
            }
        }

        void testMatrixScalingWithAssignment() {
            core::SharedPointer<core::wrapper::Matrix> m3x4(core::wrapper::MatrixFactory::getInstance().createMatrix(3, 4));
            TS_ASSERT(m3x4.isValid());
            TS_ASSERT(m3x4->getWidth() == 3);
            TS_ASSERT(m3x4->getHeight() == 4);

            core::SharedPointer<core::wrapper::Matrix> m3x4_2(core::wrapper::MatrixFactory::getInstance().createMatrix(3, 4));
            TS_ASSERT(m3x4_2.isValid());
            TS_ASSERT(m3x4_2->getWidth() == 3);
            TS_ASSERT(m3x4_2->getHeight() == 4);

            float value = 1;
            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    m3x4->set(col, row, value);

                    value++;
                }
            }

            value = 1;
            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    TS_ASSERT_DELTA(m3x4->get(col, row), value, 1e-5);
                    TS_ASSERT_DELTA(m3x4_2->get(col, row), 0, 1e-5);

                    value++;
                }
            }

            m3x4_2 = core::SharedPointer<core::wrapper::Matrix>((*m3x4) * 3.0);

            value = 0;
            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    TS_ASSERT_DELTA(m3x4->get(col, row) * 3.0, m3x4_2->get(col, row), 1e-5);
                }
            }
        }

        void testMatrixMultiplyWithoutAssignment() {
            core::SharedPointer<core::wrapper::Matrix> m3x3(core::wrapper::MatrixFactory::getInstance().createMatrix(3, 3));
            TS_ASSERT(m3x3.isValid());
            TS_ASSERT(m3x3->getWidth() == 3);
            TS_ASSERT(m3x3->getHeight() == 3);

            core::SharedPointer<core::wrapper::Matrix> m3x3_2(core::wrapper::MatrixFactory::getInstance().createMatrix(3, 3));
            TS_ASSERT(m3x3_2.isValid());
            TS_ASSERT(m3x3_2->getWidth() == 3);
            TS_ASSERT(m3x3_2->getHeight() == 3);

            m3x3->set(0, 0, 1);
            m3x3->set(1, 0, 2);
            m3x3->set(2, 0, 3);

            m3x3->set(0, 1, 4);
            m3x3->set(1, 1, 5);
            m3x3->set(2, 1, 6);

            m3x3->set(0, 2, 7);
            m3x3->set(1, 2, 8);
            m3x3->set(2, 2, 9);

            m3x3_2->set(0, 0, 10);
            m3x3_2->set(1, 0, 20);
            m3x3_2->set(2, 0, 30);

            m3x3_2->set(0, 1, 40);
            m3x3_2->set(1, 1, 50);
            m3x3_2->set(2, 1, 60);

            m3x3_2->set(0, 2, 70);
            m3x3_2->set(1, 2, 80);
            m3x3_2->set(2, 2, 90);

            float value = 1;
            for (uint32_t row = 0; row < 3; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    TS_ASSERT_DELTA(m3x3->get(col, row), value, 1e-5);
                    TS_ASSERT_DELTA(m3x3_2->get(col, row), value*10, 1e-5);

                    value++;
                }
            }

            (*m3x3) *= (*m3x3_2);

            TS_ASSERT_DELTA(m3x3->get(0, 0), 300, 1e-5);
            TS_ASSERT_DELTA(m3x3->get(1, 0), 360, 1e-5);
            TS_ASSERT_DELTA(m3x3->get(2, 0), 420, 1e-5);

            TS_ASSERT_DELTA(m3x3->get(0, 1), 660, 1e-5);
            TS_ASSERT_DELTA(m3x3->get(1, 1), 810, 1e-5);
            TS_ASSERT_DELTA(m3x3->get(2, 1), 960, 1e-5);

            TS_ASSERT_DELTA(m3x3->get(0, 2), 1020, 1e-5);
            TS_ASSERT_DELTA(m3x3->get(1, 2), 1260, 1e-5);
            TS_ASSERT_DELTA(m3x3->get(2, 2), 1500, 1e-5);
        }

        void testMatrixMultiplyWithAssignment() {
            core::SharedPointer<core::wrapper::Matrix> m3x3(core::wrapper::MatrixFactory::getInstance().createMatrix(3, 3));
            TS_ASSERT(m3x3.isValid());
            TS_ASSERT(m3x3->getWidth() == 3);
            TS_ASSERT(m3x3->getHeight() == 3);

            core::SharedPointer<core::wrapper::Matrix> m3x3_2(core::wrapper::MatrixFactory::getInstance().createMatrix(3, 3));
            TS_ASSERT(m3x3_2.isValid());
            TS_ASSERT(m3x3_2->getWidth() == 3);
            TS_ASSERT(m3x3_2->getHeight() == 3);

            core::SharedPointer<core::wrapper::Matrix> m3x3_product;
            TS_ASSERT(!m3x3_product.isValid());

            m3x3->set(0, 0, 1);
            m3x3->set(1, 0, 2);
            m3x3->set(2, 0, 3);

            m3x3->set(0, 1, 4);
            m3x3->set(1, 1, 5);
            m3x3->set(2, 1, 6);

            m3x3->set(0, 2, 7);
            m3x3->set(1, 2, 8);
            m3x3->set(2, 2, 9);

            m3x3_2->set(0, 0, 10);
            m3x3_2->set(1, 0, 20);
            m3x3_2->set(2, 0, 30);

            m3x3_2->set(0, 1, 40);
            m3x3_2->set(1, 1, 50);
            m3x3_2->set(2, 1, 60);

            m3x3_2->set(0, 2, 70);
            m3x3_2->set(1, 2, 80);
            m3x3_2->set(2, 2, 90);

            float value = 1;
            for (uint32_t row = 0; row < 3; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    TS_ASSERT_DELTA(m3x3->get(col, row), value, 1e-5);
                    TS_ASSERT_DELTA(m3x3_2->get(col, row), value*10, 1e-5);

                    value++;
                }
            }

            m3x3_product = core::SharedPointer<core::wrapper::Matrix>( (*m3x3) * (*m3x3_2) );

            TS_ASSERT_DELTA(m3x3_product->get(0, 0), 300, 1e-5);
            TS_ASSERT_DELTA(m3x3_product->get(1, 0), 360, 1e-5);
            TS_ASSERT_DELTA(m3x3_product->get(2, 0), 420, 1e-5);

            TS_ASSERT_DELTA(m3x3_product->get(0, 1), 660, 1e-5);
            TS_ASSERT_DELTA(m3x3_product->get(1, 1), 810, 1e-5);
            TS_ASSERT_DELTA(m3x3_product->get(2, 1), 960, 1e-5);

            TS_ASSERT_DELTA(m3x3_product->get(0, 2), 1020, 1e-5);
            TS_ASSERT_DELTA(m3x3_product->get(1, 2), 1260, 1e-5);
            TS_ASSERT_DELTA(m3x3_product->get(2, 2), 1500, 1e-5);
        }

        void testMatrixTranspose() {
            core::SharedPointer<core::wrapper::Matrix> m3x4(core::wrapper::MatrixFactory::getInstance().createMatrix(3, 4));
            TS_ASSERT(m3x4.isValid());
            TS_ASSERT(m3x4->getWidth() == 3);
            TS_ASSERT(m3x4->getHeight() == 4);

            float value = 1;
            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    m3x4->set(col, row, value);

                    value++;
                }
            }

            value = 1;
            for (uint32_t row = 0; row < 4; row++) {
                for (uint32_t col = 0; col < 3; col++) {
                    TS_ASSERT_DELTA(m3x4->get(col, row), value, 1e-5);

                    value++;
                }
            }

            m3x4->transpose();

            TS_ASSERT_DELTA(m3x4->get(0, 0), 1, 1e-5);
            TS_ASSERT_DELTA(m3x4->get(1, 0), 4, 1e-5);
            TS_ASSERT_DELTA(m3x4->get(2, 0), 7, 1e-5);
            TS_ASSERT_DELTA(m3x4->get(3, 0), 10, 1e-5);

            TS_ASSERT_DELTA(m3x4->get(0, 1), 2, 1e-5);
            TS_ASSERT_DELTA(m3x4->get(1, 1), 5, 1e-5);
            TS_ASSERT_DELTA(m3x4->get(2, 1), 8, 1e-5);
            TS_ASSERT_DELTA(m3x4->get(3, 1), 11, 1e-5);

            TS_ASSERT_DELTA(m3x4->get(0, 2), 3, 1e-5);
            TS_ASSERT_DELTA(m3x4->get(1, 2), 6, 1e-5);
            TS_ASSERT_DELTA(m3x4->get(2, 2), 9, 1e-5);
            TS_ASSERT_DELTA(m3x4->get(3, 2), 12, 1e-5);
        }
};

#endif /*CORE_MATRIXTESTSUITE_H_*/
