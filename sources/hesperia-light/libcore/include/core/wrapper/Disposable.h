/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_DISPOSABLE_H_
#define HESPERIA_CORE_WRAPPER_DISPOSABLE_H_

namespace core {
    namespace wrapper {

        /**
         * This interface marks an object as disposable.
         */
        class Disposable {
            public:
                virtual ~Disposable();
        };

    }
} // core::wrapper

#endif /*HESPERIA_CORE_WRAPPER_DISPOSABLE_H_*/
