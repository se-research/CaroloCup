/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef MOCKS_PARAMETERVALUELIST_H
#define MOCKS_PARAMETERVALUELIST_H

#include <list>

namespace mocks {

    template <typename TYPE> class ParameterValueList
    {
        public:
            ParameterValueList() :
                m_items(),
                m_current(m_items.begin())
            {}

            void addItem(const TYPE& value) {
                m_items.push_back(value);
            }

            void prepare() {
                m_current = m_items.begin();
            }

            TYPE getCurrentItem() {
                TYPE value = *m_current;
                ++m_current;

                return value;
            }

            bool itemsAvaiable() {
                return (m_current != m_items.end());
            }

        private:
            std::list< TYPE > m_items;
            typename std::list< TYPE >::iterator m_current;
    };
}

#endif
