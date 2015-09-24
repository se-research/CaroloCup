#ifndef MEASURE_TIME
#define MEASURE_TIME


#include <iostream>
#include <chrono>

/// This function take from: Nikos Athanasiou(http://stackoverflow.com/)
/// Note: Only support --std=c++11
/// std::cout << measure<>::execution( your_function_here ) << std::endl;

template<typename TimeT = std::chrono::milliseconds>
struct measure
{
    template<typename F, typename ...Args>
    static typename TimeT::rep execution(F&& func, Args&&... args)
    {
        auto start = std::chrono::system_clock::now();
        std::forward<decltype(func)>(func)(std::forward<Args>(args)...);
        auto duration = std::chrono::duration_cast< TimeT>(std::chrono::system_clock::now() - start);
        return duration.count();
    }
};

#endif // MEASURE_TIME


