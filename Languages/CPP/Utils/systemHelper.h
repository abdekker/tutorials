#pragma once

// Header-only helper class for some system and C++ utilities
using namespace std;
class systemHelper
{
public:
     // Constructor / destructor
    systemHelper() {}
    ~systemHelper() {}

    void SetFlushOutputBuffer(void)
    {
         // Ensure the output buffer is flushed on each insertion operation
         setvbuf(stdout, NULL, _IONBF, 0);

         // Alternative:
         //std::cout << std::unitbuf;
    }
};
