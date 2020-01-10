#pragma once
#include <chrono>
#include <iostream>
#include <stdio.h>
#include <string.h>
#include <vector>

#include "../../Languages/CPP/Utils/stringHelper.h"

using namespace std;
//using std::chrono::high_resolution_clock;
//using std::vector;
class ClassPuzzle001
{
public:
    ClassPuzzle001() {}
    ~ClassPuzzle001() {}

    void Attempt1();
    void Attempt1NoPrint(vector<int>& vec);

    void Attempt2();
    void Attempt2NoPrint(vector<int>& vec);

    void Attempt3();
    void Attempt3NoPrint(vector<int>& vec);

    void Attempt4();
    void Attempt4NoPrint(vector<int>& vec);

    void PerformanceTests();
};
