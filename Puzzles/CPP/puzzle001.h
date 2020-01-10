#pragma once
#include <chrono>
#include <iostream>
#include <stdio.h>
#include <string.h>
#include <vector>

#include "../../Languages/CPP/Utils/stringHelper.h"

enum AttemptNumber
{
    ATTEMPT_NONE,
    ATTEMPT_1,      // integer division 1
    ATTEMPT_2,      // integer division 2
    ATTEMPT_3,      // integer division 3
    ATTEMPT_4,      // convert to char *
    ATTEMPT_5,      // convert to std::string
    ATTEMPT_LAST
};

using namespace std;
class ClassPuzzle001
{
public:
    ClassPuzzle001() {}
    ~ClassPuzzle001() {}

    void DoAttempt(const AttemptNumber attempt, const int target, const bool print);
    void PerformanceTests();
};
