#pragma once

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

class ClassPuzzle0001
{
public:
    ClassPuzzle0001() {}
    ~ClassPuzzle0001() {}

	// Main entry point
	void RunPuzzle();

private:
    void DoAttempt(const AttemptNumber attempt, const int target, const bool print);
    void PerformanceTests();
};
