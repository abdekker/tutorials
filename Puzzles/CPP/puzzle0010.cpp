#include "stdafx.h"

#include <iomanip>
#include <numeric>

/* You build a large tower of 200 Lego bricks. Each round, launch a missile (or ball) at the tower and
knock off a (different) prime number of bricks off the tower, fewer than remain. After each shot, the
number of bricks left in the tower was:
1) a prime
2) a square
3) a cube
4) a prime times a square greater than 1
5) a prime times a cube greater than 1
6) none of the aforementioned
7) a prime
The above would still be valid if the numbers blasted off by the sixth and seventh shots were swapped.
How many bricks remained in the tower after the seventh shot?

Source: Sunday Times, Teaser 3041 (2021-01-03) */

/* Note: The puzzle appears to be "cooked", as the following valid sequences are found:
Solution  1:   3  53  19  13  31  71   7   (bricks left = 3)
Solution  2:   3  53  19  13  31  67  11   (bricks left = 3)
Solution  3:   3  53  19  13  31  67   7   (bricks left = 7)
Solution  4:   3  53  19  13  31  59  11   (bricks left = 11)
Solution  5:   3  53  19  13  31  47  23   (bricks left = 11)
Solution  6:   3  53  19  13  31  47  11   (bricks left = 23)
Solution  7:   3  53  19  13  31  43   7   (bricks left = 31)
Solution  8:   3  53  19  13  31  23  47   (bricks left = 11)
Solution  9:   3  53  19  13  31  23  11   (bricks left = 47)
Solution 10:   3  53  19  13  31  11  67   (bricks left = 3)
Solution 11:   3  53  19  13  31  11  59   (bricks left = 11)
Solution 12:   3  53  19  13  31  11  47   (bricks left = 23)
Solution 13:   3  53  19  13  31  11  23   (bricks left = 47)
Solution 14:   3  53  19  13  31   7  71   (bricks left = 3)
Solution 15:   3  53  19  13  31   7  67   (bricks left = 7)
Solution 16:   3  53  19  13  31   7  43   (bricks left = 31)

This appears to give valid solutions of: 3, 7, 11, 23, 31 and 47 */
#define ROUND1 0
#define ROUND2 1
#define ROUND3 2
#define ROUND4 3
#define ROUND5 4
#define ROUND6 5
#define ROUND7 6

mathHelper mathHelp;
vectorHelper vectorHelp;

// Structure to hold the puzzle details (allowing them to be passed around easily)
struct Puzzle0010 {
    // Main puzzle details
    int startingBricks;        // Number of starting bricks in the tower
    int totalRounds;

    // Pre-calculated lists (all strictly less than the starting number of bricks)
    ARRAY primes;           // round 1 & 7
    ARRAY squares;          // round 2
    ARRAY cubes;            // round 3
    ARRAY primeSquares;     // round 4
    ARRAY primeCubes;       // round 5

    // Puzzle details
    int round;
    MATRIX possibles;
    ARRAY candidate;

    // Solutions (if any)
    MATRIX solutions;
};

bool IsRemainingBricksValid(Puzzle0010* puzzle, const int round, const int bricks)
{
    if (round == ROUND1)
    {
        // Bricks should be a prime
        if (mathHelp.IsPrime(bricks))
            return true;
    }
    else if (round == ROUND2)
    {
        // Bricks should be a square
        if (vectorHelp.Contains(puzzle->squares, bricks))
            return true;
    }
    else if (round == ROUND3)
    {
        // Bricks should be a cube
        if (vectorHelp.Contains(puzzle->cubes, bricks))
            return true;
    }
    else if (round == ROUND4)
    {
        // Bricks should be a square (greater than 1) times a prime
        if (vectorHelp.Contains(puzzle->primeSquares, bricks))
            return true;
    }
    else if (round == ROUND5)
    {
        // Bricks should be a cube (greater than 1) times a prime
        if (vectorHelp.Contains(puzzle->primeCubes, bricks))
            return true;
    }
    else if (round == ROUND6)
    {
        // Bricks should be none of the above
        if ((!mathHelp.IsPrime(bricks)) &&
            (!vectorHelp.Contains(puzzle->squares, bricks)) &&
            (!vectorHelp.Contains(puzzle->cubes, bricks)) &&
            (!vectorHelp.Contains(puzzle->primeSquares, bricks)) &&
            (!vectorHelp.Contains(puzzle->primeCubes, bricks)))
            return true;
    }
    if (round == ROUND7)
    {
        // Bricks should be a prime
        if (mathHelp.IsPrime(bricks))
            return true;
    }

    return false;
}

bool IsSwappingRoundsValid(Puzzle0010* puzzle)
{
    // After finding a valid sequence of removals, check the final condition to swap rounds 6 & 7.
    // The resulting candidate solution should also be valid.
    ARRAY localCandidate;
    localCandidate.push_back(puzzle->startingBricks - puzzle->candidate[ROUND1]);
    localCandidate.push_back(localCandidate[ROUND1] - puzzle->candidate[ROUND2]);
    localCandidate.push_back(localCandidate[ROUND2] - puzzle->candidate[ROUND3]);
    localCandidate.push_back(localCandidate[ROUND3] - puzzle->candidate[ROUND4]);
    localCandidate.push_back(localCandidate[ROUND4] - puzzle->candidate[ROUND5]);
    localCandidate.push_back(localCandidate[ROUND5] - puzzle->candidate[ROUND7]);   // } Rounds 6 & 7 swapped
    localCandidate.push_back(localCandidate[ROUND6] - puzzle->candidate[ROUND6]);   // }
    for (int round=ROUND1; round <= ROUND7; round++)
    {
        if (!IsRemainingBricksValid(puzzle, round, localCandidate[round]))
            return false;
    }

    return true;
}

int GetRemainingBricks(Puzzle0010* puzzle, ARRAY* solution)
{
    // For valid solutions, this helper calculates the bricks left in the tower
    int bricksRemaining = puzzle->startingBricks;
    for (int round=ROUND1; round <= ROUND7; round++)
    {
        bricksRemaining -= solution->at(round);
    }

    return bricksRemaining;
}

void FillRound(Puzzle0010* puzzle)
{
    // Set the possibilities for removing bricks this round, which must satisfy these criteria:
    // a) number removed must be prime and fewer than the number of remaining bricks
    //        Note: "puzzle.primes" was set up during initialisation and can be used for this purpose
    // b) number removed should be different to any number removed in previous rounds
    // c) after removal, remaining bricks should meet the criteria for this round (see puzzle details at top)

    // Count bricks remaining in the tower (ie. removed in previous rounds)
    int bricksInTower =
        (puzzle->startingBricks - std::accumulate(puzzle->candidate.begin(), puzzle->candidate.end(), 0));

    // Enumerate possible removals for this round
    int bricksRemaining = 0;
    for (int& elem : puzzle->primes)    // C++11 range-based loop
    {
        if (elem < bricksInTower)
        {
            // Fewer than the remaining number of bricks in the tower!
            bricksRemaining = (bricksInTower - elem);
            if (!vectorHelp.Contains(puzzle->candidate, elem))
            {
                // Unique number of bricks removed from the tower!
                if (IsRemainingBricksValid(puzzle, puzzle->round, bricksRemaining))
                {
                    // ...and the remaining bricks is valid for this round!
                    puzzle->possibles[puzzle->round].push_back(elem);
                }
            }
        }
        else
            break;    // Primes were added monotonically increasing, so safe to exit...
    }
}

void PopRound(Puzzle0010* puzzle)
{
    // Go backwards, one round at a time, cleaning up...
    while (
        (puzzle->round >= ROUND1) &&
        (puzzle->possibles[puzzle->round].size() == 0))
    {
        puzzle->round--;
        if ((puzzle->round >= ROUND1) &&
            (puzzle->possibles[puzzle->round].size() > 0))
        {
            puzzle->possibles[puzzle->round].pop_back();
            puzzle->candidate.pop_back();
        }
    }

    if (puzzle->round < ROUND1)
        puzzle->round = ROUND1;
}

int RunPuzzle0010(int startingBricks)
{
    if (startingBricks > 10000)
    {
        std::cout << "(too many bricks)\n";
        return -1;
    }

    // Count the number of solutions
    Puzzle0010 puzzle;
    puzzle.startingBricks = startingBricks;
    puzzle.totalRounds = 7; // ie. rounds go from 0 to 6

    // Pre-calculate lists to speed up calculations. These will the possible number of bricks left in the
    // tower at the end of each round (and therefore strictly fewer than the number of starting bricks).

    // List: Primes (used for removal each round and the requirement for rounds 1 & 7)
    puzzle.primes.push_back(2);
    for (int prime = 3; prime < puzzle.startingBricks; prime += 2)
    {
        if (mathHelp.IsPrime(prime))
            puzzle.primes.push_back(prime);
    }

    //std::cout << "Primes under " << startingBricks << ":\n";
    //PrintArray(puzzle.primes);

    // List: squares (requirement for round 2)
    int sq = 1;
    while ((sq * sq) < puzzle.startingBricks)
    {
        puzzle.squares.push_back((sq * sq));
        sq++;
    }

    // List: cubes (requirement for round 3)
    int cb = 1;
    while ((cb * cb * cb) < puzzle.startingBricks)
    {
        puzzle.cubes.push_back((cb * cb * cb));
        cb++;
    }

    // List: squares (greater than 1) times primes (requirement for round 4)
    for (int prime = 0; prime < puzzle.primes.size(); prime++)
    {
        for (int sq = 2; sq < puzzle.startingBricks; sq++)
        {
            if ((puzzle.primes[prime] * sq * sq) < puzzle.startingBricks)
                puzzle.primeSquares.push_back((puzzle.primes[prime] * sq * sq));
            else
                break;
        }
    }
    //PrintArray(puzzle.primeSquares);

    // List: cubes (greater than 1) times primes (requirement for round 5)
    for (int prime = 0; prime < puzzle.primes.size(); prime++)
    {
        for (int cb = 2; cb < puzzle.startingBricks; cb++)
        {
            if ((puzzle.primes[prime] * cb * cb * cb) < puzzle.startingBricks)
                puzzle.primeCubes.push_back((puzzle.primes[prime] * cb * cb * cb));
            else
                break;
        }
    }
    //PrintArray(puzzle.primeCubes);

    // Create an array of possible solutions which we will iterate through
    int round;
    for (round=ROUND1; round < puzzle.totalRounds; round++)
    {
        ARRAY rd;
        puzzle.possibles.push_back(rd);
    }

    // Solve the puzzle! Start with round 1...
    int candidatesConsidered = 0;
    puzzle.round = ROUND1;
    FillRound(&puzzle);
    while (puzzle.possibles[ROUND1].size() > 0)
    {
        if (puzzle.round < ROUND7)
        {
            // Rounds 1-6
            if (puzzle.possibles[puzzle.round].size() > 0)
            {
                // Add a possible from this round and move to the next round
                puzzle.candidate.push_back(puzzle.possibles[puzzle.round++].back());
                FillRound(&puzzle);
            }
            else
            {
                // Never reached round 7, so this candidate can be abandoned...
                candidatesConsidered++;
                PopRound(&puzzle);
            }
        }
        else
        {
            // Round 7...all possibles are valid!
            while (puzzle.possibles[puzzle.round].size() > 0)
            {
                // Probable solution!
                candidatesConsidered++;
                puzzle.candidate.push_back(puzzle.possibles[puzzle.round].back());

                // Check the final condition (that swapping the 6th and 7th rounds is a valid solution)
                // before adding this candidate to the list of solution
                if (IsSwappingRoundsValid(&puzzle))
                {
                    // Valid solution. Add a final value, which is the number of remaining
                    puzzle.solutions.push_back(puzzle.candidate);
                }

                // Unwind and continue with the next possible value for round 7
                puzzle.possibles[puzzle.round].pop_back();
                puzzle.candidate.pop_back();
            }
            PopRound(&puzzle);
        }
    }

    if (puzzle.solutions.size() > 0)
    {
        int solutionNum = 0;
        for (auto solution : puzzle.solutions)
        {
            std::cout << "Solution " << std::setw(2) << ++solutionNum << ": ";
            vectorHelp.PrintVector<int>(solution, 3);
            std::cout << "   (bricks left = " << GetRemainingBricks(&puzzle, &solution) << ")\n";
        }
    }

    return (int)puzzle.solutions.size();
}
