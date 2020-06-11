#include <iostream>
#include <conio.h>

// Uncomment the version you want to test (only one in each clase should be defined)
#define FORWARD_DECLARE_FUNCTION_WORKS

//#define FORWARD_DECLARE_CLASS_FAIL1
//#define FORWARD_DECLARE_CLASS_FAIL2
#define FORWARD_DECLARE_CLASS_WORKS

#ifdef FORWARD_DECLARE_CLASS_WORKS
    #undef FORWARD_DECLARE_CLASS_FAIL1
    #undef FORWARD_DECLARE_CLASS_FAIL2
#endif

#ifdef FORWARD_DECLARE_CLASS_FAIL2
    #undef FORWARD_DECLARE_CLASS_FAIL1
#endif

using namespace std;

// Function
#ifdef FORWARD_DECLARE_FUNCTION_WORKS
    // Forward declaration of a function (bottom of file)
    int sumFunction(int, int);
#endif

// Class
#ifdef FORWARD_DECLARE_CLASS_FAIL1
// Version 1: Define B before A
class B
{
public: 
    int x; 
    void setData(int n) { x = n; }
    friend int sumClass(A, B); 
};

class A
{
public:
    int y; 
    void setData(int m) { y = m; }
    friend int sumClass(A, B); 
};
#endif // FORWARD_DECLARE_CLASS_FAIL1

#ifdef FORWARD_DECLARE_CLASS_FAIL2
// Version 2: Define A before B
class A
{
public:
    int y; 
    void setData(int m) { y = m; }
    friend int sumClass(A, B); 
};

class B
{
public: 
    int x; 
    void setData(int n) { x = n; }
    friend int sumClass(A, B); 
};
#endif // FORWARD_DECLARE_CLASS_FAIL1

#ifdef FORWARD_DECLARE_CLASS_WORKS
// Version 3: Forward declare both classes. Tells the compiler of the existence of an entity (A or B)
// before actually defining the entity. This version works!
class A;
class B;

class B
{
public: 
    int x; 
    void setData(int n) { x = n; }
    friend int sumClass(A, B); 
};

class A
{
public:
    int y; 
    void setData(int m) { y = m; }
    friend int sumClass(A, B); 
};
#endif // FORWARD_DECLARE_CLASS_WORKS

int sumClass(A m, B n) 
{
    return (m.y + n.x); 
}

int main(void)
{
    // This application explores the concept of forward declaration.
    // Adapted from: https://www.geeksforgeeks.org/what-are-forward-declarations-in-c

    // Check which conditional compilation flags are defined by uncommenting this section
 /*#ifdef FORWARD_DECLARE_FUNCTION_WORKS
    cout << "FORWARD_DECLARE_FUNCTION_WORKS!\n";
#endif

#if defined(FORWARD_DECLARE_CLASS_FAIL1)
    cout << "FORWARD_DECLARE_CLASS_FAIL1!\n";
#elif defined (FORWARD_DECLARE_CLASS_FAIL2)
    cout << "FORWARD_DECLARE_CLASS_FAIL2!!\n";
#elif defined (FORWARD_DECLARE_CLASS_WORKS)
    cout << "FORWARD_DECLARE_CLASS_WORKS!\n";
#endif*/

    cout << "### Forward declaration ###\n";
    cout << "  Simple function\n";
    cout << "    Sum is: " << sumFunction(2, 5) << endl << endl;
    // The above line will generate error C3861 [MSVC: identifier not found]

    cout << "  Class\n";
    B b; 
    A a; 
    a.setData(2);
    b.setData(5); 
    cout << "    Sum is: " << sumClass(a, b); 
    // The above line will generate errro C2061
    // * [MSVC: syntax error: identifier 'A'] (FORWARD_DECLARE_CLASS_FAIL1)
    // * [MSVC: syntax error: identifier 'B'] (FORWARD_DECLARE_CLASS_FAIL2)

    // Prompt for exit
    std::cout << "\nFinished...press a key to exit\n";
    (void) _getch();
    return 0;
}

int sumFunction(int a, int b)
{
    return (a + b);
}