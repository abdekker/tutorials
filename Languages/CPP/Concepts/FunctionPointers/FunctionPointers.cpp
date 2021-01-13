#include <conio.h>
#include <iostream>

class cat
{
public:
    void walk1() { std::cout << "cat is walking 1\n"; }
    void walk2() { std::cout << "cat is walking 2\n"; }
    void walk3(int w) { std::cout << "cat is on walk number " << w << std::endl; }
    int walk4() { std::cout << "cat is walking 4 (and returning 4)\n"; return 4; }
    int walk5(int w) { std::cout << "cat is on walk number " << w << " (and returning 5)\n"; return 5; }
};

int main()
{
    setvbuf(stdout, NULL, _IONBF, 0);
    std::cout << "Function pointers in C++\n\n";
    cat bigCat;

    // Function prototype matching cat::walk1 and cat::walk2
    void (cat::*pCatVoidVoid)();
    pCatVoidVoid = &cat::walk1;
    (bigCat.*pCatVoidVoid)();
    pCatVoidVoid = &cat::walk2; // Change to different method with the same prototype
    (bigCat.*pCatVoidVoid)();

    // cat::walk3
    void (cat::*pCatVoidInt)(int w);
    pCatVoidInt = &cat::walk3;
    (bigCat.*pCatVoidInt)(3);

    // cat::walk4
    int (cat::*pCatIntVoid)();
    pCatIntVoid = &cat::walk4;
    int r1 = (bigCat.*pCatIntVoid)();
    std::cout << "  (returned " << r1 << ")\n";

    // cat::walk5
    int (cat::*pCatIntInt)(int w);
    pCatIntInt = &cat::walk5;
    int r2 = (bigCat.*pCatIntInt)(5);
    std::cout << "  (returned " << r2 << ")\n";

    std::cout << "\nAll done...";
    _getch();
    return 0;
}
