//#include <windows.h>
#include <iostream>
#include <vector>
#include <string>

using namespace std;
int main()
{
    vector<string> msg {"Hello", "C++", "World", "from", "VS Code", "and the C++ extension!"};
    for (const string& word : msg)
    {
        cout << word << " ";
    }
    cout << endl;

    // Uncomment this and "#include <windows.h>" at the top to pop up a box (and see the output)
    //MessageBox( NULL, "hello friend!", "", MB_OK );
}
