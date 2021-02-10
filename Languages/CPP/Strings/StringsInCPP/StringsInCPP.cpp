#include <conio.h>
#include <iostream>
#include <sstream>      // For std::stringstream
#include <codecvt>      // For char16_t and char32_t conversions
#include <iomanip>      // For std::setw

#include <vector>

#include "..\..\Utils\stringHelper.h"

using namespace std;
stringHelper g_sHelper;

void ConstructStdString()
{
    // Various ways to construct a std::string
    // Note: Some of these examples may generate reports (including false positives) of memory
    // leaks in tools such as valgrind. These may be related to how the memory is managed internally
    // with the standard library.
    cout << "### Construct a std::string ###\n";

    {
        string s = "Hello std::string 1";
        cout << "  Operator= with string literal\t\t" << s << "\n";
    }

    {
        string s1 = "Hello std::string 2";
        string s2 = s1;
        cout << "  By simple assignment\t\t\t" << s2 << "\n";
    }

    {
        string s("Hello std::string 3");
        cout << "  From string literal\t\t\t" << s << "\n";
    }

    {
        const char* c = "Hello std::string 4a";
        string s(c);
        cout << "  From char*\t\t\t\t" << s << "\n";
    }

    {
        const char* c = "Hello std::string 4b";
        size_t len = strlen(c);
        string s(c, len);        // To remove the last two characters (as an example), use "len-2"
        cout << "  From char*\t\t\t\t" << s << "\n";
    }

    {
        const char* c = "Hello std::string 4c";
        string s = c;    // Direct assignment
        cout << "  From char*\t\t\t\t" << s << "\n";
    }

    {
        const char* c = "Hello std::string 5";
        size_t len = strlen(c);
        string s;
        s.assign(c, len);    // Only if the string already exists!
        cout << "  Using std::string::assign\t\t" << s << "\n";
    }

    {
        const char* c = "Hello std::string 6";
        string s;
        s.append(c);
        cout << "  Using std::string::append\t\t" << s << "\n";
    }

    {
        const char* c1 = "Hello ";
        const char* c2 = "std::string 7"; 
        string s = string(c1) + string(c2);
        cout << "  Using std::string::operator+\t\t" << s << "\n";
    }

    {
        const char* c = "Hello std::string 8";
        stringstream myStream;
        myStream << c;
        string s = myStream.str();
        cout << "  Using std::stringstream\t\t" << s << "\n";
    }

    {
        cout << "  Using std::snprintf (and variadic templates)\n";
        string myString = "Hello";
        int myInt = 123;
        double myDouble = 456.789;
        string s = g_sHelper.FormatString("    myString=%s, myInt=%05d, myDouble=%10.5f",
            myString.c_str(), myInt, myDouble);
        cout << s << "\n";
    }
    cout << "#\n\n";
}

void ConstructStdStringFromVector()
{
    // Construct a std::string from a vector<char>
    cout << "### Construct a std::string from std::vector<char> ###\n";

    {
        vector<char> input({ 'H', 'e', 'l', 'l', 'o', ' ', 's', 't', 'd', ':', ':', 'v', 'e', 'c', 't', 'o', 'r', ' ', '1' });
        string s(input.begin(), input.end());
        cout << "  Using std::vector::begin / end iterators\t" << s << "\n";
    }

    {
        vector<char> input({ 'H', 'e', 'l', 'l', 'o', ' ', 's', 't', 'd', ':', ':', 'v', 'e', 'c', 't', 'o', 'r', ' ', '2' });
        ostringstream out;
        for (char c: input) {
            out << c;
        }
 
        string s(out.str());
        cout << "  Using std::ostringstream::str\t\t\t" << s << "\n";
    }

    {
        vector<char> input({ 'H', 'e', 'l', 'l', 'o', ' ', 's', 't', 'd', ':', ':', 'v', 'e', 'c', 't', 'o', 'r', ' ', '3' });
        string s;
        transform(input.begin(), input.end(), back_inserter(s),
            [](char c) { return c; });
        cout << "  Using std::transform with lambda\t\t" << s << "\n";
    }

    {
        vector<char> input({ 'H', 'e', 'l', 'l', 'o', ' ', 's', 't', 'd', ':', ':', 'v', 'e', 'c', 't', 'o', 'r', ' ', '4' });
        std::string s;
        for (char c: input)
            s.push_back(c);

        cout << "  Using std::string::push_back\t\t\t" << s << "\n";
    }
    
    {
        vector<char> input({ 'H', 'e', 'l', 'l', 'o', ' ', 's', 't', 'd', ':', ':', 'v', 'e', 'c', 't', 'o', 'r', ' ', '5' });
        std::string s;
        for (char c: input)
            s += c;

        cout << "  Using std::string::+=operator\t\t\t" << s << "\n";
    }
    
    {
        vector<char> input({ 'H', 'e', 'l', 'l', 'o', ' ', 's', 't', 'd', ':', ':', 'v', 'e', 'c', 't', 'o', 'r', ' ', '6' });
        std::string s;
        for (char c: input)
            s.append(1, c);

        cout << "  Using std::string::append\t\t\t" << s << "\n";
    }
    
    {
        vector<char> input({ 'H', 'e', 'l', 'l', 'o', ' ', 's', 't', 'd', ':', ':', 'v', 'e', 'c', 't', 'o', 'r', ' ', '7' });
        std::string s;
        for (unsigned i = 0; i < input.size(); i++)
            s.insert(i, 1, input[i]);

        cout << "  Using std::string::insert\t\t\t" << s << "\n";
    }
    cout << "#\n\n";
}

void ConstructOtherStdStrings()
{
    // Other std string types
    cout << "### Construct other std string types (u16string, u32string, wstring) ###\n";
    cout << "  Note: Conversions between char16_t and char32_t require features from C++14/17\n\n";

    {
#if (_MSVC_LANG < 201703)
        wstring_convert<codecvt_utf8_utf16<char16_t>, char16_t> utf16conv;
        u16string s = u"Hello std::u16string"s;
        cout << "  Using std::wstring_convert\t\t" << utf16conv.to_bytes(s) << " [ std::u16string s = u\"TEXT\"s; ]\n";
#else
        cout << "  (std::codecvt_utf8_utf16 deprecated in C++17. TODO: Convert to MultiByteToWideChar and WideCharToMultiByte.)\n";
#endif
    }

    {
#if (_MSVC_LANG < 201703)
        wstring_convert<codecvt_utf8_utf16<char32_t>, char32_t> utf32conv;
        u32string s = U"Hello std::u32string"s;
        cout << "  Using std::wstring_convert\t\t" << utf32conv.to_bytes(s) << " [ std::u32string s = U\"TEXT\"s; ]\n";
#else
        cout << "  (std::codecvt_utf8_utf16 deprecated in C++17. TODO: Convert to MultiByteToWideChar and WideCharToMultiByte.)\n";
#endif
    }

    {
        wstring s = L"Hello std::wstring"s;
        wcout << "  Using std::wcout\t\t\t" << s << "   [ std::wstring s = L\"TEXT\"s; ]\n";
    }
    cout << "#\n\n";
   }

void ConstructCharPointers()
{
    // Various ways to construct a char* from std::string
    cout << "### Construct a char* from a std::string ###\n";

    {
        string s = "Hello char* 1a";
        char c[20];
        strcpy_s(c, s.c_str());
        printf("  Using std::string::c_str\t\t%s\n", c);
    }

    {
        string s = "Hello char* 1b";
        char c[20];
        strcpy_s(c, &s[0]);
        printf("  Using address of std::string[0]\t%s\n", c);
    }

    {
        string s = "Hello char* 2";
        char c[20];
        s.copy(c, s.max_size() + 1);
        c[s.size()] = '\0';
        printf("  Using std::string::copy\t\t%s\n", c);
    }

    {
        string s = "Hello char* 3";
        char c[20];
        copy(s.begin(), s.end(), c);
        c[s.size()] = '\0';
        printf("  Using std::copy\t\t\t%s\n", c);
    }
    cout << "#\n\n";
}

void FormattingStdString()
{
    // Various ways to format std::string output
    cout << "### Formatting a std::string ###\n";

    cout << "(integer)\n";
    int myInt = 123;
    {
        cout << "  Left padding (std::setw)\t\t\t";
        cout << "|" << setw(10) << myInt << "|\n";
    }

    {
        cout << "  Right padding (std::left, std::setw)\t\t";
        cout << "|" << left << setw(10) << myInt << "|\n";
    }

    {
        cout << "  Left padding (std::snprintf, 10d)\t\t";
        cout << g_sHelper.FormatString("|%10d|\n", myInt);
    }

    {
        cout << "  Right padding (std::snprintf, -10d)\t\t";
        cout << g_sHelper.FormatString("|%-10d|\n", myInt);
    }

    {
        cout << "  Left padding zeroes (std::snprintf, 010d)\t";
        cout << g_sHelper.FormatString("|%010d|\n", myInt);
    }

    cout << "\n(float)\n";
    float myFloat = 1.2345f;
    {
        cout << "  Left padding (std::snprintf, 10.2f)\t\t";
        cout << g_sHelper.FormatString("|%10.2f|\n", myFloat);
    }

    {
        cout << "  Right padding (std::snprintf, -10.2f)\t\t";
        cout << g_sHelper.FormatString("|%-10.2f|\n", myFloat);
    }

    {
        cout << "  Left padding zeroes (std::snprintf, 010.2f)\t";
        cout << g_sHelper.FormatString("|%010.2f|\n", myFloat);
    }
    cout << "#\n\n";
}

void MiscellaneousStrings()
{
    cout << "### Miscellaneous string adventures ###\n";

    {
        uint8_t randomLength = 40;
        cout << g_sHelper.FormatString("  Randomised strings, length = %d\n", randomLength);
        cout << g_sHelper.FormatString("    Letters only\t\t%s\n", g_sHelper.GetRandomString(randomLength, true));
        cout << g_sHelper.FormatString("    Any printable\t\t%s\n", g_sHelper.GetRandomString(randomLength, false));
        cout << g_sHelper.FormatString("    Remove illegal\t\t%s\n", g_sHelper.GetRandomString(randomLength, false, true));
    }
    cout << "#\n\n";
}

int main()
{
    cout << "Simple string manipulations in C++\n\n";

    // Construct a std::string
    ConstructStdString();
    ConstructStdStringFromVector();

    // Other std::string types (eg. std::u16string)
    ConstructOtherStdStrings();

    // Construct char* from std::string
    ConstructCharPointers();

    // Formatting a std::string
    FormattingStdString();

    // Miscellaneous string demos
    MiscellaneousStrings();

    cout << "\nAll done...press a key to exit\n";
    _getch();
}
