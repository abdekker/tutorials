#include <iostream>
#include <sstream>      // For std::stringstream
#include <codecvt>      // For char16_t and char32_t conversions
#include <iomanip>      // For std::setw

#include "..\..\Utils\stringHelper.h"

using namespace std;
void ConstructStdString()
{
    // Various ways to construct a std::string
    // Note: Some of these examples may generate reports (including false positives) of memory
    // leaks in tools such as valgrind. These may be related to how the memory is managed internally
    // with the standard library.
    cout << "Construct a std::string\n";

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
        string s(c, len);		// To remove the last two characters (as an example), use "len-2"
        cout << "  From char*\t\t\t\t" << s << "\n";
    }

    {
        const char* c = "Hello std::string 4c";
        string s = c;	// Direct assignment
        cout << "  From char*\t\t\t\t" << s << "\n";
    }

    {
        const char* c = "Hello std::string 5";
        size_t len = strlen(c);
        string s;
        s.assign(c, len);	// Only if the string already exists!
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
		stringHelper sHelper;
		string s = sHelper.formatString("    myString=%s, myInt=%05d, myDouble=%10.5f",
			myString.c_str(), myInt, myDouble);
		cout << s << "\n";
	}
    cout << endl;
}

void ConstructOtherStdStrings()
{
    // Other std string types
    cout << "Construct other std string types (u16string, u32string, wstring)\n";
    cout << "  Note: Conversions between char16_t and char32_t require features from C++14/17\n\n";

    {
        wstring_convert<codecvt_utf8_utf16<char16_t>, char16_t> utf16conv;
        u16string s = u"Hello std::u16string"s;
        cout << "  Using std::wstring_convert\t\t" << utf16conv.to_bytes(s) << " [ std::u16string s = u\"TEXT\"s; ]\n";
    }

    {
        wstring_convert<codecvt_utf8_utf16<char32_t>, char32_t> utf32conv;
        u32string s = U"Hello std::u32string"s;
        cout << "  Using std::wstring_convert\t\t" << utf32conv.to_bytes(s) << " [ std::u32string s = U\"TEXT\"s; ]\n";
    }

    {
        wstring s = L"Hello std::wstring"s;
        wcout << "  Using std::wcout\t\t\t" << s << "   [ std::wstring s = L\"TEXT\"s; ]\n";
    }
    cout << endl;
   }

void ConstructCharPointers()
{
	// Various ways to construct a char* from std::string
	cout << "Construct a char* from a std::string\n";

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
	cout << endl;
}

void FormattingStdString()
{
    // Various ways to format std::string output
	cout << "Formatting a std::string\n";
    int myInt = 123;

    {
        cout << "  Left padding (std::setw)\t\t";
        cout << "!" << setw(10) << myInt << "!\n";
    }

    {
        cout << "  Right padding (std::left, std::setw)\t";
        cout << "!" << left << setw(10) << myInt << "!\n";
    }

    {
        cout << "  Left padding (std::snprintf, %10d)\t";
        stringHelper sHelper;
        cout << sHelper.formatString("!%10d!\n", myInt);
    }

    {
        cout << "  Right padding (std::snprintf, %-10d)\t";
        stringHelper sHelper;
        cout << sHelper.formatString("!%-10d!\n", myInt);
    }

    {
        cout << "  Left padding spaces (std::snprintf)\t";
        stringHelper sHelper;
        cout << sHelper.formatString("!%010d!\n", myInt);
    }
}

int main()
{
    cout << "Simple string manipulations in C++\n\n";

    // Construct a std::string
    ConstructStdString();

    // Other std string types (eg. std::u16string)
    ConstructOtherStdStrings();

    // Construct char* from std::string
    ConstructCharPointers();

    // Formatting a std::string
    FormattingStdString();

    cout << "\nAll Done!\n";
}
