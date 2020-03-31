#include <iostream>
#include <sstream>		// For std::stringstream
using namespace std;

void ConstructStdString()
{
	// Various ways to construct a std::string
	// Note: Some of these examples may generate reports (including false positives) of memory
	// leaks in tool such as valgrind. These may be related to how the memory is managed internally
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
		string s(c, len);		// Use "len-2" (eg) to remove the last two characters
		cout << "  From char*\t\t\t\t" << s << "\n";
	}

	{
		const char* c = "Hello std::string 4c";
		string s = c;	// Direct assignment
		cout << "  From char*\t\t\t\t" << s << "\n";
	}

	{
		string s;
		const char* c = "Hello std::string 5";
		size_t len = strlen(c);
		s.assign(c, len);	// Only if the string already exists
		cout << "  Using std::string::assign\t\t" << s << "\n";
	}

	{
		const char* c = "Hello std::string 6";
		std::string s;
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
		printf("  Using std::string::c_str\t\t%s\n", c);
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
		std::copy(s.begin(), s.end(), c);
		c[s.size()] = '\0';
		printf("  Using std::copy\t\t\t%s\n", c);
	}
	cout << endl;
}

int main()
{
    cout << "Simple string manipulations in C++\n\n";

	// Construct a std::string
	ConstructStdString();

	// Construct char* from std::string
	ConstructCharPointers();
}
