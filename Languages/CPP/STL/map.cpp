#include <iostream>
#include <conio.h>
#include <map>
#include <string>
#include <iterator>

// Forward declare helper functions (at bottom of file)
template <typename T>
void ReportIfMapContainsKey(T localMap, std::string item);

void basicMap()
{
    // Inserting data in std::map
    std::cout << "### Basic tutorial of 'std::map<std::string, int>' ###\n";
    std::map<std::string, int> mapOfWords;
    mapOfWords.insert(std::make_pair("earth", 1));
    mapOfWords.insert(std::make_pair("moon", 2));
    mapOfWords["sun"] = 3;

    // Number of items in the map
    std::cout << "\nMap contains " << mapOfWords.size() << " items\n";
    
    // Iterate through all elements in std::map
    std::cout << "\nIterate through items with 'std::map::iterator'\n";
    std::map<std::string, int>::iterator it = mapOfWords.begin();
    while (it != mapOfWords.end())
    {
        std::cout << "  " << it->first << " :: " << it->second << std::endl;
        it++;
    }

    // Alternative method to iterate through items (requires C++11)
    std::cout << "\nAlternate code to iterate through items with 'for (auto& elem : map)'\n";
    for (auto& elem : mapOfWords)
    {
        std::cout << "  " << elem.first << " :: " << elem.second << std::endl;
    }

    // Replace the value of an existing key
    std::cout << "\nChange the mapping (value) of an existing key\n";
    int oldValue = mapOfWords["earth"];
    mapOfWords["earth"] = 4;    // mapOfWords["earth"] was 1, now 4
    std::cout << "  Old value for key 'earth' was " << oldValue << ", now it is " << mapOfWords["earth"] << std::endl;
    it = mapOfWords.begin();
    while (it != mapOfWords.end())
    {
        std::cout << "  " << it->first << " :: " << it->second << std::endl;
        it++;
    }

    // Check if insertion is successful (this fails because keys must be unique)
    std::cout << "\nAttempt to insert a key with the same name as an existing key\n";
    if (!mapOfWords.insert(std::make_pair("earth", 5)).second) {
        std::cout << "  Element with key 'earth' was not inserted because it already exists\n";
    }

    // Searching for elements by key
	std::cout << "\nSearch for elements by key using 'std::map::find'\n";
    ReportIfMapContainsKey<std::map<std::string, int> >(mapOfWords, "sun");
    ReportIfMapContainsKey<std::map<std::string, int> >(mapOfWords, "mars");

    // Clear the map
    mapOfWords.clear();
    std::cout << "\nMap cleared, there are now " << mapOfWords.size() << " elements\n";
    std::cout << "#\n";
}
 
int main()
{
    // References for the std::map data structure:
    // * http://www.cplusplus.com/reference/map/map/map/
    // * https://en.cppreference.com/w/cpp/container/map

    // Tutorial adapted from https://thispointer.com/stdmap-tutorial-part-1-usage-detail-with-examples
    setvbuf(stdout, NULL, _IONBF, 0);

    // Basic map
    basicMap();

    // Prompt for exit
    std::cout << "\nFinished...press a key to exit\n";
    (void) _getch();
    return 0;
}

// Helper function(s)
template <typename T>
void ReportIfMapContainsKey(T localMap, std::string item) 
{
    // "localMap" must implement ::find (eg. std::map)
    if (localMap.find(item) != localMap.end())
        std::cout << "  '" << item << "' found" << std::endl;
    else
        std::cout << "  '" << item << "' NOT found" << std::endl;
} 
