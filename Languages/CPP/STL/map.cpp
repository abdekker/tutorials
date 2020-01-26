#include <iostream>
#include <map>
#include <string>
#include <iterator>
 
int main()
{
    // This tutorial is for the std::map data structure:
    // * http://www.cplusplus.com/reference/map/map/map/
    // * https://en.cppreference.com/w/cpp/container/map

    // Inserting data in std::map
    std::map<std::string, int> mapOfWords;
    mapOfWords.insert(std::make_pair("earth", 1));
    mapOfWords.insert(std::make_pair("moon", 2));
    mapOfWords["sun"] = 3;

    // Will replace the value of already added key i.e. earth
    mapOfWords["earth"] = 4;    // mapOfWords["earth"] was 1, now 4

    // Number of items in the map
    std::cout << "Map contains " << mapOfWords.size() << " items\n\n";

    // Iterate through all elements in std::map
    std::cout << "Iterate through items" << std::endl;
    std::map<std::string, int>::iterator it = mapOfWords.begin();
    while (it != mapOfWords.end())
    {
        std::cout << it->first << " :: " << it->second << std::endl;
        it++;
    }
    std::cout << std::endl;

    // Alternative method to iterate through items
    std::cout << "Alternate code to iterate through items" << std::endl;
    for (auto& elem : mapOfWords)
    {
        std::cout << elem.first << " :: " << elem.second << std::endl;
    }
    std::cout << std::endl;

    // Check if insertion is successful or not
    if (mapOfWords.insert(std::make_pair("earth", 1)).second == false) {
        std::cout << "Element with key 'earth' was not inserted because it already exists\n\n";
    }

    // Searching element in std::map by key
    if (mapOfWords.find("sun") != mapOfWords.end())
        std::cout << "'sun' found" << std::endl;
    if (mapOfWords.find("mars") == mapOfWords.end())
        std::cout << "'mars' not found" << std::endl;

    return 0;
}
