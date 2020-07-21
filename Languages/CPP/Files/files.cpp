#include <iostream>
#include <fstream>

#include <string>

// Random filename for testing
#define LOCAL_FILE              "RandomFile_az19[tmp-exist].txt"
#define LOCAL_FILE_NO_EXIST     "RandomFile_az19[never-exist].txt"

// Example files (avoid deleting these)
#define EXAMPLE_FILE_EMPTY      "ExampleEmpty.xyz"
#define EXAMPLE_FILE_ICO        "ExampleICO.ico"
#define EXAMPLE_FILE_TXT        "ExampleTXT.txt"

// Adapted from: http://www.cplusplus.com/doc/tutorial/files/
using namespace std;
void FileWriteText_ofstream()
{
    cout << "\n### Writing to a text file using std::ofstream ###\n";

    cout << "  \"open (filename, mode):\n";
    cout << "      filename = const wchar_t* (name of file)\n";
    cout << "      mode = ios_base::openmode (bitmask for opening flags)\n";
    cout << "        ios::in\t\tOpen for input\n";
    cout << "        ios::out\tOpen for output (default for std::ofstream)\n";
    cout << "        ios::binary\tOpen in binary mode (otherwise a text file)\n";
    cout << "        ios::ate\tSet the initial position at the end of the file (othwerwise at start)\n";
    cout << "        ios::app\tAll output operations are done at the end of the file\n";
    cout << "        ios::trunc\tIf file already existed, previous contents are deleted and replaced\n";

    // Open file and write one line (previous contents are erased)
    ofstream myFile(LOCAL_FILE);
    myFile << "Hello ofstream!\n";
    myFile.close();

    // Open file and append another line
    myFile.open(LOCAL_FILE, ios::out | ios::app);
    myFile << "Lovely roses\n";
    myFile.close();

    cout << "  (" << LOCAL_FILE << " written to disk)\n";
    cout << "#\n";
}

void FileWriteText_fstream()
{
    cout << "\n### Writing to a text file using std::fstream ###\n";
    cout << "  (Similar to std::ofstream, but default mode is ios::in | ios::out)\n";
    cout << "  (std::ofstream and std::ifstream are more straightforward)\n";

    // Open file and write one line (previous contents are overwritten, but not erased)
    fstream myFile(LOCAL_FILE);
    myFile << "Hello fstream!\n";
    myFile.close();

    // Open file and append another line
    myFile.open(LOCAL_FILE, ios::out | ios::app);
    myFile << "Fred loves ice cream";
    myFile.close();

    cout << "  (" << LOCAL_FILE << " written to disk)\n";
    cout << "#\n";
}

void FileReadText_ifstream()
{
    cout << "\n### Reading from a text file using std::ifstream ###\n";

    // Attempt to open a file that does not exist
    ifstream myFile(LOCAL_FILE_NO_EXIST);
    if (myFile.is_open())
        myFile.close();
    else
        cout << "Unable to open file: " << LOCAL_FILE_NO_EXIST << '\n';

    // Open file which does exist (because of earlier calls to "TestWrite_XXX")
    cout << "\n(Read contents, line by line)\n";
    string szLine;
    int nLine = 0;
    myFile.open(LOCAL_FILE);
    if (myFile.is_open())
    {
        while (getline(myFile, szLine))
        {
            cout << "Line " << ++nLine << ": " << szLine << '\n';
        }
        myFile.close();
    }
    else
        cout << "Unable to open file: " << LOCAL_FILE << '\n';

    cout << "\n(Read contents, character by character)\n";
    char cInput;
    myFile.open(LOCAL_FILE);
    if (myFile.is_open())
    {
        while (!myFile.eof())
        {
            myFile.read(&cInput, 1);
            cout << cInput;
        }
        cout << endl;
        myFile.close();
    }
    else
        cout << "Unable to open file: " << LOCAL_FILE << '\n';

    cout << "#\n";
}

void FileReadText_FILE()
{
    cout << "\n### Reading from a text file using stdio::FILE ###\n";

    // Attempt to open a file that does not exist
    FILE *pFile = fopen(LOCAL_FILE_NO_EXIST , "r");
    if (pFile)
        fclose(pFile);
    else
        cout << "Unable to open file: " << LOCAL_FILE_NO_EXIST << '\n';

    // Open file which does exist (because of earlier calls to "TestWrite_XXX")
    cout << "\n(Read contents, character by character)\n";
    pFile = fopen(LOCAL_FILE, "r");
    char buffer[100];
    if (pFile)
    {
        while (!feof(pFile))
        {
            if (fgets(buffer, 100, pFile) == NULL)
                break;

            fputs(buffer, stdout);
        }
        cout << '\n';
        fclose(pFile);
    }
    else
        cout << "Unable to open file: " << LOCAL_FILE << '\n';

    cout << "#\n";
}

void FileSizeText()
{
    cout << "\n### Determine size of text file ###\n";
    cout << "  (ifstream::seekg sets the file \"get\" position [ios::cur])\n";
    cout << "  (ifstream::tellg returns the file \"get\" position [offset from ios:beg])\n\n";

    cout << "  (ofstream::seekp sets the file \"put\" position)\n";
    cout << "  (ofstream::tellp returns the file \"put\" position)\n";

    // // Open file which was created with earlier calls to "TestWrite_XXX"
    ifstream myFile(LOCAL_FILE);
    if (myFile.is_open())
    {
        streampos fBegin = myFile.tellg();
        myFile.seekg(0, ios::end);
        streampos fEnd = myFile.tellg();
        myFile.close();
        cout << "  File size is: " << (fEnd - fBegin) << " bytes\n";
    }
    else
        cout << "  Error opening file\n";

    cout << "#\n";
}

void FileDelete()
{
    cout << "\n### Deleting a file ###\n";

    // Attempt to delete a file that does not exist (should return an error)
    if (remove(LOCAL_FILE_NO_EXIST) != 0)
        perror("  Error deleting file");
    else
        puts("  File successfully deleted!");

    // Delete file which does exist (because of earlier calls to "TestWrite_XXX")
    if (remove(LOCAL_FILE) != 0)
        perror("  Error deleting file");
    else
        puts("  File successfully deleted!");
    
    cout << "#\n";
}

void FileSizeBinary()
{
    cout << "\n### Read a binary file into memory and display the file size ###\n";

    // Note: Avoid deleting the example files in this folder
    // * ExampleEmpty.xyz (EXAMPLE_FILE_EMPTY)
    // * ExampleICO.ico (EXAMPLE_FILE_ICO)
    // * ExampleTXT.txt (EXAMPLE_FILE_TXT)

    // Open a file and display (some) of its' contents as binary (change filename as required)
    ifstream myFile(EXAMPLE_FILE_ICO, ios::in | ios::binary | ios::ate);
    if (myFile.is_open())
    {
        streampos fSize = myFile.tellg();       // Determine the file size
        if (fSize != 0)
        {
            char* memblock = new char[fSize];
            myFile.seekg(0, ios::beg);          // Reset file position to the beginning of the stream
            myFile.read(memblock, fSize);       // Read the entire file into memory and close
            myFile.close();

            cout << "Entire file content are now in memory (size = " << fSize << " bytes). First 10 bytes are:\n  ";
            int posMax = (fSize > 10) ? 10 : (int)fSize;
            for (int pos = 0; pos < posMax; pos++)
                cout << "[" << (int)memblock[pos] << "]";    

            cout << endl;

            // Clean up
            delete[] memblock;
        }
        else
            cout << "File is empty!\n";
    }
    else
        cout << "Unable to open file\n";

    cout << "#\n";
}

int main()
{
    // Ensure the output buffer is flushed on each insertion operation
    setvbuf(stdout, NULL, _IONBF, 0);

    // Demonstrates simple file access in C++
    cout << "### Simple file access ###\n\n";

    cout << "C++ provides the following classes for file I/O:\n";
    cout << "  ofstream:\tStream class to write to files\n";
    cout << "  ifstream:\tStream class to read from files\n";
    cout << "  fstream:\tStream class to both read and write from/to files.\n";

    FileWriteText_ofstream();
    FileWriteText_fstream();
    FileReadText_ifstream();
    FileReadText_FILE();
    FileSizeText();
    FileDelete();

    FileSizeBinary();

    cout << "\nAll done!\n";
    return 0;
}
