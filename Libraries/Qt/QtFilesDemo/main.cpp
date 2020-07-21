#include <conio.h>
#include <iostream>
#include <algorithm>

#include <QtCore/QCoreApplication>
#include <QDir>
#include <QDebug>

#include "..\Utils\qstringHelper.h"

// Example files (avoid deleting these)
#define EXAMPLE_FILE_EMPTY      "ExampleEmpty.xyz"
#define EXAMPLE_FILE_ICO        "ExampleICO.ico"
#define EXAMPLE_FILE_TXT        "ExampleTXT.txt"

using namespace std;
QString CleanAppPath(QCoreApplication &a)
{
    // Helper function that "cleans the application" path, removing folders reported during debugging. For
    // example, run the application directly and QCoreApplication::applicationDirPath is something like:
    //      <DEV-PAT>\QtFilesDemo
    // But now run it from the debugger, and the path will be similar to this:
    //      <DEV-PAT>\QtFilesDemo\x64\Debug

    // This method returns the former.
    QString fullPath = a.applicationDirPath();
    int posAppName = fullPath.indexOf(a.applicationName());
    QString cleanPath = (posAppName != -1)
        ? fullPath.mid(0, posAppName + a.applicationName().length())
        : fullPath;

    return cleanPath;
}

void TestQCoreApplication(QCoreApplication &a)
{
    cout << "\n### QCoreApplication ###\n";

    // Note: New "Qt Console Application" projects have a QCoreApplication object created by default. This can be
    // used to provide an event loop for non-GUI applications. It is started with "exec". It is not clear how to
    // use it, so this method demonstrates some minor features of the object.
    cout << "  Application name:\t" << a.applicationName().toLatin1().data() << endl;
    cout << "  App dir path:\t\t" << a.applicationDirPath().toLatin1().data() << endl;
    cout << "  App \"clean\" path:\t" << CleanAppPath(a).toLatin1().data() << endl;
    cout << "  App process ID:\t" << a.applicationPid() << endl;
    cout << "  App version:\t\t" << a.applicationVersion().toLatin1().data() << endl;
    cout << "  App object name:\t" << a.objectName().toLatin1().data() << endl;
    cout << "#\n";
}

void TestQDir()
{
    cout << "\n### QDir ###\n";
    cout << "  Home path:\t\t" << QDir::homePath().toLatin1().data() << endl;

    cout << "\n  System drives\n";
    QFileInfoList sysDrives = QDir::drives();
    int driveNum = 1;
    for (auto drive : sysDrives) {
        // Note: QFileInfo::absoluteFilePath() == QFileInfo::absolutePath() because there is no filename
        cout << "    drive " << driveNum++ << ": " << drive.absoluteFilePath().toLatin1().data() << endl;
    }

    cout << "\n(current application path)\n";
    QDir dir(QCoreApplication::applicationDirPath());
    cout << "  Directory path:\t" << dir.absolutePath().toLatin1().data() << endl;
    cout << "  File path:\t\t" << dir.absoluteFilePath("abc.xyz").toLatin1().data() << endl;
    // Note: QDir::absoluteFilePath does not check for file existence (just creates a string representing the path)

    cout << "\n(moving to C:\\Windows)\n";
    if (dir.cd(qgetenv("windir"))) {
        cout << "  Directory path:\t" << dir.absolutePath().toLatin1().data() << endl;
        cout << "  There are " << dir.count() << " files and folders in this directory (not recursive)\n";

        QFileInfoList listDirs = dir.entryInfoList(QDir::Filter::Dirs, QDir::SortFlag::Name);
        cout << "  There are " << listDirs.count() << " folders (not recursive). First few are:\n";
        for (int dirNum = 0; dirNum < std::min(5, listDirs.count()); dirNum++) {
            if (!listDirs[dirNum].fileName().contains('.'))
                cout << "    " << listDirs[dirNum].fileName().toLatin1().data() << endl;
        }
    }
    else
        cout << "  Failed to change path!\n";

    cout << "\n(moving to User's HOME folder)\n";
    dir.cd(QDir::home().absolutePath());
    cout << "  Directory path:\t" << dir.absolutePath().toLatin1().data() << endl;

    cout << "\n(moving to C:\\Tmp)\n";
    dir = QDir("C:\\Tmp");
    cout << "  Directory path:\t" << dir.absolutePath().toLatin1().data() << endl;

    QStringHelper helper;
    QString sRandom1 = helper.GetRandomString(20);
    cout << "  Check for random folder...";
    if (dir.exists(sRandom1))
        cout << "folder exists (oops!)\n";
    else
        cout << "folder does not exist as expected\n";

    cout << "(creating random folder \"" << sRandom1.toLatin1().data() << "\"...";
    if (dir.mkdir(sRandom1))
        cout << "done!)\n";
    else
        cout << "failed)\n";

    dir.cd(sRandom1);
    if (dir.isEmpty())
        cout << "  Folder is empty as expected\n";
    else
        cout << "  Folder is not empty which is weird!\n";

    dir.cdUp();
    QString sRandom2 = helper.GetRandomString(20);
    cout << "(renaming random folder...";
    if (dir.rename(sRandom1, sRandom2))
        cout << "done!)\n";
    else
        cout << "failed)\n";

    cout << "(deleting random folder...";
    if (dir.rmdir(sRandom2))
        cout << "done!)\n";
    else
        cout << "failed)\n";

    // Other options include:
    // * remove(const QString &fileName) - deletes the given file
    // * bool removeRecursively() - removes the directory, including all contents

    cout << "#\n";
}

void TestQFile(QCoreApplication &a)
{
    cout << "\n### QFile ###\n";
    cout << "(check for existence of example files)\n";

    // Get the application path where example files (should) be located
    QString appPath = (CleanAppPath(a) + QDir::separator());

    // Generate a list of example files
    QStringList examplesFileOnly, examplesFullPath;
    examplesFileOnly.push_back(EXAMPLE_FILE_EMPTY);
    examplesFileOnly.push_back(EXAMPLE_FILE_ICO);
    examplesFileOnly.push_back(EXAMPLE_FILE_TXT);
    for (auto example : examplesFileOnly)
        examplesFullPath.push_back(appPath + example);

    for (int fileNum=0; fileNum < examplesFullPath.size(); fileNum++) {
        QString test = examplesFullPath[fileNum];
        if (QFile::exists(examplesFullPath[fileNum]))
            cout << "  \"" << examplesFileOnly[fileNum].toLatin1().data() << "\" exists!\n";
        else
            cout << "  \"" << examplesFileOnly[fileNum].toLatin1().data() << "\" does not exist\n";
    }

    cout << "\n(open each file and display a few bytes)\n";
    FILE* f
    QFile myFile;
    QByteArray ba;
    for (int fileNum=0; fileNum < examplesFullPath.size(); fileNum++) {
        QFile::open(
        myFile.open(examplesFullPath[fileNum], QIODevice::ReadOnly);
        if (myFile.isOpen()) {
            cout << "  \"" << examplesFileOnly[fileNum].toLatin1().data() << "\" has a size of " << myFile.size() << " bytes\n";
            //myFile.read(c, 1
        }

        myFile.close();
    }
    //QFile myFile;
    //QByteArray(
    //char* c;
    //
    //myFile.open(examplesFullPath[0], QIODevice::ReadOnly);
    //if (myFile.isOpen()) {
    //    myFile.size();
    //}
    //while (!myFile.atEnd()) {
    //    myFile.read(c, 1
    //
    //myFile.write(msg, qstrlen(msg));        // write to stderr
    //file.close();
    //
    //

    /*
    // Example files (avoid deleting these)
#define EXAMPLE_FILE_EMPTY      "ExampleEmpty.xyz"
#define EXAMPLE_FILE_ICO        "ExampleICO.ico"
#define EXAMPLE_FILE_TXT        "ExampleTXT.txt"
    */
    cout << "#\n";
}

int main(int argc, char *argv[])
{
    cout << "### File I/O in Qt ###\n";

    QCoreApplication a(argc, argv);
    TestQCoreApplication(a);
    TestQDir();
    TestQFile(a);

    // Exit
    a.quit();
    cout << "\nAll done! Press a key to exit...\n";
    _getch();
    return 0;
}
