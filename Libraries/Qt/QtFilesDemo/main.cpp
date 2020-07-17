#include <conio.h>
#include <iostream>

#include <QtCore/QCoreApplication>
#include <QDir>
#include <QDebug>

#include "..\Utils\qstringHelper.h"

using namespace std;
void TestQCoreApplication(QCoreApplication &a)
{
    cout << "\n### QCoreApplication ###\n";

    // Note: New "Qt Console Application" projects have a QCoreApplication object created by default. This can be
    // used to provide an event loop for non-GUI applications. It is started with "exec". It is not clear how to
    // use it, so this method demonstrates some minor features of the object.
    cout << "  Application name:\t" << a.applicationName().toLatin1().data() << endl;
    cout << "  App dir path:\t\t" << a.applicationDirPath().toLatin1().data() << endl;
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
    QString sRandom = helper.GetRandomString(20);
    cout << "(creating random folder \"" << sRandom.toLatin1().data() << "\"...";
    if (dir.mkdir(sRandom))
        cout << "done!)\n";
    else
        cout << "failed)\n";

    dir.cd(sRandom);
    if (dir.isEmpty())
        cout << "  Folder is empty as expected\n";
    else
        cout << "  Folder is not empty which is weird!\n";

    dir.cdUp();
    cout << "(deleting random folder...";
    if (dir.rmdir(sRandom))
        cout << "done!)\n";
    else
        cout << "failed)\n";

/*
bool	remove(const QString &fileName)
bool	removeRecursively()
bool	rename(const QString &oldName, const QString &newName)
*/

    cout << "#\n";
}

void TestReadWriteFiles()
{
}

int main(int argc, char *argv[])
{
    cout << "### File I/O in Qt ###\n";

    QCoreApplication a(argc, argv);
    TestQCoreApplication(a);
    TestQDir();
    TestReadWriteFiles();

    // Exit
    a.quit();
    cout << "\nAll done! Press a key to exit...\n";
    _getch();
    return 0;
}
