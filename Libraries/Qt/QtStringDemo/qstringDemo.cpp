#include <conio.h>
#include <iostream>

#include <QString>
#include <QDebug>

/* Demonstrates using QString (and Qt debug tools like "qDebug" to output to the console).
Initially compiled in Visual Studio 2019.

The default "Qt Console Application" wizard adds these lines and include. Not sure what
"CoreApplication" is or how to use it.
    //#include <QtCore/QCoreApplication> (this would be at the top)
    //QCoreApplication a(argc, argv);
    //return a.exec();
*/

inline void qStdout(const QString &output)
{
    QTextStream r{stdout};
    r << output;
    r.flush();
}

using namespace std;
int main(int argc, char *argv[])
{
    // Demonstrates some aspects of using QString
    // You must install the Qt library and the "Qt VS Tools" extension to build in VS 2019
    cout << "Using QString (and Qt debugging) from the Qt library\n";
    cout << "  Install Qt (eg. C:\\Qt\\5.15.0) and the 'Qt VS Tools' extension in VS 2019\n\n";

    cout << "### Output to the console ###\n";
    cout << "  Hello world using std::cout\n";
    qDebug() << "  Hello world using qDebug. Newline character is not required.";
    QString sHelloWorld(" Hello world using qDebug with a QString. Quotation marks are included.");
    qDebug() << sHelloWorld;

    cout << "\nAlternate ways to output\n";
    QString sTest("donkey");
    qDebug() << "  qDebug: " << sTest;
    cout << "  cout: " << sTest.toLatin1().data() << " (using QString::toLatin1::data)\n";
    cout << "  cout: " << sTest.toUtf8().data() << " (using QString::toUtf8::data)\n";

    // QTextStream captures the standard output. How to get it to release it?
    //sHW = QString("  Hello world using QTextStream. Quotation marks are now NOT included.\n");
    //qStdout(sHW);

    cout << "\nThere are four Qt debugging options: qDebug, qWarning, qCritical and qFatal\n";
    qDebug() << "  Debug Message (may be removed at compile time)";
    qWarning() << "  Warning Message";
    qCritical() << "  Critical Error Message";
    //qFatal("  Fatal Error Message (kills the program in RELEASE mode but can be ignored in DEBUG mode)");
    cout << "#\n";

    cout << "\n### Constructing QString objects ###\n";
    QString sTest1("test1");
    QString sTest2 = "test";
    sTest2.push_back(QString::number(2));
    cout << "  cout 1: " << sTest1.toLatin1().data() << endl;
    cout << "  cout 2: " << sTest2.toLatin1().data() << endl;
    cout << "#\n";

    cout << "\nAll done! Press a key to exit...\n";
    _getch();
    return 0;
}
