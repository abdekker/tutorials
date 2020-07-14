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
    //return a.exec(); */
using namespace std;

inline void qStdout(const QString &output)
{
    QTextStream r{stdout};
    r << output;
}

void StringsOutputToConsole()
{
    // General output
    cout << "### Output QString to the console ###\n";
    cout << "  Hello world using std::cout\n";
    qDebug() << "  Hello world using qDebug with char *. Newline character is not required.";
    QString sHW1(" Hello world using qDebug with QString. Quotation marks are included.");
    qDebug() << sHW1;
    QString sHW2 = QString("  Hello world using QTextStream\n");
    qStdout(sHW2);

    cout << "\nAlternate ways to output\n";
    QString sTest("donkey");
    qDebug() << "  qDebug: " << sTest;
    cout << "  cout: " << sTest.toLatin1().data() << " (using QString::toLatin1::data)\n";
    cout << "  cout: " << sTest.toUtf8().data() << " (using QString::toUtf8::data)\n";

    cout << "\nThere are four Qt debugging options: qDebug, qWarning, qCritical and qFatal\n";
    qDebug() << "  Debug Message (may be removed at compile time)";
    qWarning() << "  Warning Message";
    qCritical() << "  Critical Error Message";
    qDebug() << "  (qFatal not used: Kills the program in RELEASE mode, can be ignored in DEBUG mode)";
    //qFatal("  Fatal Error Message (kills the program in RELEASE mode but can be ignored in DEBUG mode)");
    cout << "#\n";
}

void StringsConstructA()
{
    // Constructing QString objects - part A (direct construction)
    cout << "\n### Constructing strings - A ###\n";
    cout << "(Using QString constructors)\n";

    {
        QByteArray ba("constructed using 'QString(const QByteArray &ba)'");
        QString s(ba);
        cout << "  01: " << s.toLatin1().data() << endl;
    }

    {
        QString s("constructed using 'QString(const char *str)'");
        cout << "  02: " << s.toLatin1().data() << endl;
    }

    {
        QString s1("constructed using 'QString(QString &&other)'");
        QString s2(std::move(s1));
        cout << "  03: " << s2.toLatin1().data() << endl;
    }

    {
        QString s1("constructed using 'QString(const QString &other)'");
        QString s2(s1);
        cout << "  04: " << s2.toLatin1().data() << endl;
    }

    {
        QString s(QString::fromLatin1("constructed using 'QString(QLatin1String str)'"));
        cout << "  05: " << s.toLatin1().data() << endl;
    }

    {
        cout << "(Next strings are constructed using 'QString(int size, QChar ch)')\n";
        QString s1(10, QChar('z'));
        QString s2(10, 'x');
        cout << "  06a: " << s1.toLatin1().data() << endl;
        cout << "  06b: " << s2.toLatin1().data() << endl;
    }

    {
        cout << "(Next strings are constructed using 'QString(QChar ch)')\n";
        QString s1(QChar('a'));
        QString s2('b');
        cout << "  07a: " << s1.toLatin1().data() << endl;
        cout << "  07b: " << s2.toLatin1().data() << endl;
    }

    {
        QString s1("constructed using 'QString(const QChar *unicode, int size = -1)'");
        QString s2(s1.data());
        cout << "  08a: " << s2.toLatin1().data() << endl;

        QChar c[] = { 'A', 'n', 'o', 't', 'h', 'e', 'r', ' ', 'Q', 'C', 'h', 'a', 'r', '*', '\0' };
        QString s3(&c[0]);
        cout << "  08b: " << s3.toLatin1().data() << endl;
    }
    cout << "#\n";
}
    cout << "#\n";

    cout << "\nAll done! Press a key to exit...\n";
    _getch();
    return 0;
}