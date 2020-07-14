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

void StringsConstructB()
{
    // Constructing QString objects - part B (static QString::fromXYZ methods)
    cout << "\n### Constructing strings - B ###\n";
    cout << "(Using QString::fromXYZ methods, which are static)\n";

    // "QString::fromCFString" is only available on MacOS
    {
        QString s = QString::fromLatin1("constructed using 'QString::fromLatin1(const char *str, int size = -1)'");
        cout << "  01: " << s.toLatin1().data() << endl;
    }

    {
        QByteArray ba("constructed using 'QString::fromLatin1(const QByteArray &str)'");
        QString s = QString::fromLatin1(ba);
        cout << "  02: " << s.toLatin1().data() << endl;
    }

    {
        // "QString::fromLocal8Bit" is similar to "QString::fromLatin1". Uses "QTextCodec::codecForLocale()"
        // internally for the conversion.
        QString s1 = QString::fromLocal8Bit("constructed using 'QString::fromLocal8Bit(const char *str, int size = -1)'");
        cout << "  03a: " << s1.toLatin1().data() << endl;

        QByteArray ba("constructed using 'QString::fromLocal8Bit(const QByteArray &str)'");
        QString s2 = QString::fromLocal8Bit(ba);
        cout << "  03b: " << s2.toLatin1().data() << endl;
    }

    // "QString::fromNSString" is only available on MacOS

    {
        // Note: "QString::fromRawData" uses, but does not copy, the characters from the array. If the QString
        // is modified in any way, this triggers a deep copy of the data.
        QString s1("constructed using 'QString::fromRawData(const QChar *unicode, int size)'");
        QString s2 = QString::fromRawData(s1.data(), s1.size());
        cout << "  04: " << s2.toLatin1().data() << endl;
    }

    cout << "(Some QString methods crash / do not link in DEBUG mode, and require the 'd' versions of Qt.\n";
    cout << "These methods include: fromStdString, fromStdU16String, fromStdU32String, fromStdWString)\n";
#ifdef NDEBUG
    {
        string s1 = "constructed using 'QString::fromStdString(const std::string &str)'";
        QString s2 = QString::fromStdString(s1);
        cout << "  05: " << s2.toLatin1().data() << endl;
    }

    {
        u16string s1 = u"constructed using 'QString::fromStdU16String(const std::u16string &str)'"s;
        QString s2 = QString::fromStdU16String(s1);
        cout << "  06: " << s2.toLatin1().data() << endl;
    }

    {
        u32string s1 = U"constructed using 'QString::fromStdU32String(const std::u32string &str)'"s;
        QString s2 = QString::fromStdU32String(s1);
        cout << "  07: " << s2.toLatin1().data() << endl;
    }

    {
        wstring s1 = L"constructed using 'QString::fromStdWString(const std::wstring &str)'"s;
        QString s2 = QString::fromStdWString(s1);
        cout << "  08: " << s2.toLatin1().data() << endl;
    }
#endif

    // 09: QString fromUcs4(const uint *unicode, int size = -1)
    // 10: QString fromUcs4(const char32_t *str, int size = -1)
    cout << "  09: QString::fromUcs4 (TODO)\n";
    cout << "  10: QString::fromUcs4 (TODO)\n";

    {
        QString s = QString::fromUtf8("constructed using 'QString::fromUtf8(const char *str, int size = -1)'");
        cout << "  11: " << s.toLatin1().data() << endl;
    }

    {
        QByteArray ba("constructed using 'QString::fromUtf8(const QByteArray &str)'");
        QString s = QString::fromUtf8(ba);
        cout << "  12: " << s.toLatin1().data() << endl;
    }

    // 14: QString fromUtf16(const ushort *unicode, int size = -1)
    cout << "  13: QString::fromUtf16 (TODO)\n";

    {
        QString s = QString::fromUtf16(u"constructed using 'QString::fromUtf16(const char16_t *str, int size = -1)'");
        cout << "  14: " << s.toLatin1().data() << endl;
    }

    {
        QString s = QString::fromWCharArray(L"constructed using 'QString::fromWCharArray(const wchar_t *str, int size = -1)'");
        cout << "  15: " << s.toLatin1().data() << endl;
    }
    cout << "#\n";
}

void StringsConstructC()
{
    // Constructing QString objects - part C (operator= overloads)
    cout << "\n### Constructing strings - C ###\n";
    cout << "(Using QString::operator= overloads)\n";

    {
        QByteArray ba("Hello QByteArray");
        QString s = ba;
        cout << "  QString &operator=(const QByteArray &ba)\t\t" << s.toLatin1().data() << endl;
    }

    {
        QString s1("Hello QString (using std::move)");
        QString s2 = std::move(s1);
        cout << "  QString &operator=(QString &&other)\t\t\t" << s2.toLatin1().data() << endl;
    }

    {
        QString s1("Hello QString");
        QString s2 = s1;
        cout << "  QString &operator=(const QString &other)\t\t" << s2.toLatin1().data() << endl;
    }

    {
        QString s = "Hello QString";
        cout << "  QString &operator=(QChar ch)\t\t\t\t" << s.toLatin1().data() << endl;
    }

    {
        QLatin1String s1("Hello QLatin1String");
        QString s2 = s1;
        cout << "  QString &operator=(QLatin1String str)\t\t\t" << s2.toLatin1().data() << endl;
    }

    {
        QString s = "Hello char *";
        cout << "  QString &operator=(const char *str)\t\t\t" << s.toLatin1().data() << endl;
    }

    {
        QString s = 'z';
        cout << "  QString &operator=(char ch)\t\t\t\t" << s.toLatin1().data() << endl;
    }
    cout << "#\n";
}
    cout << "\nAll done! Press a key to exit...\n";
    _getch();
    return 0;
}