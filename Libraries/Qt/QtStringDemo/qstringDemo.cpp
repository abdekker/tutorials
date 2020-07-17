#include <conio.h>
#include <iostream>

#include <QString>
#include <QUuid>                // For QUuid::createUuid()
#include <QRegularExpression>   // For regular expressions
#include <QDebug>

// Used when generating a randomised string
enum class RandomString {
    RandomTypeCapitalsOnly,
    RandomTypeAlphabetical,
    RandomTypeUUID
};

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

void StringsConstructD()
{
    // Constructing QString objects - part D (miscellaneous)
    cout << "\n### Constructing strings - D ###\n";
    cout << "(Miscellaneous construction of strings)\n";

    QString sBase = "Hello World!";
    QString sAppend = "1234567890";
    cout << "\n(Base string = " << sBase.toLatin1().data() << ")\n";
    cout << "(Append string = " << sAppend.toLatin1().data() << ")\n";

    {
        QString s = sBase;
        s.append(sAppend);
        cout << "  QString &append(const QString &str)\t\t\t" << s.toLatin1().data() << endl;
    }

    {
        QString s = sBase;
        s.append(sAppend[0]);
        cout << "  QString &append(QChar ch)\t\t\t\t" << s.toLatin1().data() << endl;
    }

    {
        QString s = sBase;
        s.append(sAppend.data(), 5);
        cout << "  QString &append(const QChar *str, int len)\t\t" << s.toLatin1().data() << endl;
    }

    {
        QString s = sBase;
        QStringRef ref(&sAppend);
        s.append(ref);
        cout << "  QString &append(const QStringRef &reference)\t\t" << s.toLatin1().data() << endl;
    }

    {
        QString s = sBase;
        s.append(QLatin1String(sAppend.toLatin1()));
        cout << "  QString &append(QLatin1String str)\t\t\t" << s.toLatin1().data() << endl;
    }

    {
        QString s = sBase;
        s.append(sAppend.toLatin1().data());
        cout << "  QString &append(const char *str)\t\t\t" << s.toLatin1().data() << endl;
    }

    {
        QString s = sBase;
        s.append(sAppend.toLatin1());
        cout << "  QString &append(const QByteArray &ba)\t\t\t" << s.toLatin1().data() << endl;
    }

    //QString repeated(int times) const
    cout << "#\n";
}

void StringsModifyA()
{
    // Modifying existing QString objects - part A (QString::arg)
    cout << "\n### Modifying existing strings - A ###\n";
    cout << "(Using QString::arg overloads)\n\n";

    {
        QString a("World!");
        QString s1 = QString("Hello %1").arg(a);
        cout << "  QString::arg(const QString &a,...)\t\t\t" << s1.toLatin1().data() << endl;

        QString s2 = QString("Hello %1").arg(a, 10, QChar(' '));
        cout << "  QString::arg(const QString &a, int, QChar)\t\t" << s2.toLatin1().data() << endl;
    }

    {
        qlonglong a = 12345;    // qlonglong == __int64
        QString s1 = QString("Hello %1").arg(a);
        cout << "  QString::arg(qlonglong a,...)\t\t\t\t" << s1.toLatin1().data() << endl;
    
        QString s2 = QString("Hello %1").arg(a, 10, 10, QChar(' '));
        cout << "  QString::arg(qlonglong a, int, int, QChar)\t\t" << s2.toLatin1().data() << endl;
    }

	{
        qulonglong a = 98765;    // qulonglong == unsigned__int64
        QString s1 = QString("Hello %1").arg(a);
        cout << "  QString::arg(qulonglong a,...)\t\t\t" << s1.toLatin1().data() << endl;
    
        QString s2 = QString("Hello %1").arg(a, 10, 10, QChar(' '));
        cout << "  QString::arg(qulonglong a, int, int, QChar)\t\t" << s2.toLatin1().data() << endl;
    }

	{
        long a = 1122;
        QString s1 = QString("Hello %1").arg(a);
        cout << "  QString::arg(long a,...)\t\t\t\t" << s1.toLatin1().data() << endl;
    
        QString s2 = QString("Hello %1").arg(a, 10, 10, QChar(' '));
        cout << "  QString::arg(long a, int, int, QChar)\t\t\t" << s2.toLatin1().data() << endl;
		cout << "    (...and similarly for 'ulong', 'int', 'uint', 'short', and 'ushort')\n";
    }

    {
        double a = 3.14159265358979;
        QString s1 = QString("Hello %1").arg(a);
        cout << "  QString::arg(double a,...)\t\t\t\t" << s1.toLatin1().data() << endl;
    
        QString s2 = QString("Hello %1").arg(a, 15, 'g', 12, QChar(' '));
        cout << "  QString::arg(double a, int, char, int, QChar)\t\t" << s2.toLatin1().data() << endl;
    }

/*
QString::arg(char a, int fieldWidth = 0, QChar fillChar = QLatin1Char(' ')) const
QString::arg(QChar a, int fieldWidth = 0, QChar fillChar = QLatin1Char(' ')) const
QString::arg(QStringView a, int fieldWidth = 0, QChar fillChar = QLatin1Char(' ')) const
QString::arg(QLatin1String a, int fieldWidth = 0, QChar fillChar = QLatin1Char(' ')) const
QString::arg(const QString &a1, const QString &a2) const
QString::arg(const QString &a1, const QString &a2, const QString &a3) const
QString::arg(const QString &a1, const QString &a2, const QString &a3, const QString &a4) const
QString::arg(const QString &a1, const QString &a2, const QString &a3, const QString &a4, const QString &a5) const
QString::arg(const QString &a1, const QString &a2, const QString &a3, const QString &a4, const QString &a5, const QString &a6) const
QString::arg(const QString &a1, const QString &a2, const QString &a3, const QString &a4, const QString &a5, const QString &a6, const QString &a7) const
QString::arg(const QString &a1, const QString &a2, const QString &a3, const QString &a4, const QString &a5, const QString &a6, const QString &a7, const QString &a8) const
QString::arg(const QString &a1, const QString &a2, const QString &a3, const QString &a4, const QString &a5, const QString &a6, const QString &a7, const QString &a8, const QString &a9) const
QString::arg(Args &&... args) const
*/

    //entry.textS = QString("Item %1").arg(item++);
    //entry.textS = QString("Item %1").arg(item++);

    //string s

    //sTest2.push_back(QString::number(2));
    cout << "#\n";
}

void StringsExtract()
{
}

void StringsCompare()
{
/*bool operator!=(QLatin1String other) const
bool operator!=(const char *other) const
bool operator!=(const QByteArray &other) const

bool operator<(QLatin1String other) const
bool operator<(const char *other) const
bool operator<(const QByteArray &other) const
bool operator<=(QLatin1String other) const
bool operator<=(const char *other) const
bool operator<=(const QByteArray &other) const
bool operator==(QLatin1String other) const
bool operator==(const char *other) const
bool operator==(const QByteArray &other) const
bool operator>(QLatin1String other) const
bool operator>(const char *other) const
bool operator>(const QByteArray &other) const
bool operator>=(QLatin1String other) const
bool operator>=(const char *other) const
bool operator>=(const QByteArray &other) const*/
}

void StringsSearch()
{
/*
int indexOf(QLatin1String str, int from = 0, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
int indexOf(QChar ch, int from = 0, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
int indexOf(const QString &str, int from = 0, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
int indexOf(const QStringRef &str, int from = 0, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
int indexOf(QStringView str, int from = 0, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
int indexOf(const QRegExp &rx, int from = 0) const
int indexOf(QRegExp &rx, int from = 0) const
int indexOf(const QRegularExpression &re, int from = 0) const
int indexOf(const QRegularExpression &re, int from, QRegularExpressionMatch *rmatch) const
*/
}

void StringsRandom(RandomString type, int length)
{
    // Creates a randomised string
    static int numCalls = 0;
    QString szRandom;
    qsrand(clock());    // "clock" returns the ms since the process starts
    switch (type) {
        case RandomString::RandomTypeCapitalsOnly:
        {
            // Capital letters only
            szRandom.resize(length);
            for (int pos = 0; pos < length; pos++)
                szRandom[pos] = (QChar('A' + char(qrand() % ('Z' - 'A'))));
        }
        break;

        case RandomString::RandomTypeAlphabetical:
        {
            // Upper, lowercase letters and numbers
            const QString possibleCharacters("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");
            for (int pos = 0; pos < length; pos++)
                szRandom.append(possibleCharacters.at(qrand() % possibleCharacters.length()));
        }
        break;

        case RandomString::RandomTypeUUID:
        {
            // Generate a UUIID (pseudo-random string guaranteed to be unique)
            szRandom = QUuid::createUuid().toString();
            szRandom.remove(QRegularExpression("{|}|-"));   // To have only hex numbers
        }
        break;
    }

    if (szRandom.size()) {
        QString szResult = QString("  Random string (type=%1, length=%2):\t%3").arg(
            QString::number(numCalls++), QString::number(length), szRandom);
        cout << szResult.toLatin1().data() << endl;
    }
}

int main(int argc, char *argv[])
{
    // Ensure the output buffer is flushed on each insertion operation
    setvbuf(stdout, NULL, _IONBF, 0);
    //std::cout << std::unitbuf;

    // Demonstrates some aspects of using strings in Qt
    // You must install the Qt library and the "Qt VS Tools" extension to build in VS 2019
    cout << "Using QString and QChar from the Qt library\n";
    cout << "  Includes Qt debugging options to the console\n";
    cout << "  Install Qt (eg. C:\\Qt\\5.15.0) and the 'Qt VS Tools' extension in VS 2019\n\n";

    StringsOutputToConsole();

    StringsConstructA();
    StringsConstructB();
    StringsConstructC();
    StringsConstructD();

    StringsModifyA();

    StringsExtract();
    StringsCompare();
    StringsSearch();

    cout << "\n### Randomising strings ###\n";
    StringsRandom(RandomString::RandomTypeCapitalsOnly, 9);
    StringsRandom(RandomString::RandomTypeAlphabetical, 15);
    StringsRandom(RandomString::RandomTypeUUID, 0);
    cout << "#\n";

    // Exit
    cout << "\nAll done! Press a key to exit...\n";
    _getch();
    return 0;
}

/*
This list taken from: https://doc.qt.io/qt-5/qstring.html
[ Items that are tabbed out have been implemented in this test program ]

        QString(const QByteArray &ba)
        QString(const char *str)
        QString(QString &&other)
        QString(const QString &other)
        QString(QLatin1String str)
        QString(int size, QChar ch)
        QString(QChar ch)
        QString(const QChar *unicode, int size = -1)
QString()
        QString & operator=(const QByteArray &ba)
        QString & operator=(QString &&other)
        QString & operator=(const QString &other)
~QString()
QString & append(const QString &str)
QString & append(QChar ch)
QString & append(const QChar *str, int len)
QString & append(const QStringRef &reference)
QString & append(QLatin1String str)
QString & append(const char *str)
QString & append(const QByteArray &ba)
QString arg(const QString &a, int fieldWidth = 0, QChar fillChar = QLatin1Char(' ')) const
QString arg(qlonglong a, int fieldWidth = 0, int base = 10, QChar fillChar = QLatin1Char(' ')) const
QString arg(qulonglong a, int fieldWidth = 0, int base = 10, QChar fillChar = QLatin1Char(' ')) const
QString arg(long a, int fieldWidth = 0, int base = 10, QChar fillChar = QLatin1Char(' ')) const
QString arg(ulong a, int fieldWidth = 0, int base = 10, QChar fillChar = QLatin1Char(' ')) const
QString arg(int a, int fieldWidth = 0, int base = 10, QChar fillChar = QLatin1Char(' ')) const
QString arg(uint a, int fieldWidth = 0, int base = 10, QChar fillChar = QLatin1Char(' ')) const
QString arg(short a, int fieldWidth = 0, int base = 10, QChar fillChar = QLatin1Char(' ')) const
QString arg(ushort a, int fieldWidth = 0, int base = 10, QChar fillChar = QLatin1Char(' ')) const
QString arg(double a, int fieldWidth = 0, char format = 'g', int precision = -1, QChar fillChar = QLatin1Char(' ')) const
QString arg(char a, int fieldWidth = 0, QChar fillChar = QLatin1Char(' ')) const
QString arg(QChar a, int fieldWidth = 0, QChar fillChar = QLatin1Char(' ')) const
QString arg(QStringView a, int fieldWidth = 0, QChar fillChar = QLatin1Char(' ')) const
QString arg(QLatin1String a, int fieldWidth = 0, QChar fillChar = QLatin1Char(' ')) const
QString arg(const QString &a1, const QString &a2) const
QString arg(const QString &a1, const QString &a2, const QString &a3) const
QString arg(const QString &a1, const QString &a2, const QString &a3, const QString &a4) const
QString arg(const QString &a1, const QString &a2, const QString &a3, const QString &a4, const QString &a5) const
QString arg(const QString &a1, const QString &a2, const QString &a3, const QString &a4, const QString &a5, const QString &a6) const
QString arg(const QString &a1, const QString &a2, const QString &a3, const QString &a4, const QString &a5, const QString &a6, const QString &a7) const
QString arg(const QString &a1, const QString &a2, const QString &a3, const QString &a4, const QString &a5, const QString &a6, const QString &a7, const QString &a8) const
QString arg(const QString &a1, const QString &a2, const QString &a3, const QString &a4, const QString &a5, const QString &a6, const QString &a7, const QString &a8, const QString &a9) const
QString arg(Args &&... args) const
const QChar at(int position) const
QChar back() const
QCharRef back()
QString::iterator begin()
QString::const_iterator begin() const
int capacity() const
QString::const_iterator cbegin() const
QString::const_iterator cend() const
void chop(int n)
QString chopped(int len) const
void clear()
int compare(const QString &other, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
int compare(const QStringRef &ref, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
int compare(QLatin1String other, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
int compare(QStringView s, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
int compare(QChar ch, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
QString::const_iterator constBegin() const
const QChar * constData() const
QString::const_iterator constEnd() const
bool contains(const QString &str, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
bool contains(QChar ch, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
bool contains(const QStringRef &str, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
bool contains(QLatin1String str, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
bool contains(QStringView str, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
bool contains(const QRegExp &rx) const
bool contains(QRegExp &rx) const
bool contains(const QRegularExpression &re) const
bool contains(const QRegularExpression &re, QRegularExpressionMatch *rmatch) const
int count(const QString &str, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
int count() const
int count(QChar ch, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
int count(const QStringRef &str, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
int count(const QRegExp &rx) const
int count(const QRegularExpression &re) const
QString::const_reverse_iterator crbegin() const
QString::const_reverse_iterator crend() const
QChar * data()
const QChar * data() const
QString::iterator end()
QString::const_iterator end() const
bool endsWith(const QString &s, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
bool endsWith(const QStringRef &s, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
bool endsWith(QStringView str, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
bool endsWith(QLatin1String s, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
bool endsWith(QChar c, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
QString & fill(QChar ch, int size = -1)
QChar front() const
QCharRef front()
int indexOf(QLatin1String str, int from = 0, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
int indexOf(QChar ch, int from = 0, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
int indexOf(const QString &str, int from = 0, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
int indexOf(const QStringRef &str, int from = 0, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
int indexOf(QStringView str, int from = 0, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
int indexOf(const QRegExp &rx, int from = 0) const
int indexOf(QRegExp &rx, int from = 0) const
int indexOf(const QRegularExpression &re, int from = 0) const
int indexOf(const QRegularExpression &re, int from, QRegularExpressionMatch *rmatch) const
QString & insert(int position, const QString &str)
QString & insert(int position, QChar ch)
QString & insert(int position, const QChar *unicode, int size)
QString & insert(int position, const QStringRef &str)
QString & insert(int position, QLatin1String str)
QString & insert(int position, const char *str)
QString & insert(int position, const QByteArray &str)
bool isEmpty() const
bool isLower() const
bool isNull() const
bool isRightToLeft() const
bool isUpper() const
bool isValidUtf16() const
int lastIndexOf(const QString &str, int from = -1, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
int lastIndexOf(QChar ch, int from = -1, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
int lastIndexOf(QLatin1String str, int from = -1, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
int lastIndexOf(const QStringRef &str, int from = -1, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
int lastIndexOf(QStringView str, int from = -1, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
int lastIndexOf(const QRegExp &rx, int from = -1) const
int lastIndexOf(QRegExp &rx, int from = -1) const
int lastIndexOf(const QRegularExpression &re, int from = -1) const
int lastIndexOf(const QRegularExpression &re, int from, QRegularExpressionMatch *rmatch) const
QString left(int n) const
QString leftJustified(int width, QChar fill = QLatin1Char(' '), bool truncate = false) const
QStringRef leftRef(int n) const
int length() const
int localeAwareCompare(const QString &other) const
int localeAwareCompare(const QStringRef &other) const
QString mid(int position, int n = -1) const
QStringRef midRef(int position, int n = -1) const
QString normalized(QString::NormalizationForm mode, QChar::UnicodeVersion version = QChar::Unicode_Unassigned) const
QString & prepend(const QString &str)
QString & prepend(QChar ch)
QString & prepend(const QChar *str, int len)
QString & prepend(const QStringRef &str)
QString & prepend(QLatin1String str)
QString & prepend(const char *str)
QString & prepend(const QByteArray &ba)
void push_back(const QString &other)
void push_back(QChar ch)
void push_front(const QString &other)
void push_front(QChar ch)
QString::reverse_iterator rbegin()
QString::const_reverse_iterator rbegin() const
QString & remove(int position, int n)
QString & remove(QChar ch, Qt::CaseSensitivity cs = Qt::CaseSensitive)
QString & remove(QLatin1String str, Qt::CaseSensitivity cs = Qt::CaseSensitive)
QString & remove(const QString &str, Qt::CaseSensitivity cs = Qt::CaseSensitive)
QString & remove(const QRegExp &rx)
QString & remove(const QRegularExpression &re)
QString::reverse_iterator rend()
QString::const_reverse_iterator rend() const
QString repeated(int times) const
QString & replace(int position, int n, const QString &after)
QString & replace(int position, int n, QChar after)
QString & replace(int position, int n, const QChar *unicode, int size)
QString & replace(QChar before, QChar after, Qt::CaseSensitivity cs = Qt::CaseSensitive)
QString & replace(const QChar *before, int blen, const QChar *after, int alen, Qt::CaseSensitivity cs = Qt::CaseSensitive)
QString & replace(QLatin1String before, QLatin1String after, Qt::CaseSensitivity cs = Qt::CaseSensitive)
QString & replace(QLatin1String before, const QString &after, Qt::CaseSensitivity cs = Qt::CaseSensitive)
QString & replace(const QString &before, QLatin1String after, Qt::CaseSensitivity cs = Qt::CaseSensitive)
QString & replace(const QString &before, const QString &after, Qt::CaseSensitivity cs = Qt::CaseSensitive)
QString & replace(QChar ch, const QString &after, Qt::CaseSensitivity cs = Qt::CaseSensitive)
QString & replace(QChar c, QLatin1String after, Qt::CaseSensitivity cs = Qt::CaseSensitive)
QString & replace(const QRegExp &rx, const QString &after)
QString & replace(const QRegularExpression &re, const QString &after)
void reserve(int size)
void resize(int size)
void resize(int size, QChar fillChar)
QString right(int n) const
QString rightJustified(int width, QChar fill = QLatin1Char(' '), bool truncate = false) const
QStringRef rightRef(int n) const
QString section(QChar sep, int start, int end = -1, QString::SectionFlags flags = SectionDefault) const
QString section(const QString &sep, int start, int end = -1, QString::SectionFlags flags = SectionDefault) const
QString section(const QRegExp &reg, int start, int end = -1, QString::SectionFlags flags = SectionDefault) const
QString section(const QRegularExpression &re, int start, int end = -1, QString::SectionFlags flags = SectionDefault) const
QString & setNum(int n, int base = 10)
QString & setNum(short n, int base = 10)
QString & setNum(ushort n, int base = 10)
QString & setNum(uint n, int base = 10)
QString & setNum(long n, int base = 10)
QString & setNum(ulong n, int base = 10)
QString & setNum(qlonglong n, int base = 10)
QString & setNum(qulonglong n, int base = 10)
QString & setNum(float n, char format = 'g', int precision = 6)
QString & setNum(double n, char format = 'g', int precision = 6)
QString & setRawData(const QChar *unicode, int size)
QString & setUnicode(const QChar *unicode, int size)
QString & setUtf16(const ushort *unicode, int size)
void shrink_to_fit()
QString simplified() const
int size() const
QStringList split(const QString &sep, Qt::SplitBehavior behavior = Qt::KeepEmptyParts, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
QStringList split(QChar sep, Qt::SplitBehavior behavior = Qt::KeepEmptyParts, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
QStringList split(const QRegExp &rx, Qt::SplitBehavior behavior = Qt::KeepEmptyParts) const
QStringList split(const QRegularExpression &re, Qt::SplitBehavior behavior = Qt::KeepEmptyParts) const
QVector<QStringRef> splitRef(const QString &sep, Qt::SplitBehavior behavior = Qt::KeepEmptyParts, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
QVector<QStringRef> splitRef(QChar sep, QString::SplitBehavior behavior, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
QVector<QStringRef> splitRef(QChar sep, Qt::SplitBehavior behavior = Qt::KeepEmptyParts, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
QVector<QStringRef> splitRef(const QRegExp &rx, Qt::SplitBehavior behavior = Qt::KeepEmptyParts) const
QVector<QStringRef> splitRef(const QRegularExpression &re, Qt::SplitBehavior behavior = Qt::KeepEmptyParts) const
void squeeze()
bool startsWith(const QString &s, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
bool startsWith(const QStringRef &s, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
bool startsWith(QStringView str, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
bool startsWith(QLatin1String s, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
bool startsWith(QChar c, Qt::CaseSensitivity cs = Qt::CaseSensitive) const
void swap(QString &other)
CFStringRef toCFString() const
QString toCaseFolded() const
double toDouble(bool *ok = nullptr) const
float toFloat(bool *ok = nullptr) const
QString toHtmlEscaped() const
int toInt(bool *ok = nullptr, int base = 10) const
QByteArray toLatin1() const
QByteArray toLocal8Bit() const
long toLong(bool *ok = nullptr, int base = 10) const
qlonglong toLongLong(bool *ok = nullptr, int base = 10) const
QString toLower() const
NSString * toNSString() const
short toShort(bool *ok = nullptr, int base = 10) const
std::string toStdString() const
std::u16string toStdU16String() const
std::u32string toStdU32String() const
std::wstring toStdWString() const
uint toUInt(bool *ok = nullptr, int base = 10) const
ulong toULong(bool *ok = nullptr, int base = 10) const
qulonglong toULongLong(bool *ok = nullptr, int base = 10) const
ushort toUShort(bool *ok = nullptr, int base = 10) const
QVector<uint> toUcs4() const
QString toUpper() const
QByteArray toUtf8() const
int toWCharArray(wchar_t *array) const
QString trimmed() const
void truncate(int position)
const QChar * unicode() const
const ushort * utf16() const
bool operator!=(QLatin1String other) const
bool operator!=(const char *other) const
bool operator!=(const QByteArray &other) const
QString & operator+=(QChar ch)
QString & operator+=(const QString &other)
QString & operator+=(const QStringRef &str)
QString & operator+=(QLatin1String str)
QString & operator+=(const char *str)
QString & operator+=(const QByteArray &ba)
QString & operator+=(char ch)
bool operator<(QLatin1String other) const
bool operator<(const char *other) const
bool operator<(const QByteArray &other) const
bool operator<=(QLatin1String other) const
bool operator<=(const char *other) const
bool operator<=(const QByteArray &other) const
QString & operator=(QChar ch)
QString & operator=(QLatin1String str)
QString & operator=(const char *str)
QString & operator=(char ch)
bool operator==(QLatin1String other) const
bool operator==(const char *other) const
bool operator==(const QByteArray &other) const
bool operator>(QLatin1String other) const
bool operator>(const char *other) const
bool operator>(const QByteArray &other) const
bool operator>=(QLatin1String other) const
bool operator>=(const char *other) const
bool operator>=(const QByteArray &other) const
QCharRef operator[](int position)
const QChar operator[](int position) const
const QChar operator[](uint position) const
QCharRef operator[](uint position)
Static Public Members
QString asprintf(const char *cformat, ...)
int compare(const QString &s1, const QString &s2, Qt::CaseSensitivity cs = Qt::CaseSensitive)
int compare(const QString &s1, QLatin1String s2, Qt::CaseSensitivity cs = Qt::CaseSensitive)
int compare(QLatin1String s1, const QString &s2, Qt::CaseSensitivity cs = Qt::CaseSensitive)
int compare(const QString &s1, const QStringRef &s2, Qt::CaseSensitivity cs = Qt::CaseSensitive)
QString fromCFString(CFStringRef string)
QString fromLatin1(const char *str, int size = -1)
QString fromLatin1(const QByteArray &str)
QString fromLocal8Bit(const char *str, int size = -1)
QString fromLocal8Bit(const QByteArray &str)
QString fromNSString(const NSString *string)
QString fromRawData(const QChar *unicode, int size)
QString fromStdString(const std::string &str)
QString fromStdU16String(const std::u16string &str)
QString fromStdU32String(const std::u32string &str)
QString fromStdWString(const std::wstring &str)
QString fromUcs4(const uint *unicode, int size = -1)
QString fromUcs4(const char32_t *str, int size = -1)
QString fromUtf8(const char *str, int size = -1)
QString fromUtf8(const QByteArray &str)
QString fromUtf16(const ushort *unicode, int size = -1)
QString fromUtf16(const char16_t *str, int size = -1)
QString fromWCharArray(const wchar_t *string, int size = -1)
int localeAwareCompare(const QString &s1, const QString &s2)
int localeAwareCompare(const QString &s1, const QStringRef &s2)
QString number(long n, int base = 10)
QString number(int n, int base = 10)
QString number(uint n, int base = 10)
QString number(ulong n, int base = 10)
QString number(qlonglong n, int base = 10)
QString number(qulonglong n, int base = 10)
QString number(double n, char format = 'g', int precision = 6)
QString vasprintf(const char *cformat, va_list ap)
Related Non-Members
bool operator!=(const QString &s1, const QString &s2)
bool operator!=(const char *s1, const QString &s2)
const QString operator+(const QString &s1, const QString &s2)
const QString operator+(const QString &s1, const char *s2)
const QString operator+(const char *s1, const QString &s2)
const QString operator+(char ch, const QString &s)
const QString operator+(const QString &s, char ch)
bool operator<(const QString &s1, const QString &s2)
bool operator<(const char *s1, const QString &s2)
QDataStream & operator<<(QDataStream &stream, const QString &string)
bool operator<=(const QString &s1, const QString &s2)
bool operator<=(const char *s1, const QString &s2)
bool operator==(const QString &s1, const QString &s2)
bool operator==(const char *s1, const QString &s2)
bool operator>(const QString &s1, const QString &s2)
bool operator>(const char *s1, const QString &s2)
bool operator>=(const QString &s1, const QString &s2)
bool operator>=(const char *s1, const QString &s2)
QDataStream & operator>>(QDataStream &stream, QString &string)
*/