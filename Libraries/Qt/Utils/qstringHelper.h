#ifndef QSTRING_HELPER_H_
#define QSTRING_HELPER_H_

#include <QString>

// Header-only helper class for QString. To use, simply define a variable of type "QStringHelper".
class QStringHelper
{
public:
    // Constructor / destructor
    QStringHelper() {}
    ~QStringHelper() {}

    QString GetRandomString(const int cLength)
    {
        // Upper, lowercase letters and numbers
        const QString possibleCharacters("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");
        QString randomised;
        qsrand(clock());
        for (int pos = 0; pos < cLength; pos++)
            randomised.append(possibleCharacters.at(qrand() % possibleCharacters.length()));

        return randomised;
    }
};

#endif // PAP_TOC_H_