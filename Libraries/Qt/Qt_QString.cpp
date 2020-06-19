// Demonstrates some aspects of the Qt library QString
#include <QChar>
#include <QString>
#include <QDebug>

int main()
    //QString test("Hello world!");
    QString test = "Hello world!";
    //qDebug() << test;
    
    //QString result;
    //QTextStream(&result) << "pi = " << 3.14;    // "pi = 3.14"
    
    int i = 1;
    int total = 17;
    QString fileName = "myFile.doc";
    QString status = QString("Processing file %1 of %2: %3").arg(i).arg(total).arg(fileName);
    // "Processing file 1 of 17: myFile.doc"
    
    /*QString hello("hello");
    QStringRef el(&hello, 2, 3);
    QLatin1String world("world");
    QString message =  hello % el % world % QChar('!'); // "hellolloworld!"*/
    
    return 0;
}
