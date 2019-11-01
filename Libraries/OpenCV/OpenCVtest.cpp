// Note: On Linux, OpenCV is installed to /usr/include/opencv and /usr/include/opencv2
#include <opencv2/highgui/highgui.hpp>

/*#include "opencv2/core/core_c.h"
#include "opencv2/core/core.hpp"
#include "opencv2/flann/miniflann.hpp"
#include "opencv2/imgproc/imgproc_c.h"
#include "opencv2/imgproc/imgproc.hpp"
#include "opencv2/video/video.hpp"
#include "opencv2/features2d/features2d.hpp"
#include "opencv2/objdetect/objdetect.hpp"
#include "opencv2/calib3d/calib3d.hpp"
#include "opencv2/ml/ml.hpp"
#include "opencv2/highgui/highgui_c.h"
#include "opencv2/highgui/highgui.hpp"*/
//#include "opencv2/contrib/contrib.hpp

#include "OpenCVtest.hpp"
//#include "TestSystem.hpp"

#ifdef WINDOWS
    #include <direct.h>
    #define GetCurrentDir _getcwd
#else
    #include <unistd.h>
    #define GetCurrentDir getcwd
 #endif

#include <iostream>
using namespace cv;

int main()
{
    // Compilation in VScode not working. To compile:
    // * Navigate to the project directory (~/Projects/Test/OpenCVtest)
    // * Run this command:
    //      - g++ -ggdb `pkg-config --cflags opencv` -o `basename OpenCVtest.cpp .cpp` OpenCVtest.cpp `pkg-config --libs opencv`

    std::cout << "Hello World 1" << std::endl;

    // Create a test system class for paths
    TestSystem mySystem;
    mySystem.Initialise();
    std::cout << "System: Output run path = " << mySystem.GetOutputRunPath() << std::endl;
    std::cout << "System: Working dirtectory = " << mySystem.GetWorkingDirectory() << std::endl;

    // Use some local functions
    std::cout << myRandom() << std::endl;
    std::cout << mySum(5, 7) << std::endl;

    // Load image
    //Mat img = imread("//home//abdekker//Pictures//test.bmp", CV_LOAD_IMAGE_COLOR);
    //Mat img = imread("//home//abdekker//Pictures//test.png", 1);
    //Mat img = imread("//home//abdekker//Pictures//test.png", 1);
    //Mat img = imread("..//testImages//test.png", 1);
    Mat img = imread(mySystem.GetPathToTestImages() + "test.bmp", 1);
    std::cout << "Width: " << img.cols << " Height: " << img.rows << std::endl;
    if (!img.empty())
        imshow("Alain OpenCV test", img);

    // This bash script may allow you to display a video stream
    //cv::VideoCapture
    /* vcap = cv.VideoCapture("rtsp://192.168.1.2:8080/out.h264")
    while(1):

        ret, frame = vcap.read()
        cv.imshow('VIDEO', frame)
        cv.waitKey(1) */

    std::cout << "Hello World 2" << std::endl;

    //std::cout << ${execPath} << std::endl;
    std::cout << "Hello World 3" << std::endl;

    waitKey(0);
    return 0;
}

int myRandom()
{
    return rand();
}

int mySum(int x, int y)
{
    return (x + y);
}

void myOutputRunPath()
{
    char pid[32];
    char buf[256];
    size_t len = sizeof(buf);
    sprintf(pid, "/proc/%d/exe", getpid());
    int nBytes = MIN(readlink(pid, buf, len), len - 1);
    if (nBytes == 0)
    {
        // Failed to get output path
        std::cout << "Output path not be read!" << std::endl;
    }
    else
    {
        // Success!
        buf[nBytes] = '\0';
        std::cout << "Output path:" << buf << std::endl;
    }
}

void myOutputWorkingDir()
{
    char cwd[256];
    if (GetCurrentDir(cwd, sizeof(cwd)) == NULL)
    {
        // Failed to get working directory
        std::cout << "Working directory not found!" << std::endl;
    }
    else
    {
        // Success!
        std::cout << "Working directory: " <<  cwd << std::endl;
    }
}

// Start: TestSystem
TestSystem::TestSystem()
{
    // Constructor
}

void TestSystem::Initialise()
{
    // Initialise the output run path, working directory and other system parameters

    // Output run path
    char pid[32];
    char buf[256];
    size_t len = sizeof(buf);
    sprintf(pid, "/proc/%d/exe", getpid());
    int nBytes = MIN(readlink(pid, buf, len), len - 1);
    if (nBytes  == 0)
    {
        // Failed to get output path
        std::cout << "Output path not be read!" << std::endl;
    }
    else
    {
        // Success!
        buf[nBytes] = '\0';
        szOutputRunPath = buf;
    }

    // Working directory
    if (GetCurrentDir(buf, len) == NULL)
    {
        // Failed to get working directory
        std::cout << "Working directory not found!" << std::endl;
    }
    else
    {
        // Success!
        szWorkingDirectory = buf;
        szPathToTestImages = (szWorkingDirectory + "//testImages//");
    }
}
// End: TestSystem
