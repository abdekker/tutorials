#ifndef OPEN_CV_TEST_HPP
#define OPEN_CV_TEST_HPP

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

int myRandom();
int mySum(int x, int y);

class TestSystem
{
    public:
        TestSystem();
        void Initialise();

        std::string GetOutputRunPath() { return szOutputRunPath; }
        std::string GetWorkingDirectory() { return szWorkingDirectory; }
        std::string GetPathToTestImages() { return szPathToTestImages; }

    private:
        std::string szOutputRunPath;
        std::string szWorkingDirectory;
        std::string szPathToTestImages;
};

#endif	//OPEN_CV_TEST_HPP
