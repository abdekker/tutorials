#ifndef TEST_OPENGL_H
#define TEST_OPENGL_H

#include <iostream>

#ifdef __APPLE__
    // MacOS
    #include <GLUT/glut.h>
#else
    // Linux or Windows
    #include <GL/glut.h>
#endif

#endif  // TEST_OPENGL_H
