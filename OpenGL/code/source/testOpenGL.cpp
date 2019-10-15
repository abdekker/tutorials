#include "testOpenGL.h"

// This code come from this tutorial: https://www.youtube.com/watch?v=exQ43PFWJBU
// Code adapted from Adrian Buzatu:
// git@github.com:abuzatu/BuzatuCPP.git
//      - CMake/tutorialOPENGL

// to allow me to move it with the keyboard WASD keys
// global variables holding the x,y,z coordiates of the cube
int cx=0;
int cy=0;
int cz=0;
// in theCube() we need to add a glPushMatrix() and a glPopMatrix()
// and inside translate to the coordinates cx,cy,cz
// then crate a keyboard() function to see what key is being pressed
// and depending on that change update the position of the cube

// to allow to create polygones with 4 vertices
struct Quads {
  // the coordinates of the 4 corners
  int x1,y1,z1;
  int x2,y2,z2;
  int x3,y3,z3;
  int x4,y4,z4;
  // the color
  float r,g,b;
  // the state, the counter of which of the corners I am at
  int state;
  // the counter of how many we have
  int total;
};

// add global variable of the number of Quads
int cn;

// global variable that stores an array of Quads, say 100 for now
Quads Q[100];

// add one Quad
// every time we add a space bar we add one more point
// that will increase our state by one, meaning we are at the next point
void addQuad() {
  // for the first Quad, so index 0
  // increase the state by one, so go tot the next point
  Q[0].state++;
  // our Quad has only 4 points, let's prevent it by resetting it to 1
  if (Q[0].state>4) {
    Q[0].state=1;
  }
  //
  int st=Q[0].state;

  if (st==1) {
    Q[0].total++;
    cn=Q[0].total;
  }
  // std::cout<<"st="<<st<<std::endl;
  //
  if (st==1) {
    Q[cn].x1=cx; Q[cn].y1=cy; Q[cn].z1=cz;
  }
  if (st==1 || st==2) {
    Q[cn].x2=cx; Q[cn].y2=cy; Q[cn].z2=cz;
  }
  if (st==1 || st==2 || st==3) {
    Q[cn].x3=cx; Q[cn].y3=cy; Q[cn].z3=cz;
  }
  if (st==1 || st==2 || st==3 || st==4) {
    Q[cn].x4=cx; Q[cn].y4=cy; Q[cn].z4=cz;
  }

}

// function that draws all the Quads, which needs to be added in the display()
void drawQuads() {
  // loop over all the Quads in the array
  // for each print the 4 vertices with the respective color
  for (int i=1; i!=Q[0].total+1; i++) {
    glBegin(GL_QUADS);
    glColor3f(Q[i].r,Q[i].g,Q[i].b);
    glVertex3f(Q[i].x1,Q[i].y1,Q[i].z1);
    glVertex3f(Q[i].x2,Q[i].y2,Q[i].z2);
    glVertex3f(Q[i].x3,Q[i].y3,Q[i].z3);
    glVertex3f(Q[i].x4,Q[i].y4,Q[i].z4);
    glEnd();
  }
}

// our cube, which needs to be added in the display()
void theCube() {
  // to allow the cube to be moved
  glPushMatrix();
  // set the colors in order R,G,B as floats from 0.0 to 1.0
  glColor3f(1.0,1.0,1.0);
  // translate the cube to its new coordiates
  glTranslatef(cx,cy,cz);
  // set the size of the cube
  glutSolidCube(0.8);
  // to allow the cube to be moved
  glPopMatrix();
}

// function that draws the grid, which needs to be added in the display()
void drawGrid() {
  // draw one line 40 times
  // the first 20 times, translated in the z axis, towards viewer, out of screen
  // each line horizontal going left to right)
  // the next 20 times, rotate with 270 degrees around the y vertical axis
  // translate towards the right
  // each line horizontal going from far to close to the reader
  // so a for loop 40 times
  for (int i=0; i!=40; i++) {
    glPushMatrix();
    if (i<20) {
      // translate along the z axis, by unit of i
      glTranslatef(0,0,i);
    } else {
      // first translate by a unit i along the x axis
      glTranslatef(i-20,0,0);
      // then rotate by 270 degrees (or -90 degrees) along the y axis
      glRotatef(-90,0,1,0);
    }
    //
    glBegin(GL_LINES);
    glColor3f(1,1,1);
    glLineWidth(1);
    glVertex3f(0,-0.1,0);
    glVertex3f(19,-0.1,0);
    glEnd();
    glPopMatrix();
  }
}

// function that shows what is displayed at one time
void display() {
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();
  // translate to left and closer to us
  glTranslatef(-13,+0,-45);
  // rotate the perspective by 40 degrees
  // around the direction defined by vector from (0,0,0) to (1,1,0)
  glRotatef(40,1,1,0);
  drawGrid();
  drawQuads();
  theCube();
  // done our code so refresh the buffer before going to the new one
  glutSwapBuffers();
}



// function that checks what key is pressed
// and as a function of that changes the position of the cube
void keyboard(unsigned char key, int x, int y) {
  if (key=='w') {
    // goes towards the front, away from viewer, which means decrease the z coordinate by one
    cz-=1;
  } else if (key=='s') {
    // goes towards the back, towards the viewer, which means increase the z coordinate by one
    cz+=1;
  } else if (key=='a') {
    // goes towards the left, which means decrease the x coordinate by one
    cx-=1;
  } else if (key=='d') {
    // goes towards the right, which means increase the x coordinate by one
    cx+=1;
  } else if (key=='q') {
    // goes upwards, which means increase the y coordinate by one
    cy+=1;
  } else if (key=='z') {
    // goes downwards, which means decrease the y coordinate by one
    cy-=1;
  } else if (key==32) {
    // if spacebar key is pressed, add a new Quad
    addQuad();
  } else if (key=='r') {
  // when pressing r, set the color of the current Quad to red
    Q[cn].r=1; Q[cn].g=0; Q[cn].b=0;
  } else if (key=='g') {
  // when pressing g, set the color of the current Quad to green
    Q[cn].r=0; Q[cn].g=1; Q[cn].b=0;
  } else if (key=='b') {
  // when pressing b, set the color of the current Quad to blue
    Q[cn].r=0; Q[cn].g=0; Q[cn].b=1;
  } else if (key=='y') {
  // when pressing y, set the color of the current Quad to yellow
    Q[cn].r=1; Q[cn].g=1; Q[cn].b=0; // yellow = red + green
  }

  // propage the changed position to be displayed
  glutPostRedisplay();

}

void init() {
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(35,1.0f,0.1f,1000);
  glMatrixMode(GL_MODELVIEW);
  glEnable(GL_DEPTH_TEST);
  glClearColor(0.1,0.1,0.1,1);
}

int main(int argc, char **argv) {
  std::cout<<"Test: Will draw a grid and move a cube and build quads!"<<std::endl;

  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE);
  glutInitWindowSize(800, 600);
  glutInitWindowPosition(100, 100);
  glutCreateWindow("");
  glutDisplayFunc(display);
  glutKeyboardFunc(keyboard);
  init();
  glutMainLoop();
  return 0;
}
