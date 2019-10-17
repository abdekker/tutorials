#ifndef TEST_SYSTEM_HPP
#define TEST_SYSTEM_HPP

class Rectangle
{
    public:
        Rectangle(int inputWidth, int inputHeight);
        int area() { return (width*height); }

    private:
        int width, height;
};

#endif	//TEST_SYSTEM_HPP
