#include <SFML/Graphics.hpp>
#include <iostream>

using namespace std;

int main() {
  /* cout << "argc=" << argc << endl;
  cout << "program=" << argv[0] << endl;
  return 0; */

    // Create Window
  sf::RenderWindow window(sf::VideoMode(640, 480), "SFML Application Sivan");

  // Create shapes
  // Circle
  sf::Vector2f posCircle(100.0f, 100.0f);
  float circleIncX = -0.05f;
  sf::CircleShape shapeCircle;
  shapeCircle.setRadius(40.f);
  shapeCircle.setPosition(posCircle);
  shapeCircle.setFillColor(sf::Color::Cyan);

  sf::Vector2f posRect(200.0f, 200.0f);
  sf::RectangleShape shapeRect;
  shapeRect.setSize(sf::Vector2f(20.0f, 45.0f));
  shapeRect.setPosition(posRect);
  shapeRect.setFillColor(sf::Color::Yellow);

    int drawNum = 0;
  while (window.isOpen()) {
    drawNum++;
    sf::Event event;
    while (window.pollEvent(event)) {
      if (event.type == sf::Event::Closed) window.close();
    }

    window.clear();

    posCircle.x += circleIncX;
    if (posCircle.x < 0.0f)
        circleIncX *= -1.0f;
    else if ((posCircle.x + (2.0f * shapeCircle.getRadius())) > window.getSize().x)
        circleIncX *= -1;

    shapeCircle.setPosition(posCircle);
    window.draw(shapeCircle);
    window.draw(shapeRect);
    window.display();
  }

  return 0;
}
