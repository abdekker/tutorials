#pragma once

// Data types and structure definitions
#include <vector>

// Type definitions
typedef std::vector<int> ARRAY;
typedef std::vector< std::vector<int>> MATRIX;

typedef std::vector<unsigned int> ARRAY_U;
typedef std::vector< std::vector<unsigned int>> MATRIX_U;

// Data structures

// Simple (x,y) point. Usage example: POINT<int> location = {1, 2};
template <typename T>
struct POINT {
	// Data
	T x, y;

	// Construction
	POINT abs() {
		POINT newPt;
		newPt.x = (this->x > 0) ? this->x : -this->x;
		newPt.y = (this->y > 0) ? this->y : -this->y;
		return newPt;
	}

	// Operators
	void operator= (const POINT &pt) { this->x = pt.x; this->y = pt.y; }

	void operator+ (const POINT &pt) { this->x += pt.x; this->y += pt.y; }
	void operator+= (const POINT &pt) { this->x += pt.x; this->y += pt.y; }

	void operator- (const POINT &pt) { this->x -= pt.x; this->y -= pt.y; }
	void operator-= (const POINT &pt) { this->x -= pt.x; this->y -= pt.y; }

	void operator* (const T factor) { this->x *= factor; this->y *= factor;}
	void operator*= (const T factor) { this->x *= factor; this->y *= factor; }

	bool operator< (const POINT &pt) { return ((this->x < pt.x) || (this->y < pt.y)); }
	bool operator> (const POINT &pt) { return ((this->x > pt.x) || (this->y > pt.y)); }

	// Transformations
	void transformSwap() { T tmp = this->x; this->x = this->y; this->y = tmp; }
	void transformAbs() {
		this->x = (this->x >= 0) ? this->x : -this->x;
		this->y = (this->y >= 0) ? this->y : -this->y;
	}
};
