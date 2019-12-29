#include "main.h"

typedef struct {
        float x;
        float y;
} point_t;


// Check if a point is left/on/right of an infinite line.
// Given three points p0 p1 p2, where p0 p1 define the line.
//   returns >0 when p2 is left of the line p0 p1.
//           =0 when p2 is on the line.
//           <0 when p2 is right of the line p0 p1.
inline float
gloss_isLeft (point_t p0, point_t p1, point_t p2)
{
        return  (p1.x - p0.x) * (p2.y - p0.y)
              - (p2.x - p0.x) * (p1.y - p0.y);
}


// Compute the winding number of a point in a polygon.
//
//   We take the point we are testing (p),
//   an array containing the vertices (v),
//   and the number of vertices in the polygon (n).
//
//   The array contains the polygon vertices in clockwise order,
//   followed by an extra copy of the first vertex.
//   The length of the array is *one more* than the value n.
//
//   The winding number will be 0 if p is outside the polygon.
int     gloss_windingNumber
        ( point_t  p    // the point we are testing
        , point_t* v    // array of vertices of length (n + 1)
        , int n)        // number of vertices in the polygon.
{
        // winding number counter
        int    wn = 0;

        // loop through all edges of the polygon
        for (int i = 0; i < n; i++) {           // edge from V[i] to  V[i+1]
            if (v[i].y <= p.y) {                // start y <= P.y
                if (v[i+1].y > p.y)             // an upward crossing
                     if (gloss_isLeft (v[i], v[i+1], p) > 0)  // P left of  edge
                         wn++;                  // have  a valid up intersect
            }
            else {                              // start y > P.y (no test needed)
                if (v[i+1].y  <= p.y)           // a downward crossing
                     if (gloss_isLeft (v[i], v[i+1], p) < 0)  // P right of  edge
                         wn--;                  // have  a valid down intersect
            }
        }
        return wn;
}


// Check if a point is within a general polygon.
//   The edges must be connected in clockwise order.
//   We compute the winding number and return whether that is positive.
//   This works for arbitrary polygons.
bool    gloss_pointInPoly
        ( point_t  p
        , point_t* v
        , int n)
{
        int wn  = gloss_windingNumber(p, v, n);
        return (wn > 0);
}


// Check if a point is within a convex polygon.
//   The edges must be connected in clockwise order.
//   We check if the point is on the right of every connecting edge.
//   This only works for convex polygons.
bool    gloss_pointInConvexPoly
        ( point_t  p
        , point_t* v
        , int n)
{
        float px = p.x;
        float py = p.y;

        float x = 1;
        for (int i = 0; i < n; i++)
        {       float ax    = v[i].x;
                float ay    = v[i].y;

                float bx    = v[i+1].x;
                float by    = v[i+1].y;

                // determinate of vectors (ab,ap)
                float det   = (bx - ax) * (py - ay)
                            - (by - ay) * (px - ax);

                float right = ceilf(fmax(fmin(det,1),0));
                x *= right;
        }
        return x >= 1;
}


point_t shape_p[]
        = { {500, 300}, {800, 800}, {200, 800}, {500, 300} };
int shape_n = 3;



// Render the image.
double  gloss_render
        ( size_t sizeX, size_t sizeY
        , void   *pixels
        , size_t  pitch)
{
        uint64_t counter_start;
        uint64_t counter_end;
        counter_start = SDL_GetPerformanceCounter();

        for (size_t y = 0; y < sizeX; y++)
        for (size_t x = 0; x < sizeY; x++)
        {
                uint32_t* buf
                 = (uint32_t*) ((uint8_t*)pixels + y * pitch + x * 4);

                bool b  = gloss_pointInPoly
                        ( (point_t){.x = x, .y = y}
                        , shape_p, shape_n );

                if (b)
                        *buf = 0xff8080ff;
                else    *buf = 0x00000000;
        }

        counter_end     = SDL_GetPerformanceCounter();
        double  time
         = (double)(counter_end - counter_start)
         / (double)(SDL_GetPerformanceFrequency());

        return time;
}
