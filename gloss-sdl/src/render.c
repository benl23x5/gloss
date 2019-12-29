#include "main.h"

// Render the image.
void    gloss_render
        ( size_t sizeX, size_t sizeY
        , void   *pixels
        , size_t  pitch)
{
        uint64_t counter_start;
        uint64_t counter_end;
        counter_start = SDL_GetPerformanceCounter();

        double posX   = sizeX / 2;
        double posY   = sizeY / 2;

        for (size_t y = 0; y < sizeX; y++)
        for (size_t x = 0; x < sizeY; x++)
        {
                double xd = posX - (double)x;
                double yd = posY - (double)y;

                double d  = sqrt(xd*xd + yd*yd);

                uint32_t* buf
                 = (uint32_t*) ((uint8_t*)pixels + y * pitch + x * 4);

                if (d < sizeX / 4)
                        *buf = 0xff8080ff;
                else    *buf = 0x00000000;
        }

        counter_end     = SDL_GetPerformanceCounter();
        double  time
         = (double)(counter_end - counter_start)
         / (double)(SDL_GetPerformanceFrequency());

        printf  ("time %f\n", time);
}
