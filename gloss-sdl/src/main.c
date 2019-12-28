
#include <assert.h>
#include <SDL2/SDL.h>
#include "main.h"

void    gloss_render
        ( size_t sizeX, size_t sizeY
        , void  *pixels
        , size_t pitch);


int main(int argc, char** argv)
{
        size_t  winSizeX        = 600;
        size_t  winSizeY        = 600;

        // ------------------------------------------------
        // Initialize SDL and open the main window.
        size_t bufSizeX          = 0;
        size_t bufSizeY          = 0;
        SDL_Window*     window   = 0;
        SDL_Renderer*   renderer = 0;
        SDL_Texture*    texture  = 0;
        SDL_Surface*    surface  = 0;
        gloss_init
                ( winSizeX, winSizeY
                , &bufSizeX, &bufSizeY
                , &window,  &renderer, &texture, &surface);


        // ------------------------------------------------
        // The main drawing loop.

        // Event counter.
        int iEvent = 0;

        while (1) {
                SDL_Event event;

                SDL_WaitEvent(&event);
                iEvent++;

                switch (event.type)
                { case SDL_QUIT:
                        goto quit;

                  case SDL_MOUSEMOTION:
                        // printf("  motion\n");
                        continue;

                  case SDL_WINDOWEVENT:
                    switch (event.window.event)
                    { case SDL_WINDOWEVENT_SHOWN:
                        printf("  %6d window shown\n", iEvent);
                        break;

                      default:
                        printf("  %6d window unhandled\n", iEvent);
                        break;
                    }
                    break;

                    default:
                        printf("  %6d unhandled\n", iEvent);
                        break;
                  }

                // Lock texture while we're writing to it.
                SDL_LockTexture
                        ( texture, 0
                        , &surface->pixels
                        , &surface->pitch);

                gloss_render
                        ( bufSizeX, bufSizeY
                        , surface->pixels
                        , surface->pitch);

                // Unlock texture now that we've finished writing.
                SDL_UnlockTexture (texture);

                // Copy the texture to the display.
                SDL_RenderClear   (renderer);
                SDL_RenderCopy    (renderer, texture, 0, 0);
                SDL_RenderPresent (renderer);
        }

        quit:

        SDL_DestroyRenderer(renderer);
        SDL_Quit();
        return 0;
}

void    gloss_render
        ( size_t sizeX, size_t sizeY
        , void   *pixels
        , size_t  pitch)
{
        double posX   = sizeX / 2;
        double posY   = sizeY / 2;

        for (size_t y = 0; y < sizeX; y++)
        for (size_t x = 0; x < sizeY; x++)
        {
                double xd = posX - (double)x;
                double yd = posY - (double)y;

                double d = sqrt(xd*xd + yd*yd);

                uint32_t* buf
                 = (uint32_t*) ((uint8_t*)pixels + y * pitch + x * 4);

                if (d < sizeX / 4)
                        *buf = 0xff8080ff;
                else    *buf = 0x00000000;
        }
}
