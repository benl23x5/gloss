
#include <assert.h>
#include <SDL2/SDL.h>
#include <sys/time.h>
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
                ( false
                , winSizeX, winSizeY
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


