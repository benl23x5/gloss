
#include <assert.h>
#include <SDL2/SDL.h>
#include <sys/time.h>
#include "main.h"

bool            debug_init                      = false;
bool            debug_events                    = false;

SDL_Window*     state_window                    = 0;
SDL_Renderer*   state_renderer                  = 0;
SDL_Texture*    state_texture                   = 0;
SDL_Surface*    state_surface                   = 0;
TTF_Font*       state_font_DejaVuSans           = 0;
TTF_Font*       state_font_DejaVuSansMono       = 0;

int main(int argc, char** argv)
{
        size_t  winSizeX        = 600;
        size_t  winSizeY        = 600;

        // ------------------------------------------------
        // Initialize SDL and open the main window.
        size_t bufSizeX          = 0;
        size_t bufSizeY          = 0;
        gloss_init
                ( winSizeX, winSizeY
                , &bufSizeX, &bufSizeY);

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
                        if (debug_events)
                                printf("  %6d window shown\n", iEvent);
                        break;

                      default:
                        if (debug_events)
                                printf("  %6d window unhandled\n", iEvent);
                        break;
                    }
                    break;

                    default:
                        if (debug_events)
                                printf("  %6d unhandled\n", iEvent);
                        break;
                  }

                // Lock texture while we're writing to it.
                SDL_LockTexture
                        ( state_texture, 0
                        , &state_surface->pixels
                        , &state_surface->pitch);

                double time
                 = gloss_render
                        ( bufSizeX, bufSizeY
                        , state_surface->pixels
                        , state_surface->pitch);

                // Unlock texture now that we've finished writing.
                SDL_UnlockTexture (state_texture);

                // Write the frame creation time to the buffer.
                char str[256];
                snprintf(str, 256, "time %2.6f", time);

                SDL_Color color = { 0xa0, 0xa0, 0xa0 };
                SDL_Surface *surface_text
                 = TTF_RenderUTF8_Blended
                        ( state_font_DejaVuSansMono
                        , str, color);
                assert(surface_text != 0);

                SDL_BlitSurface(surface_text, 0, state_surface, 0);
                SDL_FreeSurface(surface_text);

                // Copy the texture to the display.
                SDL_RenderClear   (state_renderer);
                SDL_RenderCopy    (state_renderer, state_texture, 0, 0);
                SDL_RenderPresent (state_renderer);
        }
        quit:

        SDL_DestroyRenderer(state_renderer);
        SDL_Quit();
        return 0;
}


