
#include <assert.h>
#include <SDL2/SDL.h>


int main(int argc, char** argv)
{
        SDL_Window      *window   = 0;
        SDL_Renderer    *renderer = 0;
        SDL_Texture     *texture  = 0;
        SDL_Surface     *surface  = 0;

        size_t  winSizeX        = 600;
        size_t  winSizeY        = 600;

        // Init video subsystem.
        assert (SDL_Init (SDL_INIT_VIDEO) == 0);
        assert (SDL_InitSubSystem (SDL_INIT_VIDEO) == 0);

        // Open the main window.
        window  = SDL_CreateWindow
                ( "Test"                        // window title.
                , SDL_WINDOWPOS_UNDEFINED       // x position of window.
                , SDL_WINDOWPOS_UNDEFINED       // y position of window.
                , winSizeX, winSizeY            // window size.
                , SDL_WINDOW_RESIZABLE);
        assert (window != 0);

        // Create a new renderer context for the window.
        renderer = SDL_CreateRenderer
                ( window                        // attach to this window.
                , -1                            // use first available driver.
                , SDL_RENDERER_SOFTWARE);
        assert (renderer != 0);

        // Make a new texture buffer for the window.
        texture = SDL_CreateTexture
                ( renderer
                , SDL_PIXELFORMAT_ARGB8888
                , SDL_TEXTUREACCESS_STREAMING
                , winSizeX, winSizeY);
        assert (texture != 0);

        // Make a RGB surface that we can write into.
        surface = SDL_CreateRGBSurface
                ( 0
                , winSizeX, winSizeY
                , 32
                , 0x00ff0000
                , 0x0000ff00
                , 0x000000ff
                , 0xff000000);
        assert (surface != 0);

        // Spin in a loop drawing random rectangles.
        while (1) {
                SDL_Event event;

                SDL_PollEvent(&event);
                if (event.type == SDL_QUIT)
                        break;

                // Lock texture while we're writing to it.
                SDL_LockTexture
                        ( texture, 0
                        , &surface->pixels
                        , &surface->pitch);

                double posX   = 300;
                double posY   = 300;

                for (size_t y = 0; y < winSizeY; y++)
                for (size_t x = 0; x < winSizeX; x++)
                {
                        double xd = posX - (double)x;
                        double yd = posY - (double)y;

                        double d = sqrt(xd*xd + yd*yd);

                        uint32_t* buf
                         = (uint32_t*) ((uint8_t*)surface->pixels + y * surface->pitch + x * 4);

                        if (d < 100)
                                *buf = 0xff8080ff;
                        else    *buf = 0x00000000;
                }

                // Unlock texture now that we've finished writing.
                SDL_UnlockTexture (texture);

                // Copy the texture to the display.
                SDL_RenderClear   (renderer);
                SDL_RenderCopy    (renderer, texture, 0, 0);
                SDL_RenderPresent (renderer);
        }

        SDL_DestroyRenderer(renderer);
        SDL_Quit();
        return 0;
}