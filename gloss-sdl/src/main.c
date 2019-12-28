
#include <assert.h>
#include <SDL2/SDL.h>


int main(int argc, char** argv)
{
        SDL_Window      *win    = 0;
        SDL_Renderer    *ren    = 0;
        SDL_Texture     *tex    = 0;

        size_t  winSizeX        = 1024;
        size_t  winSizeY        = 768;

        // Init video subsystem.
        assert (SDL_Init (SDL_INIT_VIDEO) == 0);
        assert (SDL_InitSubSystem (SDL_INIT_VIDEO) == 0);

        // Open the main window.
        win     = SDL_CreateWindow
                ( "Test"                        // window title.
                , SDL_WINDOWPOS_UNDEFINED       // x position of window.
                , SDL_WINDOWPOS_UNDEFINED       // y position of window.
                , winSizeX, winSizeY            // window size.
                , SDL_WINDOW_RESIZABLE);
        assert (win != 0);


        ren     = SDL_CreateRenderer
                ( win                           // attach to this window.
                , -1                            // use first available driver.
                , SDL_RENDERER_SOFTWARE);

        // Make a new texture buffer for the window.
        tex     = SDL_CreateTexture
                ( ren
                , SDL_PIXELFORMAT_RGBA8888
                , SDL_TEXTUREACCESS_TARGET
                , winSizeX, winSizeY);
        assert (tex != 0);

        // Spin in a loop drawing random rectangles.
        while (1) {
                SDL_Event event;

                SDL_Rect  rect  = {.w = 100, .h = 50};

                SDL_PollEvent(&event);
                if (event.type == SDL_QUIT)
                        break;
                rect.x = rand() % 500;
                rect.y = rand() % 500;

                SDL_SetRenderTarget     (ren, tex);

                SDL_SetRenderDrawColor  (ren, 0x00, 0x00, 0x00, 0x00);
                SDL_RenderClear         (ren);

                SDL_RenderDrawRect      (ren, &rect);
                SDL_SetRenderDrawColor  (ren, 0xff, 0xff, 0xff, 0x00);
                SDL_RenderFillRect      (ren, &rect);
                SDL_SetRenderTarget     (ren, 0);
                SDL_RenderCopy          (ren, tex, 0, 0);
                SDL_RenderPresent       (ren);
        }

        SDL_DestroyRenderer(ren);
        SDL_Quit();
        return 0;
}