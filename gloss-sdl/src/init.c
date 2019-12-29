#include "main.h"

void    gloss_init
        ( size_t  winSizeX, size_t winSizeY
        , size_t* outBufSizeX, size_t* outBufSizeY)
{
        // ------------------------------------------------
        // Dump the SDL versions for debugging.
        if (debug_init)
        { SDL_version     version;
          SDL_GetVersion  (&version);
          printf ( "SDL version     = %d.%d.%d\n"
                 , version.major, version.minor, version.patch);

          SDL_TTF_VERSION(&version);
          printf ( "SDL ttf version = %d.%d.%d\n"
                 , version.major, version.minor, version.patch);
        }

        // ------------------------------------------------
        // Initialize SDL_ttf library and load our font.
        assert(TTF_Init() == 0);

        state_font_DejaVuSans
         = TTF_OpenFont ("fonts/DejaVuSans.ttf", 24);
        assert (state_font_DejaVuSans != 0);

        state_font_DejaVuSansMono
         = TTF_OpenFont ("fonts/DejaVuSansMono.ttf", 24);
        assert (state_font_DejaVuSansMono != 0);

        // ------------------------------------------------
        // Initialize the video subsystem.
        assert (SDL_Init (SDL_INIT_VIDEO) == 0);
        assert (SDL_InitSubSystem (SDL_INIT_VIDEO) == 0);

        if (debug_init)
        { int numDisplays = SDL_GetNumVideoDisplays();
          for (int iDisplay = 0; iDisplay < numDisplays; iDisplay++)
          {       float ddpi, hdpi, vdpi;
                SDL_Rect bounds;

                assert  (SDL_GetDisplayDPI    (iDisplay, &ddpi, &hdpi, &vdpi) == 0);
                assert  (SDL_GetDisplayBounds (iDisplay, &bounds) == 0);

                printf  ("display %d\n", iDisplay);
                printf  ("  dpi      = { .ddpi = %3.2f, .hdpi = %3.2f, .vdpi = %3.2f }\n"
                        , ddpi, hdpi, vdpi);
                printf  ("  bounds   = { .x = %d, .y = %d, .w = %d, .h = %d }\n"
                        , bounds.x, bounds.y, bounds.w, bounds.h);

                int numDisplayModes = SDL_GetNumDisplayModes (iDisplay);
                for (int iMode = 0; iMode < numDisplayModes; iMode++)
                {       SDL_DisplayMode mode;
                        assert  (SDL_GetDisplayMode(iDisplay, iMode, &mode) == 0);
                        printf  ("  mode %2d  = { .w = %4d, .h = %4d, .format = %s }\n"
                                , iMode
                                , mode.w, mode.h
                                , SDL_GetPixelFormatName(mode.format));
                }
          }
        }

        // ------------------------------------------------
        // Create the main window.
        //   Allowing high DPI windows will cause windows opened on a
        //   high DPI display to have a larger drawable size than the
        //   abstract "window size" that we have specified in screen
        //   coordinates.
        state_window
         = SDL_CreateWindow
                ( "Test"                        // window title.
                , SDL_WINDOWPOS_UNDEFINED       // x position of window.
                , SDL_WINDOWPOS_UNDEFINED       // y position of window.
                , winSizeX, winSizeY            // window size.
                , SDL_WINDOW_ALLOW_HIGHDPI);
        assert (state_window != 0);

        // Check reported size of window.
        //   This should match winSizeX/Y, even for high DPI windows.
        int     winSizeReportedX = 0;
        int     winSizeReportedY = 0;
        SDL_GetWindowSize
                ( state_window
                , &winSizeReportedX, &winSizeReportedY);

        if (debug_init)
        { printf ("window size\n");
          printf ("  reported = { .w = %d, .h = %d }\n", winSizeReportedX, winSizeReportedY);
        }

        assert (  winSizeX == winSizeReportedX
               && winSizeY == winSizeReportedY);

        // ------------------------------------------------
        // Check drawable size of the main window window.
        //   If we have a high DPI window then this will be larger than the
        //   winSizeX/Y that we asked for when we created it.
        int     winSizeDrawableX = 0;
        int     winSizeDrawableY = 0;
        SDL_GL_GetDrawableSize
                ( state_window
                , &winSizeDrawableX, &winSizeDrawableY);

        if (debug_init)
        { printf( "  drawable = { .w = %d, .h = %d }\n"
                , winSizeDrawableX, winSizeDrawableY);
        }

        // ------------------------------------------------
        // List the available renderers.
        if (debug_init)
        { int     numRenderDrivers = SDL_GetNumRenderDrivers();
          for (int i = 0; i < numRenderDrivers; i++)
          {     SDL_RendererInfo info;
                assert  (SDL_GetRenderDriverInfo(i, &info) == 0);
                printf  ( "renderer %d = \"%s\"\n"
                        , i, info.name);
          }
        }

        // Create a new renderer context for the window.
        state_renderer
         = SDL_CreateRenderer
                ( state_window                  // attach to this window.
                , -1                            // use first available driver.
                , SDL_RENDERER_ACCELERATED);
        assert (state_renderer != 0);

        // ------------------------------------------------
        // Check output size of the renderer.
        //   For high DPI windows this will be either the original winSizeX/Y
        //   or the larger winSizeDrawableX/Y depending on the renderer.
        //   On OSX only the accelerated renderers produce High DPI output.
        int     outSizeReportedX = 0;
        int     outSizeReportedY = 0;
        SDL_GetRendererOutputSize
                ( state_renderer
                , &outSizeReportedX, &outSizeReportedY);

        if (debug_init)
        { printf("output size\n");
          printf("  reported = %d, %d\n"
                , outSizeReportedX, outSizeReportedY);
        }

        // ------------------------------------------------
        // Make a new texture buffer to back the window.
        //
        //   The texture buffer should have the same size as the renderer
        //   output size. In the case where we got a high DPI window,
        //   but a software renderer with a lower output size, we still
        //   want use the lower ouptut size for the texture, otherwise we
        //   will waste time creating pixels that will just be discarded
        //   when the texture is copied to the screen.
        //
        //   The pixel format is set to ARGB8888 as in the current version
        //   of SDL that seems to be the native format for every display mode
        //   (obtained with SDL_GetDisplayMode above).
        state_texture
         = SDL_CreateTexture
                ( state_renderer
                , SDL_PIXELFORMAT_ARGB8888
                , SDL_TEXTUREACCESS_STREAMING
                , outSizeReportedX, outSizeReportedY);
        assert (state_texture != 0);

        // ------------------------------------------------
        // Make a RGB surface that we can write into.
        state_surface
         = SDL_CreateRGBSurface
                ( 0             // flags are unused and set to 0.
                , outSizeReportedX, outSizeReportedY
                , 32            // bit depth
                , 0x00ff0000    // red mask
                , 0x0000ff00    // blue mask
                , 0x000000ff    // green mask
                , 0xff000000);  // alpha mask
        assert (state_surface != 0);

        *outBufSizeX    = outSizeReportedX;
        *outBufSizeY    = outSizeReportedY;
}
