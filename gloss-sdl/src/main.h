#pragma once

#include <assert.h>
#include <stdbool.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>

extern bool             debug_init;
extern bool             debug_events;

extern SDL_Window*      state_window;
extern SDL_Renderer*    state_renderer;
extern SDL_Texture*     state_texture;
extern SDL_Surface*     state_surface;

extern TTF_Font*        state_font_DejaVuSans;
extern TTF_Font*        state_font_DejaVuSansMono;

void    gloss_init
        ( size_t  winSizeX,    size_t  winSizeY
        , size_t* outBufSizeX, size_t* outBufSizeY);

double  gloss_render
        ( size_t sizeX, size_t sizeY
        , void   *pixels
        , size_t  pitch);
