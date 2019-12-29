#pragma once

#include <assert.h>
#include <stdbool.h>
#include <SDL2/SDL.h>

void    gloss_init
        ( bool    bDebug
        , size_t  winSizeX,    size_t  winSizeY
        , size_t* outBufSizeX, size_t* outBufSizeY
        , SDL_Window**   outWindow
        , SDL_Renderer** outRenderer
        , SDL_Texture**  outTexture
        , SDL_Surface**  outSurface);

void    gloss_render
        ( size_t sizeX, size_t sizeY
        , void   *pixels
        , size_t  pitch);
