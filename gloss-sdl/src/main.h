#pragma once

void    gloss_init
        ( size_t  winSizeX,    size_t  winSizeY
        , size_t* outBufSizeX, size_t* outBufSizeY
        , SDL_Window**   outWindow
        , SDL_Renderer** outRenderer
        , SDL_Texture**  outTexture
        , SDL_Surface**  outSurface);
