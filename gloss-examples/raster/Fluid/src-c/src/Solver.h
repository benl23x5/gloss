#pragma once

extern void
dens_step ( int method
          , int iters,  int N
          , float* x,   float* x0
          , float* u,   float* v
          , float diff, float dt);

extern void
vel_step  ( int method
          , int iters,  int N
          , float* u,   float* v
          , float* u0,  float* v0
          , float visc, float dt);
