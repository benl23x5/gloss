#pragma once


#define SWAP(x0,x) {float * tmp=x0;x0=x;x=tmp;}

void    add_source      (int N, float* x, float* s, float dt);
void    set_bnd         (int N, int b,  float* x);
void    copy            (int N, float*  d, float* d0);

void    diffuse         (int method, int iters, int N, int b,  float* x, float* x0, float diff, float dt);
void    advect          (int N, int b,  float* d, float* d0, float* u, float* v, float dt);
void    project (int method, int iters, int N, float * u, float * v, float * p, float * div );

