
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define IX(i,j) ((i)+(N+2)*(j))
#define SWAP(x0,x) {float * tmp=x0;x0=x;x=tmp;}
#define FOR_EACH_CELL(BODY) for ( i=1 ; i <= N ; i++ ) { for ( j=1 ; j <= N ; j++ ) { BODY }}

// ----------------------------------------------------------------------------
void    add_source      (int N, float* x, float* s, float dt);
void    set_bnd         (int N, int b,  float* x);
void    copy            (int N, float*  d, float* d0);

void    diffuse         (int method, int iters, int N, int b,  float* x, float* x0, float diff, float dt);
void    advect          (int N, int b,  float* d, float* d0, float* u, float* v, float dt);


void
print_array (int N, float* arr)
{
        int i, j;

        for (j = N; j >= 1; j--) {
                for (i = 1; i <= N; i++)
                        printf ("% f ", arr[IX(i, j)]);
                printf ("\n");
        }

        printf("\n");
}

// ----------------------------------------------------------------------------
void
add_source (int N, float * x, float * s, float dt)
{
        int i, size=(N+2)*(N+2);
        for ( i=0 ; i<size ; i++ )
                x[i] += dt * s[i];
}


// -- Boundary Conditions -----------------------------------------------------
void
set_bnd_zero ( int N, int b, float * x )
{
        int i;

        for ( i = 1 ; i <= N ; i++ ) {
                x[IX(0  ,i)] = 0;
                x[IX(N+1,i)] = 0;
                x[IX(i,0  )] = 0;
                x[IX(i,N+1)] = 0;
        }
        x[IX(0  ,0  )] = 0;
        x[IX(0  ,N+1)] = 0;
        x[IX(N+1,0  )] = 0;
        x[IX(N+1,N+1)] = 0;
}


void set_bnd_box (int N, int b, float * x)
{
        int i;

        for ( i=1 ; i<=N ; i++ ) {
                x[IX(0  ,i)] = b==1 ? -x[IX(1,i)] : x[IX(1,i)];
                x[IX(N+1,i)] = b==1 ? -x[IX(N,i)] : x[IX(N,i)];
                x[IX(i,0  )] = b==2 ? -x[IX(i,1)] : x[IX(i,1)];
                x[IX(i,N+1)] = b==2 ? -x[IX(i,N)] : x[IX(i,N)];
        }
        x[IX(0  ,0  )] = 0.5f * (x[IX(1,0  )] + x[IX(0  ,1)]);
        x[IX(0  ,N+1)] = 0.5f * (x[IX(1,N+1)] + x[IX(0  ,N)]);
        x[IX(N+1,0  )] = 0.5f * (x[IX(N,0  )] + x[IX(N+1,1)]);
        x[IX(N+1,N+1)] = 0.5f * (x[IX(N,N+1)] + x[IX(N+1,N)]);
}


void set_bnd (int N, int b, float* x)
{
        set_bnd_zero (N, b, x);
}


// -- Linear Solver -----------------------------------------------------------
void
solve_gauss_seidel (int iters, int N, int b, float* x, float* x0, float a, float c)
{
        int i, j, k;
        for (k=0; k < iters; k++) {
                FOR_EACH_CELL(
                        x[IX(i,j)] = ( x0[IX(i,j)]
                                     + a * (x[IX(i-1,j)]
                                          + x[IX(i+1,j)]
                                          + x[IX(i,j-1)]
                                          + x[IX(i,j+1)]))/c;
                )
                set_bnd (N, b, x);
        }
}


void
solve_jacobi (int iters, int N, int b, float* x, float* x0, float a, float c)
{
        int i, j, k;
        float* x1       = malloc((N+2)*(N+2)*sizeof(float));
        float* x2       = malloc((N+2)*(N+2)*sizeof(float));
        copy (N, x1, x);

        set_bnd(N, b, x1);
        for (k = 0; k < iters; k++) {
                FOR_EACH_CELL(
                        x2[IX(i, j)]
                                = ( x0[IX(i, j)]
                                  + a * (x1[IX(i-1, j)]
                                       + x1[IX(i+1, j)]
                                       + x1[IX(i, j-1)]
                                       + x1[IX(i, j+1)]))/c;
                )
                set_bnd(N, b, x2);
                SWAP(x1, x2)
        }

        copy (N, x, x1);

        free (x1);
        free (x2);
}


void lin_solve (int method, int iters, int N, int b, float* x, float* x0, float a, float c)
{
        switch (method) {
        case 0: solve_gauss_seidel(iters, N, b, x, x0, a, c);
                break;

        case 1: solve_jacobi(iters, N, b, x, x0, a, c);
                break;

        default:
                abort();
        }
}


// -- Diffusion ---------------------------------------------------------------
void
diffuse (int method, int iters, int N, int b, float *x, float *x0, float diff, float dt )
{
        float a = dt * diff * N * N;
        lin_solve (method, iters, N, b, x, x0, a, 1 + 4 * a);
}


// -- Advection ---------------------------------------------------------------
void
advect (int N, int b, float * d, float * d0, float * u, float * v, float dt )
{
        assert (d); assert (d0); assert (u); assert (v);

        int   i, j, i0, j0, i1, j1;
        float x, y, s0, t0, s1, t1, dt0;

        dt0 = dt * N;
        for ( j=1 ; j <= N ; j++ )
        for ( i=1 ; i <= N ; i++ ) {

                x = i - dt0 * u[IX(i,j)];
                y = j - dt0 * v[IX(i,j)];

                if (x < 0.5f)   x = 0.5f;
                if (x > N+0.5f) x = N+0.5f;

                i0=(int)x;
                i1=i0+1;

                if (y < 0.5f)   y = 0.5f;
                if (y > N+0.5f) y = N + 0.5f;

                j0=(int)y;
                j1=j0+1;

                s1 = x-i0;
                s0 = 1-s1;

                t1 = y-j0;
                t0 = 1-t1;

                d[IX(i,j)] = s0 * (t0 * d0[IX(i0,j0)] + t1 * d0[IX(i0,j1)])
                           + s1 * (t0 * d0[IX(i1,j0)] + t1 * d0[IX(i1,j1)]);
        }
        set_bnd ( N, b, d );
}

void copy (int N, float * d, float * d0)
{
        assert (d); assert (d0);
        int i, j;

        for ( i=1 ; i <= N ; i++ )
        for ( j=1 ; j <= N ; j++ ) {
                d[IX(i,j)] = d0[IX(i,j)];
        }
}


// -- Projection --------------------------------------------------------------
void project (int method, int iters, int N, float * u, float * v, float * p, float * div )
{
        int i, j;

        FOR_EACH_CELL(
                div[IX(i,j)] = -0.5f*(u[IX(i+1,j)]-u[IX(i-1,j)]+v[IX(i,j+1)]-v[IX(i,j-1)])/N;
                p[IX(i,j)]   = div[IX(i,j)];
        )
        set_bnd ( N, 0, div );
        set_bnd ( N, 0, p );

        lin_solve (method, iters, N, 0, p, div, 1, 4 );

        FOR_EACH_CELL(
                u[IX(i,j)] -= 0.5f*N*(p[IX(i+1,j)]-p[IX(i-1,j)]);
                v[IX(i,j)] -= 0.5f*N*(p[IX(i,j+1)]-p[IX(i,j-1)]);
        )
        set_bnd ( N, 1, u );
        set_bnd ( N, 2, v );
}

