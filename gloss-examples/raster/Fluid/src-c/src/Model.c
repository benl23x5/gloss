
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "Model.h"
#include "Solver.h"

#define IX(i,j) ((i)+(N+2)*(j))


// Free a model.
void
model_free (struct Model* model)
{
        free (model->u);
        free (model->u_prev);
        free (model->v);
        free (model->v_prev);
        free (model->dens);
        free (model->dens_prev);
}


// Clear all data in a model.
void
model_clear (struct Model* model)
{
        int i;
        for ( i=0 ; i < model->size ; i++ ) {
                model->u[i]
                 = model->v[i]
                 = model->u_prev[i]
                 = model->v_prev[i]
                 = model->dens[i]
                 = model->dens_prev[i]
                 = 0.0f;
        }
}


// Create a new model.
struct Model*
model_new (int width, int height)
{
        assert (width  > 0);
        assert (height > 0);

        int size         = (width + 2) * (height + 2);
        int bytes        = size        * sizeof(float);

        struct Model*   model   = malloc(sizeof(struct Model));
        assert(model);
        model->size      = size;
        model->height    = height;
        model->width     = width;
        model->u         = (float *) malloc (bytes);
        model->u_prev    = (float *) malloc (bytes);
        model->v         = (float *) malloc (bytes);
        model->v_prev    = (float *) malloc (bytes);
        model->dens      = (float *) malloc (bytes);
        model->dens_prev = (float *) malloc (bytes);
        model->delta     = 0.1;
        model->diff      = 0;
        model->visc      = 0;

        assert ( model->u    && model->u_prev
              && model->v    && model->v_prev
              && model->dens && model->dens_prev);

        model_clear (model);
        return model;
}


void
dump_array (int step_count, char* name, int N, float scale, float* d)
{
        int i, j;
        char str[256];

        // find maximum value in image.
        float max = 0;
        for (j = 1; j < N+1; j++) {
                for (i = 1; i < N+1; i++) {
                        float d00       = d[IX(i, j)] * 255 * scale;
                        if (d00 >= max) max = d00;
                }
        }

        // Write out the file
        snprintf(str, 256, "out/%04d-%s.ppm", step_count, name);
        FILE* file = fopen (str, "w+");
        fprintf (file, "P2\n");
        fprintf (file, "%d %d\n", N, N);
        fprintf (file, "%d\n", (int)max);


        // Write out image file.
        for (j = N; j >= 1; j--) {
               for (i = 1; i < N+1; i++) {
                        float d00       = d[IX(i, j)] * 255 * scale;
                        fprintf (file, "% 4d ", (int)d00);
                }

                fprintf(file, "\n");
        }

        fclose(file);
}


// -- Steps -------------------------------------------------------------------
void dens_step
        ( int step
        , int method
        , int iters, int N
        , float* x,  float* x0
        , float* u,  float* v
        , float diff, float dt)
{
        add_source (N, x, x0, dt );

        SWAP (x0, x);
        diffuse (method, iters, N, 0, x, x0, diff, dt);

        SWAP    (x0, x);
        advect  (N, 0, x, x0, u, v, dt);
}


void vel_step
        ( int step
        , int method
        , int iters,  int N
        , float* u,   float* v
        , float* u0,  float* v0
        , float visc, float dt)
{
        add_source ( N, u, u0, dt );
        add_source ( N, v, v0, dt );

        SWAP    (u0, u);
        diffuse (method, iters, N, 1, u, u0, visc, dt);

        SWAP    (v0, v);
        diffuse (method, iters, N, 2, v, v0, visc, dt);

        project (method, iters, N, u, v, u0, v0);
        SWAP    (u0, u);
        SWAP    (v0, v);

        advect  (N, 1, u, u0, u0, v0, dt);
        advect  (N, 2, v, v0, u0, v0, dt);

        project (method, iters, N, u, v, u0, v0);
}

