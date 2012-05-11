
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "Model.h"

#define IXN(N,i,j) ((i)+(N+2)*(j))


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
model_clean (struct Model* model)
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
        struct Model*   model   = malloc(sizeof(struct Model));
        model->size      = (model->width+2)*(model->height+2);
        model->height    = height;
        model->width     = width;

        int bytes        = model->size * sizeof(float);

        model->u         = (float *) malloc (bytes);
        model->v         = (float *) malloc (bytes);
        model->u_prev    = (float *) malloc (bytes);
        model->v_prev    = (float *) malloc (bytes);
        model->dens      = (float *) malloc (bytes);
        model->dens_prev = (float *) malloc (bytes);

        assert ( model->u    && model->u_prev
              && model->v    && model->v_prev
              && model->dens && model->dens_prev);

        return model;
}


void 
dump_density ( int step_count, int N, float * d )
{
        int i, j;
        char str[256];
        snprintf(str, 256, "out/density%04d.ppm", step_count);
        FILE* file = fopen (str, "w+");
        fprintf (file, "P2\n");
        fprintf (file, "%d %d\n", N, N);
        fprintf (file, "256\n");

        // find maximum value in image.
        float max = 0;
        for (j = 0; j < N; j++) {
                for (i = 0; i < N; i++) {
                        float d00       = d[IXN(N, i, j)];
                        if (d00 >= max) max = d00;
                }
        }

        // Write out image file.
        for (j = 0; j < N; j++) {
               for (i = 0; i < N; i++) {
                        float d00       = d[IXN(N, i, j)];
                        d00             = d00;
                        fprintf (file, "%d ", (int)d00);
                }

                fprintf(file, "\n");
        }

        fclose(file);
}


