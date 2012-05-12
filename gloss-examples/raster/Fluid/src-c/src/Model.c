
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
        snprintf(str, 256, "out/%04d-%s.ppm", step_count, name);
        FILE* file = fopen (str, "w+");
        fprintf (file, "P2\n");
        fprintf (file, "%d %d\n", N+2, N+2);
        fprintf (file, "256\n");

        // find maximum value in image.
        float max = 0;
        for (j = 0; j < N+2; j++) {
                for (i = 0; i < N+2; i++) {
                        float d00       = d[IXN(N, i, j)];
                        if (d00 >= max) max = d00;
                }
        }

        // Write out image file.
        for (j = N+1; j >= 0; j--) {
               for (i = 0; i < N+2; i++) {
                        float d00       = (d[IXN(N, i, j)] / max) * 255;
                        fprintf (file, "% 4d ", (int)d00);
                }

                fprintf(file, "\n");
        }

        fclose(file);
}


