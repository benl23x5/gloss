#pragma once

struct Model
{
        int     size;
        int     height;
        int     width;
        float   *u, *u_prev;
        float   *v, *v_prev;

        float   *dens,  *dens_prev;

        float   delta;
        float   diff;
        float   visc;
};

extern void
model_free      (struct Model* model);

extern void
model_clean     (struct Model* model);

extern struct Model*
model_new       (int width, int height);

extern void
dump_density    (int step_count, int N, float * d);

