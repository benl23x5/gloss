//  Based on code by Jos Stam (jstam@aw.sgi.com)
//
//  This is a simple prototype that demonstrates how to use the
//  code provided in the GDC2003 paper entitled "Real-Time Fluid Dynamics
//  for Games". This code uses OpenGL and GLUT for graphics and interface
//
//      How to use this demo:
//      Add densities with the right mouse button
//      Add velocities with the left mouse button and dragging the mouse
//      Toggle density/velocity display with the 'v' key
//      Clear the simulation by pressing the 'c' key
//      Quit by pressing the 'q' key
//
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <GLUT/glut.h>
#include <string.h>

#include "Model.h"
#include "Interface.h"
#include "Solver.h"

#define IX(i,j) ((i)+(N+2)*(j))


int main ( int argc, char ** argv )
{
        // What initial conditions to use.
        //  0 - zero.  1 - symmetric /w cosine.
        int mode_initial        = 0;

        // Quit after this many steps.
        int mode_max_steps      = 0;

        // In batch mode, don't display the window.
        int mode_batch          = 0;

        int   width             = 128;
        int   scale             = 5;
        float delta             = 0.1;
        float diff              = 0.00001;
        float visc              = 0;
        struct Model* model     = 0;

        int i;

        // Read command-line arguments.
        for (i = 1; i < argc; i++) {
                if      (strcmp (argv[i], "--batch") == 0)
                        mode_batch      = 1;

                else if (strcmp (argv[i], "--initial") == 0)
                        mode_initial  = 1;

                else if (strcmp (argv[i], "--max") == 0)
                        mode_max_steps  = atoi (argv[++i]);

                else if (strcmp (argv[i], "--width") == 0)
                        width           = atoi (argv[++i]);

                else if (strcmp (argv[i], "--scale") == 0)
                        scale           = atoi (argv[++i]);

                else if (strcmp (argv[i], "--iters") == 0)
                        state_solver_iters = atoi (argv[++i]);

                else if (strcmp (argv[i], "--jacobi") == 0)
                        state_solver_method = 1;

                else if (strcmp (argv[i], "--gauss-seidel") == 0)
                        state_solver_method = 0;

                else {
                        printf ("bad usage\n");
                        exit(0);
                }
        }

        // Set the window size based on the model size
        state_window_width      = width * scale;
        state_window_height     = width * scale;

        // Create the initial model.
        model           = model_new (width, width);
        model->delta    = delta;
        model->diff     = diff;
        model->visc     = visc;
        state_model     = model;


        // // In benchmark mode set some standard initial conditions.
        if (mode_initial)
        {
                float yc        = width / 2;
                float xc        = width / 2;

                int y, x;
                int N  = model->width;
                for (y = 1; y <= N; y++)
                for (x = 1; x <= N; x++) {
                        int x2          = x - 1;
                        int y2          = y - 1;

                        float xk1       = cos (10 * (x2 - xc) / width);
                        float yk1       = cos (10 * (y2 - yc) / width);
                        float d1        = xk1 * yk1;
                        if (d1 < 0) d1 = 0;

                        float xk2       = cos (15 * (x2 - xc) / width);
                        float yk2       = cos (15 * (y2 - yc) / width);
                        float d2        = xk2 * yk2 / 5;

                        model->dens     [IX(x, y)] = d1;
                        model->dens_prev[IX(x, y)] = d1;
                        model->v        [IX(x, y)] = d2;
                        model->v_prev   [IX(x, y)] = d2;
                }
        }

        // In batch mode, just run the simulation a fixed number of times.
        if (mode_batch) {
                int i;

                for (i = 0; i < mode_max_steps; i++) {
                        get_from_UI (model);

                        vel_step
                                ( i
                                , state_solver_method
                                , state_solver_iters
                                , model->width
                                , model->u,      model->v
                                , model->u_prev, model->v_prev
                                , model->visc,   model->delta);

                        dens_step
                                ( i
                                , state_solver_method
                                , state_solver_iters
                                , model->width
                                , model->dens,   model->dens_prev
                                , model->u,      model->v
                                , model->diff,   model->delta);
                }

                dump_array(i, "density", model->width, 1, model->dens);
        }

        // In interactive mode, display the simulation in a window.
        else {
                glutInit (&argc, argv);
                open_glut_window ();
                glutMainLoop     ();
        }


        exit (0);
}
