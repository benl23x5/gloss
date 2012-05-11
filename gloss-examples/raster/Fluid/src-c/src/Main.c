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
#include <GLUT/glut.h>
#include "Model.h"
#include "Interface.h"


int main ( int argc, char ** argv )
{
        int mode_benchmark      = 0;
        int mode_interactive    = 0;

        int   width     = 0;
        float delta     = 0.1;
        float diff      = 0;
        float visc      = 0;

        struct Model* model     = 0;

	if ( argc != 1 && argc != 2) {
                printf("bad usage\n");
                exit(1);
	}

        else if ( argc == 2 ) {
                mode_benchmark      = 1;
                mode_interactive    = 1;
                width               = atoi (argv[1]);
                state_window_width  = width * 2;
                state_window_height = width * 2;
        }

	else {
                mode_benchmark      = 0;
                mode_interactive    = 1;
                width               = 256;
                state_window_width  = width * 2;
                state_window_height = width * 2;

	} 


        // Create the initial model.
        model   = model_new (width, width);
        model->delta    = delta;
        model->diff     = diff;
        model->visc     = visc;
        state_model     = model;


        // // In benchmark mode set some standard initial conditions.
        // if (mode_benchmark)
        // {
        //         int y;
        //         for (y = 10; y <= N - 10; y += 10) {
        //                 dens[IX(40, y)] = 10 * y;
        //                 u   [IX(20,  y)] = ((float)y / (float)N) * 10;
        //         }
        // }

        // In interactive mode, display the simulation in a window.
//        if (mode_interactive) {
                glutInit (&argc, argv);
                open_glut_window ();
                glutMainLoop     ();
//        }

        // In non-interactive mode, just step it a fixed number of times.
/*        else {
                int maxSteps    = 100;
                int i;
                for (i = 0; i < maxSteps; i++) {
                        vel_step    ( N, u, v, u_prev, v_prev, visc, dt );
                        dens_step   ( N, dens, dens_prev, u, v, diff, dt );
                }

                dump_density(step_count, N, dens);
        }
*/
	exit (0);
}
