/*
  ======================================================================
   demo.c --- protoype to show off the simple solver
  ----------------------------------------------------------------------
   Author : Jos Stam (jstam@aw.sgi.com)
   Creation Date : Jan 9 2003

   Description:

	This code is a simple prototype that demonstrates how to use the
	code provided in my GDC2003 paper entitles "Real-Time Fluid Dynamics
	for Games". This code uses OpenGL and GLUT for graphics and interface

  =======================================================================
*/

#include <stdlib.h>
#include <stdio.h>
#include <GLUT/glut.h>

/* macros */

#define IX(i,j) ((i)+(N+2)*(j))

/* external definitions (from solver.c) */

extern void dens_step    ( int N, float * x, float * x0, float * u, float * v, float diff, float dt );
extern void vel_step     ( int N, float * u, float * v, float * u0, float * v0, float visc, float dt );
extern void dump_density ( int step_count, int N, float * d );


/* global variables */

static int N;
static float dt, diff, visc;
static float force, source;
static int dvel;

static float *u,    *u_prev,    u_back;
static float *v,    *v_prev,    u_back;
static float *dens, *dens_prev, dens_back;

static int win_id;
static int win_x, win_y;
static int mouse_down[3];
static int omx, omy, mx, my;
static int step_count   = 0;


// ----------------------------------------------------------------------
//  relates mouse movements to forces sources
// ----------------------------------------------------------------------
static void get_from_UI ( float * d, float * u, float * v )
{
	int i, j, size = (N+2)*(N+2);

	for ( i=0 ; i<size ; i++ ) {
		u[i] = v[i] = d[i] = 0.0f;
	}

	if ( !mouse_down[0] && !mouse_down[2] ) return;

	i = (int)((       mx /(float)win_x)*N+1);
	j = (int)(((win_y-my)/(float)win_y)*N+1);

	if ( i<1 || i>N || j<1 || j>N ) return;

	if ( mouse_down[0] ) {
		u[IX(i,j)] = force * (mx-omx);
		v[IX(i,j)] = force * (omy-my);
	}

	if ( mouse_down[2] ) {
		d[IX(i,j)] = source;
	}

	omx = mx;
	omy = my;

	return;
}




// ----------------------------------------------------------------------
//  main --- main routine
// ----------------------------------------------------------------------
int main ( int argc, char ** argv )
{
        int mode_benchmark      = 0;
        int mode_interactive    = 0;

	if ( argc != 1 && argc != 2 && argc != 7 ) {
		fprintf (stderr, "argc = %d\n", argc);
		fprintf ( stderr, "usage : %s N dt diff visc force source\n", argv[0] );
		fprintf ( stderr, "where:\n" );\
		fprintf ( stderr, "\t N      : grid resolution\n" );
		fprintf ( stderr, "\t dt     : time step\n" );
		fprintf ( stderr, "\t diff   : diffusion rate of the density\n" );
		fprintf ( stderr, "\t visc   : viscosity of the fluid\n" );
		fprintf ( stderr, "\t force  : scales the mouse movement that generate a force\n" );
		fprintf ( stderr, "\t source : amount of density that will be deposited\n" );
		exit ( 1 );
	}

        else if ( argc == 2 ) {
                mode_benchmark   = 1;
                mode_interactive = 1;
                N      = atoi (argv[1]);
                dt     = 0.1f;
                diff   = 0.0f;
                visc   = 0.0f;
                force  = 5.0f;
                source = 100.0f;

                win_x   = 512;
                win_y   = 512;
        }

	else if ( argc == 1 ) {
                mode_benchmark   = 0;
                mode_interactive = 1;

		N      = 256;
		dt     = 0.1f;
		diff   = 0.0f;
		visc   = 0.0f;
		force  = 5.0f;
		source = 100.0f;
		fprintf ( stderr, "Using defaults : N=%d dt=%g diff=%g visc=%g force = %g source=%g\n",
			N, dt, diff, visc, force, source );

                win_x   = N;
                win_y   = N;

	} else {
                mode_benchmark   = 0;
                mode_interactive = 1;

		N      = atoi(argv[1]);
		dt     = atof(argv[2]);
		diff   = atof(argv[3]);
		visc   = atof(argv[4]);
		force  = atof(argv[5]);
		source = atof(argv[6]);

                win_x  = N;
                win_y  = N;
	}

/*
	printf ( "\n\nHow to use this demo:\n\n" );
	printf ( "\t Add densities with the right mouse button\n" );
	printf ( "\t Add velocities with the left mouse button and dragging the mouse\n" );
	printf ( "\t Toggle density/velocity display with the 'v' key\n" );
	printf ( "\t Clear the simulation by pressing the 'c' key\n" );
	printf ( "\t Quit by pressing the 'q' key\n" );
*/
	dvel = 0;

	if ( !allocate_data () ) exit ( 1 );
	clear_data ();


        // In benchmark mode set some standard initial conditions.
        if (mode_benchmark)
        {
                int y;
                for (y = 10; y <= N - 10; y += 10) {
                        dens[IX(40, y)] = 10 * y;
                        u   [IX(20,  y)] = ((float)y / (float)N) * 10;
                }
        }

        // In interactive mode, display the simulation in a window.
        if (mode_interactive) {
                glutInit ( &argc, argv );
                open_glut_window ();
                glutMainLoop ();
        }

        // In non-interactive mode, just step it a fixed number of times.
        else {
                int maxSteps    = 100;
                int i;
                for (i = 0; i < maxSteps; i++) {
                        vel_step    ( N, u, v, u_prev, v_prev, visc, dt );
                        dens_step   ( N, dens, dens_prev, u, v, diff, dt );
                }

                dump_density(step_count, N, dens);
        }

	exit ( 0 );
}
