

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <GLUT/glut.h>

#include "Interface.h"
#include "Model.h"
#include "Solver.h"

#define IX(i,j) ((i)+(N+2)*(j))


// ----------------------------------------------------------------------------
static void     pre_display  (void);
static void     post_display (void);


// -- Global Window State ----------------------------------------------------
int             state_window_width      = 256;
int             state_window_height     = 256;
int             state_window_id;

int             state_mouse_down[3];
int             state_mouse_x, state_mouse_oldx;
int             state_mouse_y, state_mouse_oldy;

int             state_draw_vel          = 0;
struct Model*   state_model;
int             state_step_count        = 0;

float           state_gui_force         = 5;
float           state_gui_source        = 100;


// What linear solver to use.
// 0 - Gauss-Seidel, 1 - Jacobi.
int             state_solver_method     = 0;

// Number of iterations to use in the linear solver.
int             state_solver_iters      = 20;


// ----------------------------------------------------------------------------
// Draw the velocity of a model as lines.
static void
draw_velocity (struct Model* model)
{
        assert(model);

        float* u = model->u;
        float* v = model->v;
        int N    = model->width;

        float hx = 1.0f / model->width;
        float hy = 1.0f / model->height;

        int   i, j;
        float x, y;

        glColor3f   (1.0f, 1.0f, 1.0f);
        glLineWidth (1.0f);

        glBegin (GL_LINES);
        for (i=1 ; i<=model->width ; i++) {
                x = (i-0.5f)*hx;
                for (j=1 ; j<=model->height ; j++) {
                        y = (j-0.5f)*hy;

                        glVertex2f (x, y);
                        glVertex2f (x+u[IX(i,j)], y+v[IX(i,j)]);
                }
        }
        glEnd ();
}


// Draw the density of a model as quads.
static void
draw_density (struct Model* model)
{
        assert (model);

        float* dens = model->dens;
        int N       = model->width;

        float hx = 1.0f / model->width;
        float hy = 1.0f / model->height;

        float x, y, d00;
        int i, j;

        glBegin ( GL_QUADS );
        for ( i=0 ; i<=model->width ; i++ ) {
                x = (i-0.5f)*hx;
                for ( j=0 ; j<=model->height ; j++ ) {
                        y = (j-0.5f)*hy;
                        d00 = dens[IX(i,j)];
                        glColor3f (d00, d00, d00);
                        glVertex2f ( x,    y );
                        glVertex2f ( x+hx, y );
                        glVertex2f ( x+hx, y+hy );
                        glVertex2f ( x,    y+hy );
                }
        }
        glEnd ();
}


static void
display_func (void)
{
        struct Model* model   = state_model;
        assert(model);

        if (  state_step_count < 10
           || (state_step_count % 10) == 0) {
                dump_array(state_step_count, "density", model->width, 1,   model->dens);
                dump_array(state_step_count, "velctyU", model->width, 1000, model->u);
        }

        get_from_UI (model);

        vel_step    ( state_step_count
                    , state_solver_method
                    , state_solver_iters
                    , model->width
                    , model->u,      model->v
                    , model->u_prev, model->v_prev
                    , model->visc,   model->delta);

        dens_step   ( state_step_count
                    , state_solver_method
                    , state_solver_iters
                    , model->width
                    , model->dens,   model->dens_prev
                    , model->u,      model->v
                    , model->diff,   model->delta);

        pre_display ();

        if (state_draw_vel)
                draw_velocity (model);
        else    draw_density  (model);

        post_display ();
        state_step_count++;
}


// Handle user input ----------------------------------------------------------
void get_from_UI (struct Model* model)
{
        assert(model);
        float*  d       = model->dens_prev;
        float*  u       = model->u_prev;
        float*  v       = model->v_prev;

        int i, j;
        int N           = model->width;

        for ( i=0 ; i<model->size ; i++ ) {
                u[i] = v[i] = d[i] = 0.0f;
        }

        if ( !state_mouse_down[0] && !state_mouse_down[2] )
                return;

        i = (int)((state_mouse_x
                /(float)state_window_width)  * N+1);

        j = (int)(((state_window_height - state_mouse_y)
                /(float)state_window_height) * N+1);

        if ( i < 1 || i > state_window_width || j < 1 || j > state_window_height)
                return;

        if ( state_mouse_down[0] ) {
                int mouse_diffx = state_mouse_x    - state_mouse_oldx;
                int mouse_diffy = state_mouse_oldy - state_mouse_y;
                u[IX(i,j)] = state_gui_force * mouse_diffx;
                v[IX(i,j)] = state_gui_force * mouse_diffy;
        }

        if ( state_mouse_down[2] ) {
                d[IX(i,j)] = state_gui_source;
        }

        state_mouse_oldx = state_mouse_x;
        state_mouse_oldy = state_mouse_y;
}


// -- Callbacks ---------------------------------------------------------------
static void
key_func_up (unsigned char key, int x, int y)
{
        switch (key)
        {
                case '1':
                        state_mouse_down[0] = 0;
                        break;

                case '2':
                        state_mouse_down[1] = 0;
                        break;

                case '3':
                        state_mouse_down[2] = 0;
                        break;
        }
}

static void
key_func ( unsigned char key, int x, int y )
{
        switch ( key )
        {
                case '1':
                        state_mouse_down[0] = 1;
                        state_mouse_x = state_mouse_oldx = x;
                        state_mouse_y = state_mouse_oldy = y;
                        break;

                case '2':
                        state_mouse_down[1] = 1;
                        state_mouse_x = state_mouse_oldx = x;
                        state_mouse_y = state_mouse_oldy = y;
                        break;

                case '3':
                        state_mouse_down[2] = 1;
                        state_mouse_x = state_mouse_oldx = x;
                        state_mouse_y = state_mouse_oldy = y;
                        break;

                case 'c':
                case 'C':
                        model_clear (state_model);
                        break;

                case 'q':
                case 'Q':
                        model_free (state_model);
                        state_model     = 0;
                        exit (0);
                        break;

                case 'v':
                case 'V':
                        state_draw_vel = !state_draw_vel;
                        break;
        }
}


static void
mouse_func (int button, int state, int x, int y)
{
        state_mouse_x = state_mouse_oldx = x;
        state_mouse_y = state_mouse_oldy = y;

        state_mouse_down[button] = state == GLUT_DOWN;
}


static void
motion_func (int x, int y)
{
        state_mouse_x = x;
        state_mouse_y = y;
}


static void
reshape_func  (int width, int height)
{
        glutSetWindow     (state_window_id);
        glutReshapeWindow (width, height);

        state_window_width  = width;
        state_window_height = height;
}


static void
idle_func (void)
{
        glutSetWindow     (state_window_id);
        glutPostRedisplay ();
}


// Window ---------------------------------------------------------------------
void
open_glut_window (void)
{
        glutInitDisplayMode ( GLUT_RGBA | GLUT_DOUBLE );

        glutInitWindowPosition (20, 10);
        glutInitWindowSize (state_window_width, state_window_height);
        state_window_id   = glutCreateWindow ( "Fluid" );

        glClearColor       (0.0f, 0.0f, 0.0f, 1.0f);
        glClear            (GL_COLOR_BUFFER_BIT);
        glutSwapBuffers    ();
        glClear            (GL_COLOR_BUFFER_BIT);
        glutSwapBuffers    ();

        pre_display        ();

        glutKeyboardFunc   (key_func);
        glutKeyboardUpFunc (key_func_up);
        glutMouseFunc      (mouse_func);
        glutMotionFunc     (motion_func);
        glutReshapeFunc    (reshape_func);
        glutIdleFunc       (idle_func);
        glutDisplayFunc    (display_func);
}


static void
pre_display (void)
{
        glViewport     (0, 0, state_window_width, state_window_height);
        glMatrixMode   (GL_PROJECTION);
        glLoadIdentity ();
        gluOrtho2D     (0.0, 1.0, 0.0, 1.0);
        glClearColor   (0.0f, 0.0f, 0.0f, 1.0f);
        glClear        (GL_COLOR_BUFFER_BIT);
}


static void
post_display  (void)
{
        glutSwapBuffers ();
}


