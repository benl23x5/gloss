

#include <stdlib.h>
#include <stdio.h>
#include <GLUT/glut.h>

#include "Interface.h"
#include "Model.h"


static void
pre_display  (void);

static void
post_display (void);


// -- Global Window State ----------------------------------------------------
int state_window_width;
int state_window_height;
int state_window_id;

int state_mouse_down[3];
int state_mouse_x, state_mouse_oldx;
int state_mouse_y, state_mouse_oldy;

int           state_draw_vel;
struct Model* state_model;


// ----------------------------------------------------------------------------
// Draw the velocity of a model as lines.
static void
draw_velocity (struct Model* model)
{
        float* u = model->u;
        float* v = model->v;
        int n    = model->width;

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
                        glVertex2f (x+u[IXN(n,i,j)], y+v[IXN(n,i,j)]);
                }
        }
        glEnd ();
}


// Draw the density of a model as quads.
static void
draw_density (struct Model* model)
{
        float* dens = model->dens;
        float n     = model->width;

        float hx = 1.0f / model->width;
        float hy = 1.0f / model->height;

        float x, y, d00;
        int i, j;

        glBegin ( GL_QUADS );
        for ( i=0 ; i<=model->width ; i++ ) {
                x = (i-0.5f)*hx;
                for ( j=0 ; j<=model->height ; j++ ) {
                        y = (j-0.5f)*hy;
                        d00 = dens[IXN(n,i,j)];
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

        get_from_UI (model->dens_prev, model->u_prev, model->v_prev);

        vel_step    ( model->width
                    , model->u,      model->v
                    , model->u_prev, model->v_prev
                    , model->visc,   model->delta);

        dens_step   ( model->width
                    , model->dens,   model->dens_prev
                    , model->u,      model->v
                    , model->diff,   model->delta);

        pre_display ();

        if (state_draw_vel)
                draw_velocity (model);
        else    draw_density  (model);

        post_display ();
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
                        clear_data ();
                        break;

                case 'q':
                case 'Q':
                        free_data ();
                        exit ( 0 );
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
static void
open_glut_window (void)
{
        glutInitDisplayMode ( GLUT_RGBA | GLUT_DOUBLE );

        glutInitWindowPosition (0, 0);
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


