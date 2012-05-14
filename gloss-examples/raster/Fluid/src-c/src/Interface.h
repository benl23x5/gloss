#pragma once

#include "Model.h"

void
open_glut_window (void);


extern int state_window_width;
extern int state_window_height;

extern int state_solver_method;
extern int state_solver_iters;

struct Model* state_model;

void get_from_UI (struct Model* model);
