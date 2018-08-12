gloss
=====

[![Build Status](https://travis-ci.org/tmcdonell/gloss.svg?branch=master)](https://travis-ci.org/tmcdonell/gloss)

Gloss hides the pain of drawing simple vector graphics behind a nice
data type and a few display functions.

* Home page and bug tracker are on a [separate site](http://gloss.ouroborus.net).

Example
-------
Getting something on the screen is as easy as:

    import Graphics.Gloss
    main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)

Explore and run the example projects using [stack](http://haskellstack.org):

    $ stack setup
    $ stack build
    $ stack exec gloss-boids


Usage
-----
Once the window is open you can use the following:

 * Quit
   - esc-key

 * Move Viewport
   - arrow keys
   - left-click drag

 * Zoom Viewport
   - page up/down-keys
   - control-left-click drag
   - right-click drag
   - mouse wheel

 * Rotate Viewport
   - home/end-keys
   - alt-left-click drag

 * Reset Viewport
   - 'r'-key


More
----
* Animations and simulations can be constructed similarly using the 'animate' and 'simulate' functions
* Games with user input can be constructed with the 'play' function.
* See the gloss-examples package for more.


PULL REQUESTS
-------------

As Gloss is now fairly stable, I typically make releases only when there is a new GHC version. If you have added a new feature or fixed a bug, and want your pull request merged sooner than that, then send email to benl AT ouroborus.net. I don't pay attention to github notifications, but am happy to receive emails from people. If you have changed any internal functionality then please test that all the `gloss-examples` still work before submitting your pull request.

If you just want to bump a dependency version then get a Hackage trustee to edit the package information directly on Hackage. You don't need to raise a pull request here.
