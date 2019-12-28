# Gloss SDL

This project is under development, don't expect anything to work yet

This is an experimental rebuild of gloss on top of libsdl. We may also change the rendering engine to be based on Constructive Solid Geometry (CSG) and ray tracing instead of rasterization. If this effort succeeds it will become Gloss 2.

The current Gloss library suffers from the following problems, which we try to address in this work:

1. The first version of Gloss used GLUT to manage windows, but the GLUT website itself says it has "been unsupported for 20 years". SDL is actively maintained.

2. Gloss can also use GLFW for window management, but still relies on GLUT for drawing vector text. SDL_ttf provides its own text support.

3. GLUT supports only a single vector font, while SDL_ttf can draw text using standard true type font files.

4. Once control enters the GLUT main loop it does not escape. GLUT redraws can happen on window events or at a fixed timer expiration. Redraws cannot be triggered by external events such as file or network activity. The current Gloss displayIO interface tries to hack around this limitation, but the interface still only updates static pictures once per second. Using the SDL API one can define their own main loop and trigger redraws based on arbitrary events.

5. GLUT does not support high dpi displays (like Retina displays on Mac). GLFW seems to have some support, but the Gloss authors haven't tested it yet. SDL has working and maintained high DPI display support.

6. Gloss has a variety of problems when launched from the GHCi console, ranging from crashes, low framerates, or force quit of GHCi when the window is closed. The behaviour is variable depending on the platform. The SDL event framework allows the window to be closed without quiting the host executable.

7. The Gloss picture type includes a polygon constructor, which allows polygons to be defined by a path, but there is an unchecked precondition that the polygon must be convex. Trying to defined non-convex paths results in undefined behaviour. There was a project to add triangulation support to gloss but the resulting code was deemed too complex to be merged. In the move to CSG we can present primitive triangles and rectangles as polygons and avoid general polygons.

8. The Gloss picture type includes circle and arc constructors. The rasterization code for circles and arcs has been a bug farm over the years, and still has known problems. Moving to CSG/ray tracing style rendering will allow us to draw circles and arcs at full resolution in a way that hopefully avoids rasterization bugs.

9. The Gloss interface API provides both "pure" and "IO" versions of top-level functions like "display" and "simulate". If we can separate the picture and rendering API cleanly from the window management functionality then we could provide just the "pure" interfaces for simple use-cases. We would require users to use a lower level IO based API for IO driven applications.

10. Gloss does not have sound support, and there appears to be no simple graphics/sound interface in the Haskell ecosystem. Attempts to combine separate audio libraries with gloss usually end up with latency problems, resulting in auto clicking.


