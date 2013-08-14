See <http://mazzo.li/posts/graph-drawing.html> for a rendering of the
literate Haskell below.

A while ago, me and some friends wrote a [C++
tool](https://github.com/scvalex/visigoth) to generate and visualise
graphs, and I was surprised at how easy it is to "balance" graph
vertices so that they are laid out in a nice way. This tutorial
reproduces a version of the algorithm in Haskell, using the
[`gloss`](http://hackage.haskell.org/package/gloss) library to get the
graph on the screen.  Apart from `gloss` nothing outside the Haskell
Platform is needed.[^version]

[^version]: Note that you need the very last version of `gloss`,
1.8.0.1, for this code to work.  The author spent some time hacking on
gloss to make this code simpler, and the changes have been merged
recently.

This tutorial is aimed at beginners, and only a basic knowledge of
Haskell is required---we disregard performance in favour of simple code.
Here is a preview of the result:

<div class="yt"><iframe width="600" height="450" src="http://www.youtube.com/embed/lehmgscv7rk" frameborder="0" allowfullscreen></iframe></div>

Preliminaries
----

We import the libraries we need, qualifying `Map` and `Set` avoiding
clashes with the `Prelude`.

> import Data.Map.Strict (Map)
> import qualified Data.Map.Strict as Map
> import Data.Set (Set)
> import qualified Data.Set as Set
> import System.Random
>
> import Graphics.Gloss
> import Graphics.Gloss.Data.Vector
> import Graphics.Gloss.Data.ViewState
> import Graphics.Gloss.Interface.Pure.Game

The idea
----

First, let's frame the problem we want to solve.  We have an undirected
graph, and we want to position its vertices on a surface so that the
result is pleasant to look at.  "Pleasant to look at" is still a very
vague requirement depending on fuzzy things like human taste, and in
fact there are [many ways to go at this
problem](http://www.graphviz.org/).

We will gain inspiration from physics, and take vertices to be like
charged particles repelling each other, and edges to be like elastic
bands pulling the vertices together.[^force] We will calculate the
forces and update the positions in rounds, and hopefully after some time
our graph will stabilise.  With the right numbers, this gives
surprisingly good results: clusters of vertices are held together by the
numerous edges between them, while sparsely connected vertices remain
distant, reducing clutter.

[^force]: This class of algorithms is known as [*force-directed graph
drawing*](http://en.wikipedia.org/wiki/Force-directed_graph_drawing).

The `Graph`
----

We need some kind of identifier for our vertices, we will simply go for
`Int`.  An `Edge` is a pair of `Vertex`s.

> type Vertex = Int
> type Edge = (Vertex, Vertex)

We want to store our graph so that the operations we need to execute are
as natural as possible.  Given the algorithm outline given above, we
need to do two things well: iterating through all the vertices, and
iterating through the neighbours of a given vertex.  With that in mind,
the simplest thing to do is simply store the graph as the set of
neighbouring nodes for each `Vertex`:

> -- INVARIANT Every `Vertex` present in a set of neighbours is present as
> -- a key in the `Map`.
> newtype Graph = Graph {grNeighs :: Map Vertex (Set Vertex)}
>
> emptyGraph :: Graph
> emptyGraph = Graph Map.empty


When we add a vertex, we make sure that a set of neighbours exist for
that vertex.  In this way adding existing vertices will not modify the
graph.

> addVertex :: Vertex -> Graph -> Graph
> addVertex v (Graph neighs) =
>     Graph $ case Map.lookup v neighs of
>                 Nothing -> Map.insert v Set.empty neighs
>                 Just _  -> neighs

When we add an `Edge`, we first make sure that the vertices provided are
present in the graph by adding them, and then add each vertex to the
other vertex's neighbours.

> addEdge :: Edge -> Graph -> Graph
> addEdge (v1, v2) gr = Graph neighs
>   where
>     gr'    = addVertex v1 (addVertex v2 gr)
>     neighs = Map.insert v1 (Set.insert v2 (vertexNeighs v1 gr')) $
>              Map.insert v2 (Set.insert v1 (vertexNeighs v2 gr')) $
>              grNeighs gr'

`vertexNeighs` unsafely gets the neighbours of a given `Vertex`: the
precondition is that the `Vertex` provided is in the graph.

> vertexNeighs :: Vertex -> Graph -> Set Vertex
> vertexNeighs v (Graph neighs) = neighs Map.! v

This is all we need to implement the algorithm.  It is also useful to
have a function returning all the edges in the `Graph` so that we can
draw them.  `Set.foldr` and `Map.foldrWithKey` are equivalent to the
usual `foldr` for lists, with the twist that with a `Map` we fold over
the key and value at the same time.  Since the graph is undirected, we
"order" each edge so that the the vertex with the lower id appears
first: in this way we will avoid duplicates like `(1, 2)` and `(2, 1)`.

> graphEdges :: Graph -> Set Edge
> graphEdges = Map.foldrWithKey' foldNeighs Set.empty . grNeighs
>   where
>     -- For each vertex `v1`, insert an edge for each neighbour `v2`.
>     foldNeighs v1 ns es =
>         Set.foldr' (\v2 -> Set.insert (order (v1, v2))) es ns
>     order (v1, v2) = if v1 > v2 then (v1, v2) else (v2, v1)


The `Scene`
----

Now that we have our graph, we need a data structure recording the
position of each point.  We also want to be able to "grab" points to
move them around, so we add a field recording whether we have a `Vertex`
grabbed or not.  We also make use of `gloss` `ViewState`, which will let
us implement panning, rotating, and zooming in an easy way.

> -- INVARIANT The keys in `scGraph` are the same as the keys in `scPoints`.
> data Scene =
>     Scene { scGraph     :: Graph
>           , scPoints    :: Map Vertex Point
>           , scSelected  :: Maybe Vertex
>           , scViewState :: ViewState }
>
> emptyScene :: Scene
> emptyScene =
>     Scene{ scGraph     = emptyGraph
>          , scPoints    = Map.empty
>          , scSelected  = Nothing
>          , scViewState = viewStateInit }

Then two predictable operations: one that adds a `Vertex`, with its
initial position on the scene, and one that adds an `Edge`.  When adding
the `Edge`, we *need* both points to be already present---see the
invariant for `Scene`.  We cannot simply add the vertices like we do in
`addEdge` because we need their positions.

> scAddVertex :: Vertex -> Point -> Scene -> Scene
> scAddVertex v pt sc@Scene{scGraph = gr, scPoints = pts} =
>     sc{scGraph = addVertex v gr, scPoints = Map.insert v pt pts}
>
> scAddEdge :: Edge -> Scene -> Scene
> scAddEdge e@(v1, v2) sc@Scene{scGraph = gr, scPoints = pts} =
>     if Map.member v1 pts && Map.member v2 pts
>     then sc{scGraph = addEdge e gr}
>     else error "scAddEdge: non existant point!"

It is also useful to have an helper to get the position of a `Vertex`.

> vertexPos :: Vertex -> Scene -> Point
> vertexPos v Scene{scPoints = pts} = pts Map.! v

Drawing
----

Now we can write the functions to convert the `Scene` to a `Picture`.
Thanks to `gloss`, this is *extremely* easy: we are offered a simple
data type that `gloss` will use to get things on the screen.

Some constants:

> vertexRadius :: Float
> vertexRadius = 6
>
> vertexColor :: Color
> vertexColor = makeColor 1 0 0 1 -- Red
>
> edgeColor :: Color
> edgeColor = makeColor 1 1 1 0.8 -- Whiteish

Drawing a `Vertex` is simply drawing a circle.  We use `ThickCircle` to
get the circle to be filled instead of just an outline.

> drawVertex :: Vertex -> Scene -> Picture
> drawVertex v sc = Translate x y (ThickCircle (vertexRadius / 2) vertexRadius)
>   where (x, y) = vertexPos v sc

Drawing an `Edge` is drawing a `Line`.

> drawEdge :: Edge -> Scene -> Picture
> drawEdge (v1, v2) sc = Line [vertexPos v1 sc, vertexPos v2 sc]

Bringing everything together, we generate `Picture`s for all the
vertices and all the edges, and then combine those with the appropriate
colours.  Moreover we get the `ViewPort` in the `ViewState`---which
stores the current translation, rotation, and scaling--- and apply it to
the picture.

> drawScene :: Scene -> Picture
> drawScene sc@Scene{scGraph = gr, scViewState = ViewState{viewStateViewPort = port}} =
>     applyViewPortToPicture port $
>     Pictures [Color vertexColor vertices, Color edgeColor edges]
>   where
>     vertices = Pictures [drawVertex n sc | n <- Map.keys (grNeighs gr)    ]
>     edges    = Pictures [drawEdge e sc   | e <- Set.toList (graphEdges gr)]

Balancing
----

Now to the interesting part, the code necessary to balance the graph.
As mentioned, we have two contrasting forces.  Each vertex "pushes" all
the others away, and each edge "pulls" together the connected vertices.

First we define a function for the "pushing" force, resulting from the
charge of the vertices.
[Predictably](http://en.wikipedia.org/wiki/Coulomb%27s_law), the force
will be inversely proportional to the square of the distance of the two
vertices.  `Graphics.Gloss.Data.Vector` defines

    type Vector = (Float, Float)

and also a `Num` instance for `Vector`, which means that we can take
advantage of vector subtraction to easily get the distance and the
direction of the force.

The charge of each particle has been determined empirically to give good
results---increasing it will lead to a more "spaced out" graph,
decreasing it a more crowded one.  `mulSV` lets us multiply `Vector`s by
scalars, and `magV` lets us get the magnitude of a vector (in this case
the distance).  Varying the charge will determine how far apart the
vertices will be.

> charge :: Float
> charge = 100000
>
> pushForce :: Point         -- Vertex we're calculating the force for
>           -> Point         -- Vertex pushing the other away
>           -> Vector
> pushForce v1 v2 =
>     -- If we are analysing the same vertex, l = 0
>     if l > 0 then (charge / l) `mulSV` normaliseV d else 0
>   where
>     d = v1 - v2
>     l = magV d ** 2

For what concerns the force that pulls connected vertices together, it
will be [proportional to the distance of the two
edges](http://en.wikipedia.org/wiki/Hooke%27s_law), so we can take the
distance vector directly and multiply it by the stiffness, although this
time ve have the vector point in the other direction, since this force
brings the vertices together.

> stiffness :: Float
> stiffness = 1 / 2
>
> pullForce :: Point -> Point -> Vector
> pullForce v1 v2 = stiffness `mulSV` (v2 - v1)

We can then write a function to get the velocity of a `Vertex` in each
round:

> updatePosition :: Float       -- Time since the last update
>                -> Vertex      -- Vertex we are analysing
>                -> Scene
>                -> Point       -- New position
> updatePosition dt v1 sc@Scene{scPoints = pts, scGraph = gr} =
>     v1pos + pull + push
>   where
>     v1pos  = vertexPos v1 sc
>
>     -- Gets a velocity by multiplying the time by the force (we assume
>     -- a mass of 1).
>     getVel f v2pos = dt `mulSV` f v1pos v2pos
>
>     -- Sum all the pushing and pulling.  All the other vertices push,
>     -- the connected vertices pull.
>     push = Map.foldr' (\v2pos -> (getVel pushForce v2pos +)) 0 pts
>     pull = foldr (\v2pos -> (getVel pullForce v2pos +)) 0
>                  [vertexPos v2 sc | v2 <- Set.toList (vertexNeighs v1 gr)]

We bring everything together by calculating the new position for each
vertex.  We do not move the vertex that is currently selected by the
user, if there is one.

> updatePositions :: Float -> Scene -> Scene
> updatePositions dt sc@Scene{scSelected = sel, scGraph = Graph neighs} =
>     foldr f sc (Map.keys neighs)
>   where
>     f n sc' =
>         let pt = if Just n == sel then vertexPos n sc else updatePosition dt n sc'
>         in scAddVertex n pt sc'

User interaction
----

When a user clicks to grab a point, we need to check if she has caught
something.  Thus we define `inCircle` to check if the a point is inside
the drawn version of a vertex.

> inCircle :: Point             -- Where the user has clicked
>          -> Float             -- The scaling factor in the ViewPort
>          -> Point             -- The position of the vertex
>          -> Bool
> inCircle p sca v = magV (v - p) <= vertexRadius * sca

`findVertex` iterates through all the vertices and returns one if the
position where the user has clicked is in it.

> findVertex :: Point -> Float -> Scene -> Maybe Vertex
> findVertex p1 sca Scene{scPoints = pts} = Map.foldrWithKey' f Nothing pts
>   where
>     f _ _  (Just v) = Just v
>     f v p2 Nothing  = if inCircle p1 sca p2 then Just v else Nothing

User input will come in the form of `Event`s, a `gloss` data type that
represents key or mouse button presses, and mouse motion.  Thus we
define `handleEvent` to process an `Event` and a `Scene` producing a new
`Scene`:

> handleEvent :: Event -> Scene -> Scene

We want the user to be able to grab vertices.  Since the default
configuration for the `ViewState`---which we are using---already uses
the left and right mouse button for its actions, we require the user to
press `Ctrl` and click:

> handleEvent (EventKey (MouseButton LeftButton) Down Modifiers{ctrl = Down} pos) sc =
>     case findVertex (invertViewPort port pos) (viewPortScale port) sc of
>         Nothing -> sc
>         Just v  -> sc{scSelected = Just v}
>   where
>     viewState = scViewState sc
>     port      = viewStateViewPort viewState

`invertViewPort` "undoes" the rotation, translation and scaling applied
by the `ViewPort` to the picture, so that we can map user input to the
coordinates that `scPoints` refers to.

When the user releases the left mouse button and a vertex is selected,
we deselect it:

> handleEvent (EventKey (MouseButton LeftButton) Up _ _) sc@Scene{scSelected = Just _} =
>     sc{scSelected = Nothing}

When the user moves the mouse and a vertex is selected, we move the
vertex where the cursor is:

> handleEvent (EventMotion pos) sc@Scene{scPoints = pts, scSelected = Just v} =
>     sc{scPoints = Map.insert v (invertViewPort port pos) pts}
>  where
>    port = viewStateViewPort (scViewState sc)

When none of the above apply, we pass the event to the `ViewState`,
which will handle the panning, rotating, and zooming.

> handleEvent ev sc =
>     sc{scViewState = updateViewStateWithEvent ev (scViewState sc)}

Running
----

Finally, we put the code above to good use.  We will use a sample graph
to draw:

> -- Taken from <http://www.graphviz.org/Gallery/undirected/transparency.gv.txt>.
> sampleGraph :: [Edge]
> sampleGraph =
>     [(1,  30), (1,  40), (8,  46), (8,  16), (10, 25), (10, 19), (10, 33),
>      (12, 8 ), (12, 36), (12, 17), (13, 38), (13, 24), (24, 49), (24, 13),
>      (24, 47), (24, 12), (25, 27), (25, 12), (27, 12), (27, 14), (29, 10),
>      (29, 8 ), (30, 24), (30, 44), (38, 29), (38, 35), (2,  42), (2,  35),
>      (2,  11), (14, 18), (14, 24), (14, 38), (18, 49), (18, 47), (26, 41),
>      (26, 42), (31, 39), (31, 47), (31, 25), (37, 26), (37, 16), (39, 50),
>      (39, 14), (39, 18), (39, 47), (41, 31), (41, 8 ), (42, 44), (42, 29),
>      (44, 37), (44, 32), (3,  20), (3,  28), (6,  45), (6,  28), (9,  6 ),
>      (9,  16), (15, 16), (15, 48), (16, 50), (16, 32), (16, 39), (20, 33),
>      (33, 9 ), (33, 46), (33, 48), (45, 15), (4,  17), (4,  15), (4,  12),
>      (17, 21), (19, 35), (19, 15), (19, 43), (21, 19), (21, 50), (23, 36),
>      (34, 23), (34, 24), (35, 34), (35, 16), (35, 18), (36, 46), (5,  7 ),
>      (5,  36), (7,  32), (7,  11), (7,  14), (11, 40), (11, 50), (22, 46),
>      (28, 43), (28, 8 ), (32, 28), (32, 39), (32, 42), (40, 22), (40, 47),
>      (43, 11), (43, 17)
>     ]

Then an utility function `fromEdges` initialises a scene from a list of
edges randomising the positions of the vertices in the initial window
size:

> windowSize :: (Int, Int)
> windowSize = (640, 480)
>
> fromEdges :: StdGen -> [Edge] -> Scene
> fromEdges gen es =
>     foldr scAddEdge (fst (Set.foldr' addv (emptyScene, gen) vs)) es
>   where
>     vs = Set.fromList (concat [[v1, v2] | (v1, v2) <- es])
>
>     halfWidth  = fromIntegral (fst windowSize) / 2
>     halfHeight = fromIntegral (snd windowSize) / 2
>
>     addv v (sc, gen1) =
>         let (x, gen2) = randomR (-halfWidth,  halfWidth ) gen1
>             (y, gen3) = randomR (-halfHeight, halfHeight) gen2
>         in  (scAddVertex v (x, y) sc, gen3)

Finally, we use the `play` function provided by `gloss` to make
everything work.  The important arguments in `play` are the last two
functions, which update the state of the world after a user event and
after a time step, respectively.  In our case `handleEvent` and
`updatePositions` will do the job, our world being a `Scene`.

> sceneWindow :: Scene -> IO ()
> sceneWindow sc =
>     play (InWindow "Graph Drawing - ctrl + left mouse button to drag" windowSize (10, 10))
>          black 30 sc drawScene handleEvent updatePositions

Then all its left to do is to initialise the `Scene` and run
`sceneWindow`.

> main :: IO ()
> main =
>     do gen <- getStdGen
>        sceneWindow (fromEdges gen sampleGraph)

Improvements
----

The code provided is a good starting point for many improvements, here
we give some suggestions.

*   **Performance**

    The code does not scale well for big graphs, for a number of reason.

    *   *QuadTree/Voronoi diagram*: Currently our algorithm is cubic: for
        each vertex we go over all the other vertices for the push forces
        and over all the neighbours for the pull forces.

        It can be made much faster by approximating distant clusters of
        vertices to a single particle with higher charge.  An easy way is
        to subdivide recursively the space into squares, a goal achievable
        by storing the graph in a [*QuadTree*](http://en.wikipedia.org/wiki/Quadtree).[^quadtree]
        Then squares that are far enough are deemed as one entity.[^bh]

        A more precise but also more expensive way is to subdivide the
        space in a more irregular way depending on the disposition of the
        vertices, for example in what is called a
        [*Voronoi diagram*](http://en.wikipedia.org/wiki/Voronoi_diagram).

    *   *Arrays*: Currently, once a graph is loaded, it stays the same
        forever.  This considered, using `Map` is quite a waste: we can
        utilise a structure with much better performance to store the graph,
        such as an `Array` or a `Vector`. The best option would probably
        be an unboxed `Vector`.

*   **Functionality**

    *   *Weighed edges*: We can easily adapt the algorithm to work with
        weighed edges by adjusting the stiffness of each edge depending
        on the weigh.  For example if the weigh represents the distance
        between two connected nodes, the stiffness will be inversely
        proportional to the weigh, so that closer vertices will indeed
        end up being closer.

    *   *Generating graphs*: Generating realistic graphs is an interesting
        and useful challenge.  It turns out that many real networks, such
        as friendships and the web, share certain characteristics.  Such
        networks are known as [small-world networks](http://en.wikipedia.org/wiki/Small-world_network), 
        and various algorithms to generate them are available.

    *   *3D*: The algorithm can be trivially extended to the 3rd
        dimension---in fact given the right `Num` instances it will work in
        automatically, and with some type class trickery in any dimension.

        The hard part would be drawing the graph, since `gloss` does not
        go beyond 2 dimensions, and raw OpenGL is so much uglier.

    *   *`dot` files*: The program could be enhanced with a parser for
        `dot` or similar format, so that experiments could be ran on
        existing graphs.

[^quadtree]: `gloss` provides a module to work with QuadTrees,
`Graphics.Gloss.Data.QuadTree`.

[^bh]: This approach is known as [*Barnes-Hut
simulation*](http://en.wikipedia.org/wiki/Barnes%E2%80%93Hut_simulation).

If you implement any of the above in a nice way, let me know!

As usual, comments on [Reddit](http://www.reddit.com/r/haskell/comments/1kb36j/graph_drawing_with_gloss/).

