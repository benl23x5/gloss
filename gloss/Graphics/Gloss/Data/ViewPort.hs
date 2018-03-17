
module Graphics.Gloss.Data.ViewPort
        ( ViewPort(..)
        , viewPortInit
        , applyViewPortToPicture
        , invertViewPort )
where
import Graphics.Gloss.Data.Picture
import qualified Graphics.Gloss.Data.Point.Arithmetic as Pt


-- | The 'ViewPort' represents the global transformation applied to the displayed picture.
--      When the user pans, zooms, or rotates the display then this changes the 'ViewPort'.
data ViewPort
        = ViewPort {
        -- | Global translation.
          viewPortTranslate     :: !(Float, Float)

        -- | Global rotation (in degrees).
        , viewPortRotate        :: !Float

        -- | Global scaling (of both x and y coordinates).
        , viewPortScale         :: !Float
        }


-- | The initial state of the viewport.
viewPortInit :: ViewPort
viewPortInit
        = ViewPort
        { viewPortTranslate     = (0, 0)
        , viewPortRotate        = 0
        , viewPortScale         = 1
        }


-- | Translates, rotates, and scales an image according to the 'ViewPort'.
applyViewPortToPicture :: ViewPort  -> Picture -> Picture
applyViewPortToPicture
        ViewPort { viewPortScale        = vscale
                 , viewPortTranslate    = (transX, transY)
                 , viewPortRotate       = vrotate }
        = Scale vscale vscale . Rotate vrotate . Translate transX transY


-- | Takes a point using screen coordinates, and uses the `ViewPort` to convert
--   it to Picture coordinates. This is the inverse of `applyViewPortToPicture`
--   for points.
invertViewPort :: ViewPort -> Point -> Point
invertViewPort
        ViewPort { viewPortScale        = vscale
                 , viewPortTranslate    = vtrans
                 , viewPortRotate       = vrotate }
        pos
        = rotateV (degToRad vrotate) (mulSV (1 / vscale) pos) Pt.- vtrans


-- | Convert degrees to radians
degToRad :: Float -> Float
degToRad d      = d * pi / 180
{-# INLINE degToRad #-}


-- | Multiply a vector by a scalar.
mulSV :: Float -> Vector -> Vector
mulSV s (x, y)
        = (s * x, s * y)
{-# INLINE mulSV #-}


-- | Rotate a vector by an angle (in radians). +ve angle is counter-clockwise.
rotateV :: Float -> Vector -> Vector
rotateV r (x, y)
 =      (  x * cos r - y * sin r
        ,  x * sin r + y * cos r)
{-# INLINE rotateV #-}
