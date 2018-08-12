{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Field operations
module FieldElt where

class Show a => FieldElt a where
        zero      :: a

        -- | Add all elements.
        (~+~)     :: a    -> a     -> a

        -- | Subtract elements from second from first.
        (~-~)     :: a    -> a     -> a

        -- | Multiply all elements.
        (~*~)     :: a    -> Float -> a

        -- | Divide first elemends by the second.
        (~/~)     :: a    -> Float -> a

        -- | Negate all mambers.
        negate    :: a    -> a

        -- | Masked addition.
        addIf     :: Bool -> a     -> a -> a

        -- | Masked filter, members with their corresponding flags set
        --   to False get zero.
        useIf     :: Bool -> a     -> a


instance FieldElt Float where
        zero = 0
        {-# INLINE zero #-}

        (~+~) a b   = a + b
        {-# INLINE (~+~) #-}

        (~-~) a b   = a - b
        {-# INLINE (~-~) #-}

        (~*~) a b   = a * b
        {-# INLINE (~*~) #-}

        (~/~) a b   = a / b
        {-# INLINE (~/~) #-}

        negate a    = 0 ~-~ a
        {-# INLINE negate #-}

        addIf True  a b = a + b
        addIf False _ b = b
        {-# INLINE addIf #-}

        useIf True  a = a
        useIf False _ = 0
        {-# INLINE useIf #-}


instance FieldElt (Float, Float) where
        zero = (0, 0)
        {-# INLINE zero #-}

        (~+~) (a1, a2) (b1, b2) = (c1, c2)
         where  !c1 = a1 + b1
                !c2 = a2 + b2
        {-# INLINE (~+~) #-}

        (~-~) (a1, a2) (b1, b2) = (c1, c2)
         where  !c1 = a1 - b1
                !c2 = a2 - b2
        {-# INLINE (~-~) #-}

        (~*~) (a1, a2)  b       = (c1, c2)
         where  !c1 = a1 * b
                !c2 = a2 * b
        {-# INLINE (~*~) #-}

        (~/~) (a1, a2)  b       = (c1, c2)
         where  !c1 = a1 / b
                !c2 = a2 / b
        {-# INLINE (~/~) #-}

        addIf True  a b  = a ~+~ b
        addIf False _ b  = b
        {-# INLINE addIf #-}

        useIf True  a   = a
        useIf False _   = (0, 0)
        {-# INLINE useIf #-}

        negate (a1, a2)
         = (~-~) (0, 0) (a1, a2)
        {-# INLINE negate #-}

