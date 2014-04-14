-- | This module is home for auxiliary functions that augment various other libraries.
module MuPromote.Common.Misc where

-- | Points free manipulation of a list, e.g. @ (+1) <:> (+2) <:> id $ [1,2,3,4] ==> [2,4,3,4] @
(<:>) :: ( a -> a ) -> ( [a] -> [a] ) -> [a] -> [a]
(<:>) hFn tFn (h:t) = hFn h : tFn t
infixr 5 <:>

