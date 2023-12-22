-- A minimal Yampa-like module for testing Arrow instances for correctness.

module SF (module Control.Arrow, module Control.Category, SF(..), runMany) where

import Control.Category
import Control.Arrow

newtype SF a b = SF {runSF :: a -> (b, SF a b)}

run :: SF a b -> a -> (b, SF a b)
run (SF runF) = runF

runMany :: SF a b -> [a] -> ([b], SF a b)
runMany sf [] = ([], sf)
runMany sf (x : xs) =
    let (x', sf') = run sf x
        (xs', sf'') = runMany sf' xs
     in (x' : xs', sf'')

instance Category SF where
    id = SF $ \a -> (a, Control.Category.id)
    SF runF . SF runG = SF $ \a ->
        let (b, g') = runG a
            (c, f') = runF b
         in (c, f' Control.Category.. g')

instance Arrow SF where
    arr f = SF $ \a -> (f a, arr f)
    SF runF *** SF runG = SF $ \(a,b) ->
        let (a', f') = runF a
            (b', g') = runG b
         in ((a', b'), f' *** g')

instance ArrowChoice SF where
    SF runF +++ SF runG = SF $ \a ->
        case a of
          Left l -> let (l', f') = runF l
                     in (Left l', f' +++ SF runG)
          Right r -> let (r', g') = runG r
                      in (Right r', SF runF +++ g')

instance ArrowApply SF where
    app = SF $ \(SF runF, v) ->
        let (o, f') = runF v
         in (o, app)

instance ArrowLoop SF where
    loop (SF runF) = SF $ \a ->
        let ((b, c), f') = runF (a, c)
         in (b, loop f')
