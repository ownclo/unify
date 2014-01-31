{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

import qualified Control.Lens as L
import Control.Monad
import Control.Monad.Operational

data Foo = Foo
    { _n   :: Word8
    , _lst :: [Word8]
    } deriving (Show, Eq)
L.makeLenses ''Foo

data IsoM f b v = (Monad f, Monad b) => IsoM
    { forward  :: v -> f () -- printer
    , backward :: b v       -- parser
    }

data SerializerStep f b a v = (Monad f, Monad b) => Step (IsoM f b v) (L.Lens' a v)
type Description f b a = Program (SerializerStep f b a)

mkIso :: (Monad f, Monad b) => Description f b a a -> IsoM f b a
mkIso desc = IsoM forw backw
    where forw = printD desc
          backw = parseD desc

repeatIso :: (Monad f, Monad b)
          => Int -> IsoM f b v -> IsoM f b [v]
repeatIso num iso = IsoM forw back
    where forw = mapM_ (forward iso)            -- :: [v] -> f ()
          back = replicateM num $ backward iso  -- :: b [v]

times :: (Monad f, Monad b)
      => Int -> IsoM f b v
      -> L.Lens' a [v] -> Description f b a [v]
times num iso lens = singleton $ Step (repeatIso num iso) lens

parseD :: (Monad f, Monad b) => Description f b a a -> b a
parseD = eval . view
    where eval :: (Monad f, Monad b) => ProgramView (SerializerStep f b a) a -> b a
          eval (Return val) = return val
          eval (Step iso _ :>>= k) = do
              x <- backward iso
              eval . view $ k x

-- initial value is used for lensing (it's printed out)
printD :: (Monad f, Monad b) => Description f b a a -> a -> f ()
printD desc val = eval val $ view desc
    where eval :: (Monad f, Monad b) => a -> ProgramView (SerializerStep f b a) v -> f ()
          eval _ (Return _) = return ()
          eval v (Step iso lens :>>= k) = do
              let x = L.view lens v
              forward iso x
              eval v . view $ k x

byteIso :: IsoM PutM Get Word8
byteIso = IsoM put get

byte :: L.Lens' a Word8 -> Description PutM Get a Word8
byte lens = singleton $ Step byteIso lens

fooIso :: IsoM PutM Get Foo
fooIso = mkIso $ do
    n'   <- byte n
    lst' <- times (fromIntegral n') byteIso lst
    return $ Foo n' lst'

instance Binary Foo where
--     get :: Get Foo
    get = do
        len  <- fmap fromIntegral getWord8
        list <- replicateM (fromIntegral len) getWord8
        return $ Foo len list

--     put :: Foo -> Put
    put (Foo len list) = do
        put len
        mapM_ put list

main :: IO ()
main = do
    let encoDesc = runPut $ forward fooIso foo
    print $ runGet (backward fooIso) encoDesc

    let enco = encode foo
        deco = decode enco

    print $ deco == foo

    where foo = Foo 6 [1,2,3,4,5,6]
