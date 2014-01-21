{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

import qualified Control.Lens as L
import Control.Applicative
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

data SerializerStep f b a v = Step (IsoM f b v) (L.Lens' a v)
--     Step :: IsoM f b v -> L.Lens' a v -> SerializerStep f b v a

type Description f b a = Program (SerializerStep f b a)

byteIso :: IsoM PutM Get Word8
byteIso = IsoM put get

byte :: L.Lens' a Word8 -> Description PutM Get a Word8
byte lens = singleton $ Step byteIso lens

times :: (Monad f, Monad b)
      => Int -> IsoM f b v
      -> L.Lens' a [v] -> Description f b a [v]
times num iso lens = singleton $ Step repeatIso lens
    where repeatIso = IsoM forw back
          forw = mapM_ (forward iso)
          back = replicateM num $ backward iso

fooDesc :: Description PutM Get Foo ()
fooDesc = do
    len <- fromIntegral <$> byte n
    void $ times len byteIso lst

-- lenses modify initial value
parseD :: (Monad f, Monad b) => a -> Description f b a v -> b a
parseD initVal desc = eval initVal $ view desc
    where eval :: (Monad f, Monad b) => a -> ProgramView (SerializerStep f b a) v -> b a
          eval val (Return _) = return val
          eval val (Step iso lens :>>= k) = do
              x <- backward iso
              let val' = L.set lens x val
              eval val' $ view (k x)

-- initial value is used for lensing
printD :: (Monad f, Monad b) => a -> Description f b a v -> f ()
printD val desc = eval val $ view desc
    where eval :: (Monad f, Monad b) => a -> ProgramView (SerializerStep f b a) v -> f ()
          eval _ (Return _) = return ()
          eval v (Step iso lens :>>= k) = do
              let x = L.view lens v
              forward iso x
              eval v $ view (k x)

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
--     print $ L.view n foo
--     print $ L.set lst [8 :: Word8 ,2,3] foo

    let encoDesc = runPut (printD foo fooDesc)
    print $ runGet (parseD nilFoo fooDesc) encoDesc

    let enco = encode foo
        deco = decode enco

    print $ deco == foo

    where foo = Foo 6 [1,2,3,4,5,6]
          nilFoo = Foo 0 []
