{-# LANGUAGE FlexibleInstances#-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies#-}
{-# LANGUAGE StandaloneDeriving #-}
{-#  LANGUAGE PolyKinds #-}
module HetList where

--import  qualified   Data.Vinyl.Core as VC
--import  qualified   Data.Vinyl.TyFun as TF

infixr 3 `hcons`
infixr 3 `VHCons`
infixr 3 `ConL`



{-

 *HetList>  1 `hcons` 2 `hcons` (hnil)  :: VHList '[Int,Int]
 1 `VHCons` 2 `VHCons` VHNil
 *HetList>  1 `hcons` 2 `hcons` (hnil)  :: [Int]
 [1,2]
 *HetList>  1 `hcons` 'a' `hcons` (hnil)  :: VHList '[Int,Char]
 1 `VHCons` 'a' `VHCons` VHNil
-}


{-


*HetList> let  (VHWrap ls) = VHWrap  $ ("yo" `hcons ` 'a'  `hcons` hnil) in ls
"yo" `VHCons` 'a' `VHCons` VHNil
*HetList> :t let  (VHWrap ls) = VHWrap  $ ("yo" `hcons ` 'a'  `hcons` hnil) in ls
let  (VHWrap ls) = VHWrap  $ ("yo" `hcons ` 'a'  `hcons` hnil) in ls
  :: VHList '[[Char], Char]
*HetList> :t let  (VHWrap ls) = VHWrap  $ ("yo" `hcons ` 7 `hcons` hnil) in ls
let  (VHWrap ls) = VHWrap  $ ("yo" `hcons ` 7 `hcons` hnil) in ls
  :: Num h => VHList '[[Char], h]

-}

{-

*HetList> let ls@(a:as) = 1 `hcons` 2 `hcons` hnil in ls
[1,2]
*HetList> :t let ls@(a:as) = 1 `hcons` 2 `hcons` hnil in ls
let ls@(a:as) = 1 `hcons` 2 `hcons` hnil in ls :: Num h => [h]
-}


{-
*HetList> :t (\x -> case x of ls@(a `ConL` b) -> ls) :: SizedList (S n) a -> SizedList (S n) a
(\x -> case x of ls@(a `ConL` b) -> ls) :: SizedList (S n) a -> SizedList (S n) a
  :: SizedList (S n) a -> SizedList (S n) a

*HetList> :t let ls = 1 `hcons` 2 `hcons` (hnil )  in ls :: (SizedList (S (S Z)) Int)
let ls = 1 `hcons` 2 `hcons` (hnil )  in ls :: (SizedList (S (S Z)) Int)
  :: SizedList (S (S Z)) Int

-}


data VHList (xs ::[*] ) where
  VHNil :: VHList '[]
  VHCons :: a -> VHList ls -> VHList (a ': ls)

instance Show (VHList '[]) where
  show _ = "VHNil"

instance (Show (VHList bs), Show a )=> Show (VHList (a ': bs)) where
  show (VHCons a rest) =  show a ++ " `VHCons` " ++ show rest


data Nat = S Nat | Z
  deriving Show
type S = 'S
type Z = 'Z

data SizedList (n :: Nat) a where
  ZL :: SizedList Z a
  ConL :: a -> SizedList n a -> SizedList (S n) a



{-

1) this works ok
*HetList> :t let (F2N ls) = 1 `hcons` 2 `hcons` (hnil :: Flip2Nat SizedList Int Z )in ls
let (F2N ls) = 1 `hcons` 2 `hcons` (hnil :: Flip2Nat SizedList Int Z )in ls
  :: SizedList (S (S 'Z)) Int

2) this is ok




-}


instance Show (SizedList Z a) where
    show _ = "ZL"

instance (Show a, Show (SizedList n a) ) => Show (SizedList (S n) a) where
  show (ConL a as) =  show a ++ " `ConL` " ++ show as

--newtype Flip2Nat (f :: Nat -> * -> * ) (e :: *) (n :: Nat) = F2N { getFN :: f n e}
  --deriving Show

--instance (m ~ S n)=> HetCons (Flip2Nat SizedList a) a (n :: Nat) (m :: Nat ) where
--  hcons = \h (F2N tl) -> F2N $!ConL h tl

--instance (n~Z)=>HetNil (Flip2Nat SizedList a) n  where
--    hnil = F2N ZL

instance (a ~ b, b~c,a~c, m~(S n)) =>HetCons (SizedList n) (SizedList m) a b c   where
  hcons = ConL

instance  (n~Z)=>HetNil (SizedList n) a where
  hnil = ZL

{-

note, f' res -> h
precludes an ordered hrecord type (i think)
-}

class HetCons (f:: k -> * ) (f':: k -> * ) (h :: * ) (tl:: k) (res :: k)
          | f-> f', f'-> f,f h tl -> res , f h res -> tl, f res tl -> h, f' res -> h
            --, f' h tl -> res , f' res tl -> h , f' h res -> tl
                 where
  hcons :: h -> f tl -> f' res


class HetNil (f:: k -> * ) (a :: k)  where
  hnil :: f a

instance HetNil [] a where
  hnil = []

-- need to use the mertens trick
instance (res ~ '[])=> HetNil VHList res  where
  hnil = VHNil

instance HetCons VHList  VHList (a:: *) (bs :: [*]) ((a ': bs) :: [*]) where
  hcons = VHCons

-- merten trick again
instance (a~b,b~c,a~c)=> HetCons []  [] (a :: *) (b :: * ) (c:: *) where
    hcons = (:)


--instance (ls ~ '[])=>HetNil (VC.Rec el f) ls  where
--  hnil = VC.RNil

--instance (val~ (f  (el TF.$ r) )) =>HetCons (VC.Rec el f) (VC.Rec el f) val  rs (r ': rs) where
--  hcons = (VC.:&)




