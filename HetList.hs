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

--class HConsFD res head tail where  -- | head tail -> res , res head -> tail, tail res -> head where
--  type HConsRes head tail
--  type HConsHead res tail
--  type HConsTail  res head
--  hcons ::  (res ~ HConsRes head tail ,head ~ HConsHead res tail,tail ~ HConsTail res head )=>
--    head -> tail -> res
--class HNilFD res where
--  hnil :: res
--instance HNilFD [a] where
--  hnil = []
--maybe try the the a~b~c trick is due to eric mertens

infixr 3 `hcons`
infixr 3 `VHCons`

--instance   HConsFD [a] a [a] where
--  type HConsTail  [a] a = [a]
--  type HConsHead [a] [a] =  a
--  type HConsRes a [a]  = [a]
--  hcons = (:)


{-

 *HetList>  1 `hcons` 2 `hcons` (hnil)  :: VHList '[Int,Int]
 1 `VHCons` 2 `VHCons` VHNil
 *HetList>  1 `hcons` 2 `hcons` (hnil)  :: [Int]
 [1,2]
 *HetList>  1 `hcons` 'a' `hcons` (hnil)  :: VHList '[Int,Char]
 1 `VHCons` 'a' `VHCons` VHNil
-}



data HPair a b = HP a b




class HetCons (f:: k -> * ) (h :: * ) (tl:: k) (res :: k) | f h tl -> res , f h res -> tl, f res tl -> h  where
  --type HApp :: (k-> *)-> k -> *
  --type HCons f h tl :: k
  --type HUnCons f h res :: k   -- not sure if this is correct
  hcons :: -- (res ~ HCons f h tl  ,tl ~ (HUnCons f h res))  =>
    h -> f tl -> f res


class HetNil (f :: k -> * )  (tl  :: k) where
  hnil :: f tl

instance HetNil  []  ( a :: * ) where
  hnil = []

instance HetNil VHList '[] where
  hnil = VHNil

instance HetCons VHList (a:: *) (bs :: [*]) ((a ': bs) :: [*]) where
  --type HCons VHList a bs = a ': bs
  --type HUnCons VHList a (a ': bs)= bs
  hcons = VHCons


instance (a~b,b~c,a~c)=> HetCons [] (a :: *) (b :: * ) (c:: *) where
  hcons = (:)
  --type HCons [] a a = a
  --type HUnCons [] a a = a

data VHList (xs ::[*] ) where
  VHNil :: VHList '[]
  VHCons :: a -> VHList ls -> VHList (a ': ls)

instance Show (VHList '[]) where
  show _ = "VHNil"

instance (Show (VHList bs), Show a )=> Show (VHList (a ': bs)) where
  show (VHCons a rest) =  show a ++ " `VHCons` " ++ show rest


--let (HRecWrap hrec) = [(::"Foo",7),(::"Bar","hello")]

-- sketch of an idea by gershom


--class FromHList h  r | h  -> r where
--    fromHList :: HList h -> r

--instance FromHList (HList '[]) [a]  where
--    fromHList HNil = []

--instance (FromHList (HList b) [] a) => FromHList (HList (a ': b)) [] a where
--    fromHList (HCons x y) = x : fromHList y
