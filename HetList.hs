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



infixr 3 `hcons`
infixr 3 `VHCons`



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



data VHList (xs ::[*] ) where
  VHNil :: VHList '[]
  VHCons :: a -> VHList ls -> VHList (a ': ls)


data Nat = S Nat | Z

data SizedList (n :: Nat) a where
  ZL :: SizedList Z a
  ConL :: a -> SizedList n a -> SizedList (S n) a




class HetCons (f:: k -> * ) (h :: * ) (tl:: k) (res :: k) | f h tl -> res , f h res -> tl, f res tl -> h  where
  --type HApp :: (k-> *)-> k -> *
  --type HCons f h tl :: k
  --type HUnCons f h res :: k   -- not sure if this is correct
  hcons :: -- (res ~ HCons f h tl  ,tl ~ (HUnCons f h res))  =>
    h -> f tl -> f res


class HetNil (f:: k -> * ) (a :: k)  where
  hnil :: f a

instance HetNil [] a where
  hnil = []

-- need to use the mertens trick
instance (res ~ '[])=> HetNil VHList res  where
  hnil = VHNil

instance HetCons VHList (a:: *) (bs :: [*]) ((a ': bs) :: [*]) where
  --type HCons VHList a bs = a ': bs
  --type HUnCons VHList a (a ': bs)= bs
  hcons = VHCons


instance (a~b,b~c,a~c)=> HetCons [] (a :: *) (b :: * ) (c:: *) where
  hcons = (:)
  --type HCons [] a a = a
  --type HUnCons [] a a = a



