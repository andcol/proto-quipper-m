module Language.LabelContexts where

import LNLHask
import Circuit.Gate
import Circuit.Dynamic.Class
import Language.Core

import GHC.TypeLits hiding (Div)
import Data.Proxy
import Data.Type.Bool

-- |KnownCtx identifies label contexts, i.e. types of kind [(Nat,LType)] where LType = Qubit (for now)
-- reify returns a term-level representation of γ (necessary to initialize idQ in box subroutine)
-- trivialECtx returns an evaluation context which trivially matches each free variable (x :: Nat) (in our case these are only labels)
--  to the corresponding label value (x :: Int) of type Qubit (for now) for evaluation
class KnownCtx (γ :: Ctx) where
    reify :: forall proxy. proxy γ -> LabelContext
    trivialECtx :: forall proxy. proxy γ -> ECtx Deep γ

exp2LabelContext :: KnownCtx γ => Deep γ t -> LabelContext
exp2LabelContext (_ :: Deep γ t) = reify (Proxy :: Proxy γ)

exp2ECtx :: KnownCtx γ => Deep γ t -> ECtx Deep γ
exp2ECtx (_ :: Deep γ t) = trivialECtx (Proxy :: Proxy γ)

instance KnownCtx '[] where
    reify _ = []
    trivialECtx _ = eEmpty
instance (KnownNat x, KnownCtx γ', (AddF x Qubit γ') ~ ('(x, Qubit) : γ')) => KnownCtx ('(x,Qubit) : γ') where --the additional and very specific constraint can only be discharged when a concrete type is supplied
    reify _ = (reifyLabel (Proxy :: Proxy x),Qubit) : (reify (Proxy :: Proxy γ'))
    trivialECtx _ = addECtx (Proxy :: Proxy x) (VLabel $ reifyLabel (Proxy :: Proxy x)) (trivialECtx (Proxy :: Proxy γ'))

-- |LabelCtx identifies non-empty label contexts, i.e. types of kind [(Nat,LType)] where LType = Qubit (for now) which can be given an MType t
-- labelVector turns γ into a vector (tensoring) of labels, where each label is a variable from γ.
--  The type t of the resulting vector is similarly  given by the types in γ 
class (KnownCtx γ, MType Deep t) => LabelCtx (γ :: Ctx) (t :: LType) | γ -> t where
    labelVector :: Proxy γ -> Deep γ t
instance (KnownCtx γ, MType Deep t, singleton ~ IsSingleton γ, LabelCtx' singleton γ t) => LabelCtx γ t where
    labelVector = labelVector' (Proxy :: Proxy singleton)

-- The level of indirection is needed to prevent functional dependencies from conflicting ('['(x,Qubit)] and ('(x,Qubit):γ') overlap)

type family IsSingleton (γ :: Ctx) :: Bool where -- |Returns 'True iff γ is a singleton context
    IsSingleton '[ '(x,w) ] = 'True
    IsSingleton γ = 'False

class (KnownCtx γ, MType Deep t) => LabelCtx' (singleton :: Bool) (γ :: Ctx) (t :: LType) | singleton γ -> t where
    labelVector' :: Proxy singleton -> Proxy γ -> Deep γ t
instance KnownNat x => LabelCtx' 'True '[ '(x, Qubit) ] Qubit where
    labelVector' _ _ = label (Proxy :: Proxy x)
instance (KnownNat x, LabelCtx γ' t',
            -- The remaining constraints do not reflect the logic of LabelCtx
            --  they are trivial equivalences that can only be resolved by GHC once a concrete type is supplied (i.e. not now)
            (AddF x Qubit γ') ~ ('(x, Qubit) : γ'), -- True because ('(x,Qubit) : γ') is a context and thus it is ordered in the first place (1)
            (Remove x (Div ('(x, Qubit) : γ') γ')) ~ '[], -- Trivially true (if I remove all xs and then x from x:xs I get [])
            (Div ('(x, Qubit) : γ') γ') ~ '[ '(x, Qubit) ], -- Trivially true (if I remove all xs from x:xs I get [x])
            (MergeF γ' '[]) ~ γ', -- Trivially true (but MergeF is defined inductively on the first argument)
            (MergeF γ' '[ '(x, Qubit) ]) ~ ('(x, Qubit) : γ'), -- True, see (1)
            (Div γ' γ') ~ '[]) -- Trivially true (if I remove all xs from xs I get [])
                => LabelCtx' 'False ('(x,Qubit) : γ') (Qubit ⊗ t') where
    labelVector' _ _ = label (Proxy :: Proxy x) ⊗ labelVector (Proxy :: Proxy γ')

--GENERAL TYPE FUNCTIONS

type family (++) (list1 :: [a]) (list2 :: [a]) :: [a] where -- |The type-level counterpart of ++
    '[] ++ list2 = list2
    list1 ++ '[] = list1
    (e ': rl) ++ list2 = e ': (rl ++ list2)

type family Zip (l1 :: [a]) (l2 :: [b]) :: [(a,b)] where -- |The type-level counterpart of the zip function
    Zip '[] l2 = '[]
    Zip l1 '[] = '[]
    Zip (e1 ': r1) (e2 ': r2) = '(e1,e2) ': (Zip r1 r2)

type family Range (start :: Nat) (n :: Nat) :: [Nat] where -- |The type-level counterpart of [start..start+n]
    Range start 1 = '[start]
    Range start n = start ': (Range (start + 1) (n - 1))

--FUNCTIONS FOR MTYPES

type family MLen (t :: LType) :: Nat where -- |Returns the length of an MType, defined as the number of factors involved in its tensor product
    MLen Qubit = 1
    MLen (Qubit ⊗ t') = (MLen t') + 1

type family ToList (t :: LType) :: [LType] where -- |Returns a list-like representation of an MType (think of it as an unfolding)
    ToList Qubit = '[Qubit]
    ToList (Qubit ⊗ t') = Qubit ': (ToList t')

--FRESH LABEL GENERATION

type family FreshLabel (γ :: Ctx) :: Nat where -- |Generates a single label identifier (Nat) which is fresh in γ 
    FreshLabel '[] = 0
    FreshLabel '[ '(x,t) ] = x+1
    FreshLabel ('(x,t) : γ') = FreshLabel γ' --Ctx's are ordered

type family FreshLabels (γ :: Ctx) (n :: Nat) :: [Nat] where -- |Generates n consecutive label identifiers (Nat), all fresh in γ
    FreshLabels γ 1 = '[FreshLabel γ]
    FreshLabels γ n = Range (FreshLabel γ) n

type family FreshLabelContext (γ :: Ctx) (t :: LType) :: Ctx where -- |Generates a type-level label context which has labels fresh in γ and type t
    FreshLabelContext γ t = Zip (FreshLabels γ (MLen t)) (ToList t)