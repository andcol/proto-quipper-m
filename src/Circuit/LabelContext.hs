module Circuit.LabelContext where
    
import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality
import Data.Proxy


data WireType = Bit | Qubit deriving (Show)
type Label = Nat

type LabelContext = [(Label,WireType)]


--PREDICATES ON LABELS AND LABEL CONTEXTS

class FreshIn (l :: Label) (lc :: LabelContext) --l does not occur as a label in lc (= not OccursIn)
instance (Lookup l lc ~ Nothing) => FreshIn l lc

class OccursIn (l :: Label) (lc :: LabelContext) --l occurs as a label in lc (= not FreshIn)
instance (Lookup l lc ~ Just w) => OccursIn l lc

class AllOccurIn (labels :: [Label]) (lc :: LabelContext) --Each label in labels occurs as a label in lc
instance (OccursIn l lc) => AllOccurIn (l : '[]) lc
instance (OccursIn l lc, AllOccurIn rl lc) => AllOccurIn (l : rl) lc

class CompatibleOver (lc1 :: LabelContext) (labels :: [Label]) (lc2 :: LabelContext) --labels and lc2 have the same size and for every i, the wire type of the i-th label of lc2 in lc2 and the type of the i-th label of labels in lc1 match
instance (Lookup l lc1 ~ Just w) => CompatibleOver lc1 (l : '[]) ('(l',w) : '[])
instance (Lookup l lc1 ~ Just w, CompatibleOver lc1 lr rc2) => CompatibleOver lc1 (l : lr) ('(l',w) : rc2)

class SameSize (l1 :: [a]) (l2 :: [b])
instance SameSize '[] '[]
instance SameSize r1 r2 => SameSize (e1 : r1) (e2 : r2)


--TYPE-LEVEL OPERATIONS ON LABELS AND LABEL CONTEXTS

type family SizeOf (l :: [a]) :: Nat where
    SizeOf '[] = 0
    SizeOf (e ': r) = (SizeOf r) + 1

type family Zip (l1 :: [a]) (l2 :: [b]) :: [(a,b)] where
    Zip l1 '[] = '[]
    Zip '[] l2 = '[]
    Zip (e1 ': r1) (e2 ': r2) = ('(e1,e2) : (Zip r1 r2))

type family Lookup (l :: Label) (lc :: LabelContext) :: Maybe WireType where
    Lookup l '[] = 'Nothing
    Lookup l ('(l,w) : lcr) = 'Just w
    Lookup l ('(l',w) : lcr) = Lookup l lcr

type family LabelsOf (lc :: LabelContext) :: [Label] where
    LabelsOf '[] = '[]
    LabelsOf ('(l,w) : lcr) = l : (LabelsOf lcr)

type family SemanticsOf (lc :: LabelContext) :: [WireType] where
    SemanticsOf '[] = '[]
    SemanticsOf ('(l,w) : lcr) = w : (SemanticsOf lcr)

type family Disjoint (lc1 :: LabelContext) (lc2 :: LabelContext) :: Bool where
    Disjoint lc1 '[] = 'True
    Disjoint '[] lc2 = 'True
    Disjoint ('(l,w) : lcr1) lc2 = Lookup l lc2 == Nothing && Disjoint lcr1 lc2

type family Unwrap (m :: Maybe t) :: t where --TOFO possibly do without? Pattern matching on type in Merge
    Unwrap ('Just t) = t

type family Merge (lc1 :: LabelContext) (lc2 :: LabelContext) :: (Maybe LabelContext) where
    Merge '[] lc2 = 'Just lc2
    Merge ('(l,w) : lcr1) lc2 = If (Lookup l lc2 == Nothing && Disjoint lcr1 lc2) ('Just ('(l,w) : Unwrap (Merge lcr1 lc2))) 'Nothing

type family RemoveLabel (l :: Label) (lc :: LabelContext) :: LabelContext where
    RemoveLabel l '[] = '[]
    RemoveLabel l ('(l,w) : lcr) = lcr
    RemoveLabel l ('(l',w) : lcr) = '(l',w) : (RemoveLabel l lcr)

type family RemoveLabels (labels :: [Label]) (lc :: LabelContext) :: LabelContext where
    RemoveLabels '[] lc = lc
    RemoveLabels (l : lr) lc = RemoveLabels lr (RemoveLabel l lc)


--Production of fresh labels

type family Newest (l1 :: Label) (l2 :: Label) :: Label where
    Newest l l = l
    Newest l1 l2 = If (l1 <=? l2) l2 l1

type family Latest (labels :: [Label]) :: Label where
    Latest '[l] = l
    Latest (l ': rl) = Newest l (Latest rl)

type family MakeFreshLabel (lc :: LabelContext) :: Label where
    MakeFreshLabel '[] = 0
    MakeFreshLabel lc = (Latest (LabelsOf lc)) + 1

type family LabelRange (start :: Label) (n :: Nat) :: [Label] where
    LabelRange start 0 = '[]
    LabelRange start n = start : (LabelRange (start+1) (n-1))

type family MakeFreshLabels (lc :: LabelContext) (n :: Nat) :: [Label] where
    MakeFreshLabels lc n = LabelRange (MakeFreshLabel lc) n


--REIFICATION OF LABELS AND CONTEXTS

class KnownLabelList (l :: [Label]) where
    lablVal :: forall proxy. proxy l -> [Int]
instance KnownLabelList '[] where
    lablVal _ = []
instance (KnownNat x, KnownLabelList r) => KnownLabelList (x ': r) where
    lablVal (_ :: proxy (x ': r)) =  (fromIntegral $ natVal (Proxy :: Proxy x)) : (lablVal (Proxy :: Proxy r))

class KnownWireType (w :: WireType) where
    wireVal :: forall proxy. proxy w -> WireType
instance KnownWireType 'Bit where
    wireVal _ = Bit
instance KnownWireType 'Qubit where
    wireVal _ = Qubit

class KnownLabelContext (lc :: LabelContext) where
    lcVal :: forall proxy. proxy lc -> [(Integer,WireType)]
instance KnownLabelContext '[] where
    lcVal _ = []
instance (KnownNat l, KnownWireType w, KnownLabelContext lcr) => KnownLabelContext ('(l,w) : lcr) where
    lcVal (_ :: proxy ('(l,w) : lcr)) = (natVal (Proxy :: Proxy l), wireVal (Proxy :: Proxy w)) : (lcVal (Proxy :: Proxy lcr))