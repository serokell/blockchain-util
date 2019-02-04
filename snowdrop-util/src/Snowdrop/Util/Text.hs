{-# LANGUAGE DataKinds #-}

-- | Smart buildable stuff, text processing, e.t.c.

module Snowdrop.Util.Text
       (
         Buildables
       , maybeF
       , listF
       , bareListF
       , pairF
       , specifiedF

       , Doc
       , DocParams
       , DBuildable (..)
       , DText
       , DFormat
       , docF
       , dlater
       , (%%)
       , newline
       , newlineF
       , indented
       , indentedLarge
       , putDoc
       , printDoc
       , logDoc
       ) where

import           Universum hiding (head, init, last)

import           Control.Lens (makeLenses, (+~))
import           Data.Default (Default (..))
import qualified Data.Text.Buildable as Buildable
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder (Builder)
import           Data.Union (Union, absurdUnion, union)
import           Data.Vinyl.TypeLevel (RecAll)
import           Fmt (format)
import           Formatting (Format, bprint, build, later, now, sformat, (%))
import           Formatting.Internal (Format (..))
import qualified GHC.Exts as Exts

import           Snowdrop.Util.Logging (ExecM)

type Buildables ts = Each '[Buildable] ts

------------------------------------------------------
-- Formatter combinators
------------------------------------------------------

-- | Modify formatter to accept @Maybe a@
maybeF :: Format Builder (a -> Builder) -> Format r (Maybe a -> r)
maybeF builder = later $ maybe "<none>" (bprint builder)

-- | Modify formatter to accept @[a]@. List will _not_ be surrounded by brackets.
bareListF
    :: Exts.IsList l
    => Builder -> Format Builder (Exts.Item l -> Builder) -> Format r (l -> r)
bareListF delim buildElem =
    later $ \(Exts.toList -> values) ->
    if null values
    then "<none>"
    else mconcat $ intersperse delim $ bprint buildElem <$> values

-- | Modify formatter to accept @[a]@.
listF
    :: Exts.IsList l
    => Builder -> Format Builder (Exts.Item l -> Builder) -> Format r (l -> r)
listF delim buildElem =
    later $ \(Exts.toList -> values) ->
        if null values
        then "[]"
        else mconcat $
             one "[" <> (intersperse delim $ bprint buildElem <$> values) <> one "]"

-- | Formatter for pair. Use with care.
pairF
    :: Format (b -> Builder) (a -> b -> Builder)
    -> Builder
    -> Format Builder (b -> Builder)
    -> Format r ((a, b) -> r)
pairF buildA delim buildB =
    later $ \(a, b) -> bprint (buildA % now delim % buildB) a b

-- | Print entires of pair separated with colon.
specifiedF
    :: Format (b -> Builder) (a -> b -> Builder)
    -> Format Builder (b -> Builder)
    -> Format r ((a, b) -> r)
specifiedF buildA buildB = pairF buildA ": " buildB

------------------------------------------------------
-- Doc
------------------------------------------------------

{- Doc is a type which extends text building opportinities via carrying set of
parameters. These parameters may include indentation, level of masking
sensitive content, dissallowance of coloring e.t.c.

This is type alias for purpose, so that one can freely use existing
formatting stuff without changes, and pass building context only when
it is required.
-}
type Doc = DocParams -> Builder

{- Context carried during text construction.

Note that there is no way to construct objects of this type outside of the
module, instead they are provided by 'Doc'-consuming functions like 'putDoc',
this is done for two reasons:

* Forbid "Doc -> Text" conversions in the middle of text construction.
Once one gained a @Doc@, it should be transformed to @Text@ only just upon
printing, otherwise construction invariants like current indentation accounting
can get broken.

* Way of text construction may depend on where are we going to print the text to.
For instance, later we may want to have several levels of logging sensitivity,
where secret logs contain relatively full information as-is while public logs
mask some potentially deanonymizing content.
-}
data DocParams = DocParams
    { _dpIndent :: Int
      -- ^ Current indentation (in number of spaces).
    }

instance Default DocParams where
    def = DocParams 0

makeLenses ''DocParams

-- | 'Buildable' analogy for 'Doc' type.
-- If your type should be printed in some nontrivial way (e.g. in multiline),
-- use this instance instead of @Buildable@.
class DBuildable a where
    dbuild :: a -> Doc
    default dbuild :: Buildable a => a -> Doc
    dbuild x _ = bprint build x

-- | @Text@ with building context.
type DText = DocParams -> Text

-- | @Format@ with building context.
type DFormat a b = DocParams -> Format a b

instance IsString (DFormat a a) where
    fromString = const . fromString

-- | 'Format' version of 'dbuild'.
docF :: DBuildable a => DFormat r (a -> r)
docF dp = Format $ \toRes x -> toRes $ dbuild x dp

-- | 'Doc' analogy for 'later'.
dlater :: (a -> Doc) -> DocParams -> Format r (a -> r)
dlater f dp = later $ \x -> f x dp

-- | Make a newline respecting current indentation.
newline :: Doc
newline DocParams{ _dpIndent = ind } = mconcat ("\n" : replicate ind " ")

-- | 'Format' version of 'newline'.
newlineF :: DFormat a a
newlineF dp = now $ newline dp

-- | Increase indent.
indented :: DocParams -> DocParams
indented = dpIndent +~ 2

-- | Increase indent on a large value.
indentedLarge :: DocParams -> DocParams
indentedLarge = dpIndent +~ 4

{- Similar to ('%'), used for 'DFormat'. With this you can write things like

@
sformat (dp  &  "1: " %% docF%%newlineF %% "2: " %% docF) x y
@

which is equal to

@
sformat ("1: " % docF dp % newlineF dp % "2: " % docF dp)
@
-}
(%%) :: DFormat b a -> DFormat c b -> DFormat c a
(f1 %% f2) dp = f1 dp % f2 dp
infixl 8 %%

-- | Helper for using @fmt@ library with 'DBuildable'.
--
-- Usage example: @ "Item: "+|item:.dp|+"" @.
data ItemWithDocParams a =
    a :. DocParams
infixr 3 :.

instance DBuildable a => Buildable (ItemWithDocParams a) where
    build (x :. p) = dbuild x p

-- | Parameters used for printing to console.
-- Not for exporting outside (see comment to 'DocParams').
printParams :: DocParams
printParams = DocParams{ _dpIndent = 0 }

-- | 'putText' analogy for doc.
putDoc :: DText -> IO ()
putDoc doc = putTextLn (doc printParams)

-- | 'print' analogy for doc.
printDoc :: DBuildable a => a -> IO ()
printDoc x = putDoc $ \dp -> sformat (docF dp) x

-- | Ability to use a logger.
logDoc :: (T.Text -> ExecM ()) -> DText -> ExecM ()
logDoc logger doc = logger $ format "{}" (doc printParams)

deriving instance Buildable x => Buildable (Identity x)

instance Buildable (Union f '[]) where
    build = absurdUnion

instance (RecAll f  (x ': xs) Buildable, Buildable (Union f xs)) => Buildable (Union f (x ': xs)) where
    build = union Buildable.build Buildable.build

instance DBuildable (Union f '[]) where
    dbuild val _ = absurdUnion val

instance (RecAll f  (x ': xs) DBuildable, DBuildable (Union f xs)) => DBuildable (Union f (x ': xs)) where
    dbuild val dp = union (flip dbuild dp) (flip dbuild dp) val

instance (DBuildable x) => DBuildable [x] where
    dbuild ls dp =
        bprint
          (bareListF ""
              (newlineF dp%"* "%dlater dbuild (indented dp)))
          ls

