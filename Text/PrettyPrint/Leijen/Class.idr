||| An interface to store canonical pretty-printers. This is a separate
||| module to allow users to define their own Pretty interface or to have
||| different instances for base types.
module Text.PrettyPrint.Leijen.Class

import Text.PrettyPrint.Leijen

%default total
%access public export

-----------------------------------------------------------
-- overloading "pretty"
-----------------------------------------------------------

||| The member `prettyList` is only used to define the `instance Pretty
||| a => Pretty [a]`. In normal circumstances only the `pretty` function
||| is used.
interface Pretty a where
  ||| The canonical pretty-printer for a type
  pretty        : a -> Doc
  prettyList    : List a -> Doc
  prettyList    = list . map pretty

Pretty a => Pretty (List a) where
  pretty        = prettyList

Pretty Doc where
  pretty        = id

Pretty () where
  pretty ()     = text "()"

Pretty Bool where
  pretty b      = bool b

Pretty Char where
  pretty c      = char c
  prettyList s  = string (pack s)

Pretty Int where
  pretty i      = int i

Pretty Integer where
  pretty i      = integer i

Pretty Double where
  pretty f      = float f

-- Pretty Double where
--   pretty d      = double d


-- Pretty Rational where
--  pretty r      = rational r

(Pretty a,Pretty b) => Pretty (a,b) where
  pretty (x,y)  = tupled [pretty x, pretty y]

-- (Pretty a,Pretty b,Pretty c) => Pretty (a,b,c) where
--   pretty (x,y,z)= tupled [pretty x, pretty y, pretty z]

Pretty a => Pretty (Maybe a) where
  pretty Nothing        = empty
  pretty (Just x)       = pretty x
