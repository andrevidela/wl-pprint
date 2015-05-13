||| A class to store canonical pretty-printers. This is a separate
||| module to allow users to define their own Pretty class or to have
||| different instances for base types.
module Text.PrettyPrint.Leijen.Class

import Text.PrettyPrint.Leijen

-----------------------------------------------------------
-- overloading "pretty"
-----------------------------------------------------------

||| The member `prettyList` is only used to define the `instance Pretty
||| a => Pretty [a]`. In normal circumstances only the `pretty` function
||| is used.
class Pretty a where
  ||| The canonical pretty-printer for a type
  pretty        : a -> Doc
  prettyList    : List a -> Doc
  prettyList    = list . map pretty

instance Pretty a => Pretty (List a) where
  pretty        = prettyList

instance Pretty Doc where
  pretty        = id

instance Pretty () where
  pretty ()     = text "()"

instance Pretty Bool where
  pretty b      = bool b

instance Pretty Char where
  pretty c      = char c
  prettyList s  = string (pack s)

instance Pretty Int where
  pretty i      = int i

instance Pretty Integer where
  pretty i      = integer i

instance Pretty Float where
  pretty f      = float f

-- instance Pretty Double where
--   pretty d      = double d


--instance Pretty Rational where
--  pretty r      = rational r

instance (Pretty a,Pretty b) => Pretty (a,b) where
  pretty (x,y)  = tupled [pretty x, pretty y]

-- instance (Pretty a,Pretty b,Pretty c) => Pretty (a,b,c) where
--   pretty (x,y,z)= tupled [pretty x, pretty y, pretty z]

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing        = empty
  pretty (Just x)       = pretty x


