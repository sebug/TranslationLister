module TranslationExtractor where

import Text.XML.HXT.DOM.TypeDefs ( XmlTree, XNode(..) )
import Data.Tree.NTree.TypeDefs ( NTree(..) )
import Text.XML.HXT.DOM.QualifiedName ( localPart )

type XmlFilter = XmlTree -> [XmlTree]

isA :: (a -> Bool) -> (a -> [a])
isA p x | p x       = [x]
        | otherwise = []

isXText :: XmlTree -> Bool
isXText (NTree (XText _) _) = True
isXText _ = False

isXTextF :: XmlFilter
isXTextF = isA isXText

getChildren :: XmlFilter
getChildren (NTree n cs) = cs

(>>>) :: XmlFilter -> XmlFilter -> XmlFilter
(f >>> g) t = concat [ g t' | t' <- f t ]

getGrandChildren :: XmlFilter
getGrandChildren = getChildren >>> getChildren

isDataNode :: XmlTree -> Bool
isDataNode (NTree (XTag tn _) _) = localPart tn == "data"
isDataNode _ = False

isDataNodeF :: XmlFilter
isDataNodeF = isA isDataNode
