{-# LANGUAGE TemplateHaskell #-}

module Components.TH where

import Components.Types (RawElem, Container, present, containery)

import Language.Haskell.TH

mkContainer :: Name -> String -> DecsQ
mkContainer a elName = (++) <$> mkRawElem a elName
                            <*> typedD (mkName $ elName ++ "_")
                                       (appT [t|Container|] (conT a))
                                       (appE [|containery|] (varE $ mkName elName))

mkRawElem :: Name -> String -> DecsQ
mkRawElem a elName = typedD (mkName elName)
                            (appT [t|RawElem|] (conT a))
                            (appE [|present|] (litE $ stringL elName))

typedD :: Name -> TypeQ -> ExpQ -> DecsQ
typedD vName typ expr = sequenceA [ sigD vName typ
                                  , valD (varP vName) (normalB expr) [] ]
