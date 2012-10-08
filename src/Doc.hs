{-# LANGUAGE OverloadedStrings #-}

module Doc where

import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Prelude                     hiding (div)
import           Text.Blaze.Html5            hiding (head, map)
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

import           Print
import           Types

generate :: [AST] -> String
generate = concatMap generate'

generate' :: AST -> String
generate' pkg@(Package {}) = pr pkg

htmlDoc :: [AST] -> Html
htmlDoc asts = docTypeHtml $ do
  H.head $ do
    H.title "MetaModelica Functional Generic Library API Documentation"
    link ! rel "stylesheet" ! type_ "text/css" ! href "test.css"
  body $ do
    astDoc (head asts)

tos :: Print p => p -> Html
tos = fromString . pr

astDoc :: AST -> Html
astDoc c@(Comment      {}) = div ! class_ "comment" $ tos c
astDoc c@(Constant     {}) = div ! class_ "constant" $ tos c
astDoc (Function prot nam qs ds ps _ _) =
  dl ! class_ (fromString $ "function" <++> pr prot) $ do
    dt $ do
      fromString $ if prot == Just Protected then "protected " else ""
      "function "
      tos nam
      fromString . pr_polyList $ qs
      "("
      fromString . intercalate ", " . map (\(Input vd) -> pr vd) . filter isInput $ ps
      ") => "
      "("
      fromString . intercalate ", " . map (\(Output vd) -> pr vd) . filter isOutput $ ps
      ")"
    dl $ fromString $ fromMaybe "<<Missing Docs>>" ds

astDoc c@(MComment     {}) = div ! class_ "mcomment" $ tos c
astDoc c@(ASTPartFn    {}) = div ! class_ "partfn" $ tos c
astDoc c@(Replaceable  {}) = div ! class_ "replaceable" $ tos c
astDoc c@(TypeAlias    {}) = div ! class_ "typealias" $ tos c
astDoc c@(Union        {}) = div ! class_ "union" $ tos c
astDoc (Package _prot nam _doc imports contents) = do
  dl $ do
    dt ! class_ "package" $ do
      code "package"
      code ! class_ "name" $ tos nam
    dd ! class_ "dependencies" $
      dl $ do
       dt "Dependencies"
       dd $ mapM_ astImportDoc imports
    dd ! class_ "contents" $ mapM_ astDoc contents

astImportDoc :: Import -> Html
astImportDoc c@(Import _ nam _ _) = div ! class_ "import" $ tos nam
