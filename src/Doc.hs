{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module Doc where

import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.String
import qualified Data.Text.Lazy              as TL
import           Prelude                     hiding (div, id)
import qualified Prelude                     (id)
import           Text.Blaze.Html5            hiding (contents, head, map)
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Markdown

import           Print
import           Types
import           Warnings

generate :: [AST] -> String
generate = concatMap generate'

generate' :: AST -> String
generate' pkg@(Package {}) = pr pkg
generate' _ = error "generate' called with non-package"

htmlDoc :: [AST] -> Html
htmlDoc asts = docTypeHtml $ do
  H.head $ do
    H.title "MetaModelica Functional Generic Library API Documentation"
    link ! rel "stylesheet" ! type_ "text/css" ! href "test.css"
  body $ do
    astDoc (head asts)

tos :: Print p => p -> Html
tos = fromString . pr

iddl :: Name -> Html -> Html
iddl nam = dl ! id (fromString . pr $ nam)

astDoc :: AST -> Html
astDoc c@(Comment      {}) = div ! class_ "comment" $ tos c
astDoc c@(Constant     {}) = div ! class_ "constant" $ tos c
astDoc (ASTPartFn (PartFn prot nam qs doc ps)) = docFunc True prot nam qs doc ps
astDoc (Function prot nam qs doc ps _ _) = docFunc False prot nam qs doc ps
astDoc c@(MComment     {}) = div ! class_ "mcomment" $ tos c
astDoc c@(Replaceable  {}) = div ! class_ "replaceable" $ tos c
astDoc c@(TypeAlias    {}) = div ! class_ "typealias" $ tos c
astDoc c@(Union        {}) = div ! class_ "union" $ tos c
astDoc (Package _prot nam doc imports contents) = do
  iddl nam $ do
    dt ! class_ "package" $ do
      code "package"
      code ! class_ "name" $ tos nam
    docDoc doc
    dd ! class_ "dependencies" $
      dl $ do
       dt "Dependencies"
       dd $ mapM_ astImportDoc imports
    dd ! class_ "contents" $ mapM_ astDoc contents

docFunc :: Bool -> Maybe Protection -> Name -> [Name] -> Maybe DocString -> [Param] -> Html
docFunc isPartial prot nam qs doc ps = do
  let partialS = (if isPartial then "partial" else "")
  iddl nam ! class_ (fromString $ partialS <++> "function" <++> pr prot) $ do
    dt $ do
      fromString $ if prot == Just Protected then "protected " else ""
      fromString $ partialS <++> "function "
      tos nam
      fromString . pr_polyList $ qs
      "("
      fromString . intercalate ", " . map (\(Input vd) -> pr vd) . filter isInput $ ps
      ") => "
      "("
      fromString . intercalate ", " . map (\(Output vd) -> pr vd) . filter isOutput $ ps
      ")"
    docDoc doc

astImportDoc :: Import -> Html
astImportDoc (Import _ nam _ _) = div ! class_ "import" $ tos nam

docDoc :: Maybe DocString -> Html
docDoc Nothing = dd ! class_ "documentation error" $ "Missing docstring"
docDoc (Just doc) =
  dd ! class_ "documentation" $ parseDocString doc

warnings :: [Warning] -> Html
warnings = ul . mapM_ (li . fromString . show)

-- Docstring parsing

parseDocString :: String -> Html
parseDocString = markdown def . TL.pack . unsee
  where
    unsee :: String -> String
    unsee s = unlines $
      flip map (lines s) (\l -> if ((isPrefixOf "@see " . dropWhile (== ' ')) l)
        then (makeSeeMDLink . drop 5 . dropWhile (== ' ')) l
        else l)

makeSeeMDLink :: String -> String
makeSeeMDLink path = "See [" ++ path ++ "](" ++ url ++ ")"
  where
    url = case splitOn "." path of
      [_] -> "#local"
      quali -> (concat $ intersperse "." $ init quali) ++ ".html#" ++ last quali
