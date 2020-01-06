{-# LANGUAGE OverloadedStrings #-}
import           Data.List              (isSuffixOf)
import           Data.Monoid            (mappend)
import           System.FilePath.Posix  (takeBaseName,takeDirectory
                                         ,(</>),dropExtension)
import           Control.Monad          (forM_)
import           Hakyll
--------------------------------------------------------------------------------
siteTitle :: String
siteTitle = "Christoffer Kaltenbrunner"

-- Root URL
root :: String
root = "https://christofferkaltenbrunner.com"
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    -- Copy static content
    forM_ [ "favicon.ico"
          , "robots.txt"
          , "images/*"
          ] $ \f -> match f $ do
        route   idRoute
        compile copyFileCompiler

    -- Render pages
    match "pages/*.md" $ do
        route   $ prettyRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html"
                  (defaultContext `mappend` metaDescContext)
            >>= relativizeUrls
            >>= cleanIndexUrls

    -- Render plain text sitemap
    create ["sitemap.txt"] $ do
        route idRoute
        compile $ do
            pages <- loadAll ( "pages/*.md"               .&&.
                               complement "pages/404.md"  .&&.
                               complement "pages/index.md"    )
            let sitemapCtx =
                     constField "root"  root `mappend`
                     listField  "pages" pageSitemapCtx (return pages)
            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.txt" sitemapCtx
                >>= cleanIndexHtmls

    -- Render CSS
    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    -- Read templates
    match "templates/*" $
        compile templateBodyCompiler
--------------------------------------------------------------------------------
-- Context
pageSitemapCtx :: Context String
pageSitemapCtx =
    constField "root" root `mappend`
    defaultContext

metaDescContext :: Context String
metaDescContext = field "metaDesc" $ \item -> do
  desc <- getMetadataField (itemIdentifier item) "description"
  -- if description is empty return an empty string
  -- else return <meta name="description" content="$description$">
  return $ maybe "" showMetaDesc desc
    where
      showMetaDesc d = "<meta name=\"description\" content=\""
                       ++ d ++ "\">"
--------------------------------------------------------------------------------
-- Make URLs pretty
prettyRoute :: Routes
prettyRoute = customRoute $ \identifier ->
        let filePath   = toFilePath identifier
            dir        = takeDirectory filePath
            baseName   = takeBaseName filePath
            directory  | dir       == "pages" = ""
                       | otherwise            = drop 6 dir
            simpleName | baseName  == "index" = ""
                       | otherwise            = baseName
        in directory </> simpleName </> "index.html"

-- Delete "/index.html" from URLs that Hakyll generates
cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
      pattern = "/index.html"
      replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
            where idx = "index.html"
