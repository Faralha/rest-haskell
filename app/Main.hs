{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, Object, ToJSON, Value, object, parseJSON, toJSON, withObject, (.:), (.=))
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as TL
import qualified Database.MongoDB as Mongo
import Network.HTTP.Types.Status (status204, status302, status404)
import Network.URI (parseURI)
import Web.Scotty

-- Define the data types
data Link = Link {slug :: String, link :: String, clicks :: Int}

data UpdateLink = UpdateLink {newLink :: String}

instance ToJSON Link where
  toJSON (Link slug link clicks) = object ["slug" .= slug, "link" .= link, "clicks" .= clicks]

instance FromJSON Link where
  parseJSON = withObject "Link" $ \v ->
    Link
      <$> v .: "slug"
      <*> v .: "link"
      <*> pure 0 -- Initialize clicks to 0 for new links

instance FromJSON UpdateLink where
  parseJSON = withObject "UpdateLink" $ \v ->
    UpdateLink
      <$> v .: "link"

-- MongoDB connection URL
mongoUrl :: String
mongoUrl = "mongodb://user-g:g-for-goodluck@db.nafkhanzam.com/pweb-g"

-- MongoDB database and collection
dbName :: Text
dbName = "pweb-g"

collectionName :: Text
collectionName = "links"

-- Main function to run the server
main :: IO ()
main = do
  pipe <- Mongo.connect (Mongo.host "db.nafkhanzam.com")
  let db = dbName

  scotty 8080 $ do
    -- Create a new short URL link
    post "/" $ do
      body <- jsonData :: ActionM Link
      liftIO $
        Mongo.access pipe Mongo.master db $
          Mongo.insert collectionName ["slug" Mongo.=: slug body, "link" Mongo.=: link body, "clicks" Mongo.=: (0 :: Int)]
      status status204

    -- Update an existing short URL link
    put "/:slug" $ do
      slug <- param "slug" :: ActionM Text
      body <- jsonData :: ActionM UpdateLink
      liftIO $
        Mongo.access pipe Mongo.master db $
          Mongo.modify
            (Mongo.select ["slug" Mongo.=: slug] collectionName)
            ["$set" Mongo.=: ["link" Mongo.=: newLink body]]
      status status204

    -- Access a short URL link
    get "/:slug" $ do
      slug <- param "slug" :: ActionM Text
      result <- liftIO $ Mongo.access pipe Mongo.master db $ Mongo.findOne (Mongo.select ["slug" Mongo.=: slug] collectionName)
      case result of
        Just doc -> do
          let link = fromMaybe "" (Mongo.lookup "link" doc)
          liftIO $ Mongo.access pipe Mongo.master db $ Mongo.modify (Mongo.select ["slug" Mongo.=: slug] collectionName) ["$inc" Mongo.=: ["clicks" Mongo.=: (1 :: Int)]]
          status status302
          setHeader "Location" (TL.fromStrict (pack link))
        Nothing -> do
          status status404
          html "<h1>Not Found</h1>"

    -- Delete a short URL link
    delete "/:slug" $ do
      slug <- param "slug" :: ActionM Text
      liftIO $ Mongo.access pipe Mongo.master db $ Mongo.delete (Mongo.select ["slug" Mongo.=: slug] collectionName)
      status status204

    -- Get top 10 links
    get "/" $ do
      docs <- liftIO $ Mongo.access pipe Mongo.master db $ Mongo.rest =<< Mongo.find (Mongo.select [] collectionName) {Mongo.sort = ["clicks" Mongo.=: (-1 :: Int)], Mongo.limit = 10}
      json $ map (\doc -> Link (fromMaybe "" (Mongo.lookup "slug" doc)) (fromMaybe "" (Mongo.lookup "link" doc)) (fromMaybe 0 (Mongo.lookup "clicks" doc))) docs

  Mongo.close pipe