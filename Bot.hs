{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

import AST
import Lambda
import Parser
import Conversion
import Control.Monad (when, forM_, unless)
import Control.Concurrent (threadDelay)
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import qualified Discord.Requests as R

runBot :: IO ()
runBot = do
    tok <- TIO.readFile "auth-token.secret"
    t   <- runDiscord $ def { discordToken = tok
                            , discordOnEnd = putStrLn "Ended"
                            , discordOnEvent = eventHandler
                            }
    threadDelay (1 `div` 10 * 10^6)
    TIO.putStrLn t

eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis event = case event of
      MessageCreate m -> when (not (fromBot m) && atBot m) $ do
        _ <- restCall dis (R.CreateReaction (messageChannel m, messageId m) "eyes")
        let (_ , text) = T.breakOn " " (messageText m)
        let term = parseTerm $ T.unpack text
        threadDelay (1 `div` 10 * 10^6)
        processTerm dis term m
      _ -> pure ()

processTerm :: DiscordHandle -> [(LTerm,String)] -> Message -> IO ()
processTerm dis [] m = do
    _ <- restCall dis (R.CreateMessageEmbed (messageChannel m) "" $
        def { createEmbedTitle = "Parse Error"
            , createEmbedDescription = "Term failed to parse."
            , createEmbedThumbnail = Just $ CreateEmbedImageUrl
                    "https://i.imgur.com/alvo0uR.png"
            })
    pure ()
processTerm dis ((t,_):_) m = do
    _ <- restCall dis (R.CreateMessageEmbed (messageChannel m) "" $
        def { createEmbedTitle = "test embed"
            , createEmbedFields = [EmbedField "Barendregt Naming" (T.pack $ show t) Nothing
                                  ,EmbedField "De Bruijn Indices" (T.pack $ show $ removeNames stdnc t) Nothing
                                  ]
            , createEmbedThumbnail = Just $ CreateEmbedImageUrl
                    "https://i.imgur.com/alvo0uR.png"
            })
    pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

atBot :: Message -> Bool
atBot m = botId `elem` (userId `map` messageMentions m)

botId :: UserId
botId = 590901458610946071
