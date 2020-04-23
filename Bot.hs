{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module Bot where

import AST
import Lambda
import Parser
import Conversion
import qualified Evaluator as E
import Control.Monad (when, forM_, unless)
import Control.Concurrent (threadDelay)
import Data.Char (toLower)
import qualified Data.Map as Map
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import qualified Discord.Requests as R

runBot :: IO ()
runBot = do
    ref <- newIORef Map.empty
    tok <- TIO.readFile "auth-token.secret"
    t   <- runDiscord $ def { discordToken = tok
                            , discordOnEnd = putStrLn "Ended"
                            , discordOnEvent = eventHandler ref
                            }
    threadDelay (1 `div` 10 * 10^6)
    TIO.putStrLn t

eventHandler :: IORef (Map.Map MessageId LTerm) -> DiscordHandle -> Event -> IO ()
eventHandler idMap dis (MessageCreate m)
    | (not (fromAnyBot m) && atBot m) = do
        _ <- restCall dis (R.CreateReaction (messageChannel m, messageId m) "eyes")
        let (_ , text) = T.breakOn " " (messageText m)
        let term = parseTerm $ T.unpack text
        processTerm dis term m
    | (fromBot m) && isResult m = do
        let term = fst $ head $ parseTerm $ T.unpack $ embedFieldValue $ head $ embedFields $ head (messageEmbeds m)
        atomicModifyIORef idMap (\temp -> (Map.insert (messageId m) term temp, ()))
        _ <- restCall dis (R.CreateReaction (messageChannel m, messageId m) "one")
        _ <- restCall dis (R.CreateReaction (messageChannel m, messageId m) "keycap_ten")
        _ <- restCall dis (R.CreateReaction (messageChannel m, messageId m) "100")
        pure ()
    | otherwise = pure ()
eventHandler idMap dis (MessageReactionAdd r) = when (not $ rFromBot r) $ do
    temp <- readIORef idMap
    case (Map.lookup (reactionMessageId r) temp) of
        Just term -> case (emojiName $ reactionEmoji r) of
            "1️⃣" -> updateMessage idMap dis term 1 (reactionChannelId r) (reactionMessageId r)
            "🔟" -> updateMessage idMap dis term 10 (reactionChannelId r) (reactionMessageId r)
            "💯" -> updateMessage idMap dis term 100 (reactionChannelId r) (reactionMessageId r)
            _ -> pure ()
        Nothing -> pure()
eventHandler _ dis _ = pure ()

updateMessage :: IORef (Map.Map MessageId LTerm) -> DiscordHandle -> LTerm -> Int -> ChannelId -> MessageId -> IO ()
updateMessage idMap dis t i c m = do
    _ <- restCall dis (R.DeleteAllReactions (c, m))
    let result = E.eval t
    _ <- restCall dis (R.CreateMessageEmbed c "" $
        def { createEmbedTitle = "Result"
            , createEmbedFields = [EmbedField "Barendregt Naming" (T.pack $ filter (/= '"') $ show result) Nothing
                                  ,EmbedField "De Bruijn Indices" (T.pack $ show $ removeNames stdnc result) Nothing
                                  ]
            , createEmbedDescription = "click on the numbered reactions to evaluate by that many steps."
            , createEmbedThumbnail = Just $ CreateEmbedImageUrl
                "https://i.imgur.com/alvo0uR.png"
            })
    atomicModifyIORef idMap (\temp -> (Map.insert m result temp, ()))
    _ <- restCall dis (R.CreateReaction (c, m) "one")
    _ <- restCall dis (R.CreateReaction (c, m) "keycap_ten")
    _ <- restCall dis (R.CreateReaction (c, m) "100")
    pure ()


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
        def { createEmbedTitle = "Result"
            , createEmbedFields = [EmbedField "Barendregt Naming" (T.pack $ filter (/= '"') $ show t) Nothing
                                  ,EmbedField "De Bruijn Indices" (T.pack $ show $ removeNames stdnc t) Nothing
                                  ]
            , createEmbedDescription = "click on the numbered reactions to evaluate by that many steps."
            , createEmbedThumbnail = Just $ CreateEmbedImageUrl
                    "https://i.imgur.com/alvo0uR.png"
            })
    pure ()

fromAnyBot :: Message -> Bool
fromAnyBot m = userIsBot (messageAuthor m)

isResult :: Message -> Bool
isResult m | messageEmbeds m == [] = False
           | embedTitle (head (messageEmbeds m)) == Nothing = False
           | embedTitle (head (messageEmbeds m)) == Just "Result" = True
           | otherwise = False

atBot :: Message -> Bool
atBot m = botId `elem` (userId `map` messageMentions m)

fromBot :: Message -> Bool
fromBot m = botId == (userId $ messageAuthor m)

rFromBot :: ReactionInfo -> Bool
rFromBot r = botId == reactionUserId r

botId :: UserId
botId = 590901458610946071
