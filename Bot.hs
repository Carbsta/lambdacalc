{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module Bot where

import Parser
import Lambda
import VisualLambda
import Control.Monad (when, forM_, unless)
import Control.Concurrent (threadDelay)
import Data.Char (toLower)
import qualified Data.Map as Map
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as B

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

eventHandler :: IORef (Map.Map MessageId Lambda) -> DiscordHandle -> Event -> IO ()
eventHandler idMap dis (MessageCreate m)
    | (not (fromAnyBot m) && atBot m) = do
        _ <- restCall dis (R.CreateReaction (messageChannel m, messageId m) "eyes")
        let (_ , text) = T.breakOn " " (messageText m)
        let term = parseTerm $ T.unpack text
        case ("stdlib" `T.isInfixOf` text) of
            True -> sendLib dis m
            False -> processTerm dis term m
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

sendLib :: DiscordHandle -> Message -> IO ()
sendLib dis m = do
    _ <- restCall dis (R.CreateMessageEmbed (messageChannel m) "" $
        def { createEmbedTitle = "Standard Library"
            , createEmbedDescription = stdlib
            , createEmbedThumbnail = Just $ CreateEmbedImageUrl
                "https://i.imgur.com/alvo0uR.png"
            })
    pure ()

updateMessage :: IORef (Map.Map MessageId Lambda) -> DiscordHandle -> Lambda -> Int -> ChannelId -> MessageId -> IO ()
updateMessage idMap dis t i c m = do
    _ <- restCall dis (R.DeleteAllReactions (c, m))
    let result = eval t
    render (removeNames stdnc result) tempFile
    file <- B.readFile tempFile
    _ <- restCall dis (R.CreateMessageEmbed c "" $
        def { createEmbedTitle = "Result"
            , createEmbedFields = [EmbedField "Barendregt Naming" (T.pack $ filter (/= '"') $ show result) Nothing
                                  ,EmbedField "De Bruijn Indices" (T.pack $ show $ removeNames stdnc result) Nothing
                                  ]
            , createEmbedDescription = "click on the numbered reactions to evaluate by that many steps."
            , createEmbedImage = Just $ CreateEmbedImageUpload file
            , createEmbedThumbnail = Just $ CreateEmbedImageUrl
                "https://i.imgur.com/alvo0uR.png"
            })
    atomicModifyIORef idMap (\temp -> (Map.insert m result temp, ()))
    _ <- restCall dis (R.CreateReaction (c, m) "one")
    _ <- restCall dis (R.CreateReaction (c, m) "keycap_ten")
    _ <- restCall dis (R.CreateReaction (c, m) "100")
    pure ()


processTerm :: DiscordHandle -> [(Lambda,String)] -> Message -> IO ()
processTerm dis [] m = do
    _ <- restCall dis (R.CreateMessageEmbed (messageChannel m) "" $
        def { createEmbedTitle = "Parse Error"
            , createEmbedDescription = "Term failed to parse."
            , createEmbedThumbnail = Just $ CreateEmbedImageUrl
                    "https://i.imgur.com/alvo0uR.png"
            })
    pure ()
processTerm dis ((t,_):_) m = do
    render (removeNames stdnc t) tempFile
    file <- B.readFile tempFile
    _ <- restCall dis (R.CreateMessageEmbed (messageChannel m) "" $
        def { createEmbedTitle = "Result"
            , createEmbedFields = [EmbedField "Barendregt Naming" (T.pack $ filter (/= '"') $ show t) Nothing
                                  ,EmbedField "De Bruijn Indices" (T.pack $ show $ removeNames stdnc t) Nothing
                                  ]
            , createEmbedDescription = "click on the numbered reactions to evaluate by that many steps."
            , createEmbedImage = Just $ CreateEmbedImageUpload file
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

tempFile :: FilePath
tempFile = "temp.png"

stdlib :: T.Text
stdlib = "```haskell\n\
\id -> λx.x\n\
\succ -> λn.λs.λz.s(n s z)\n\
\plus -> λm.λn.λf.λz.m s(n f z)\n\
\mult -> λm.λn.λf.m (n f)\n\
\pow -> λb.λe.e b\n\
\pred -> λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)\n\
\sub -> λm.λn.n $pred m\n\
\true -> λt.λf.t\n\
\false -> λt.λf.f\n\
\and -> λp.λq.p q p\n\
\or -> λp.λq.p p q\n\
\not -> λp.p $false $true\n\
\if -> λb.λt.λf.b t f\n\
\iszero -> λn.n (λx.$false) $true\n\
\leq -> λm.λn.$iszero ($sub m n)\n\
\eq -> λm.λn.$and ($leq m n) ($leq n m)\n\
\pair -> λf.λs.λb.b f s\n\
\fst -> λp.p $true\n\
\snd -> λp.p $false\n\
\nil -> λx.$true\n\
\null -> λp.p (λx.λy.$false)\n\
\shift -> λx.$pair ($second x) ($succ ($second x))\n\
\fix -> λf.(λx.f (x x))(λx.f(x x))\n\
\fac -> λn.λf.n(λf.λn.n(f(λf.λx.n f(f x))))(λx.f)(λx.x)\n\
\fib -> λn.λf.n(λc.λa.λb.c b(λx.a (b x)))(λx.λy.x)(λx.x)f\n\
\omega -> (λx.x x)(λy.y y)\n\
\```"
