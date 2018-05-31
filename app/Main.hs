module Main
  ( main
  , sendSIGUSR1
  ) where

import Mitchell

import Eval (evaluate, force)
import Exception (bracket, bracket_)
import File
import File.Text (readFile, writeFile)
import Foldable.Partial (maximum)
import FRP
import Process (getProcessID)
import Random
import Read (readMaybe)
import Str.Utf16 (pack, unpack)
import System.Posix.Signals
import System.Random.Shuffle (shuffleM)
import Trie (Trie)

import qualified Data.Text.Lazy.IO as Str.Lazy.Utf16
import qualified List
import qualified Graphics.Vty as Vty
import qualified Str.Latin1
import qualified Str.Lazy.Binary
import qualified Str.Lazy.Utf16
import qualified Str.Lazy.Utf16 as Str.Lazy (Utf16)
import qualified Trie

main :: IO ()
main = do
  words :: Trie () <- do
    bytes :: Str.Lazy.Utf16 <-
      Str.Lazy.Utf16.readFile "/usr/share/dict/words"
    let words :: Trie ()
        words =
          bytes
            & Str.Lazy.Utf16.lines
            & List.filter ((> 2) . Str.Lazy.Utf16.length)
            & map Str.Lazy.Utf16.toLower
            & List.filter (Str.Lazy.Utf16.all (\c -> c >= 'a' && c <= 'z'))
            & force
            & map
                ((, ()) . Str.Lazy.Binary.toStrict . Str.Lazy.Utf16.encodeUtf8)
            & Trie.fromList
    evaluate words

  bracket_
    (getProcessID >>= writeFile ".pidfile" . pack . show)
    (removePathForcibly ".pidfile")
    (bracket
      (Vty.mkVty Vty.defaultConfig)
      Vty.shutdown
      (main_ words))

main_ :: Trie () -> Vty.Vty -> IO ()
main_ words vty = do
  (c0, r0) <- Vty.displayBounds (Vty.outputIface vty)
  (tickAddHandler, fireTick) <- newAddHandler
  (resizeAddHandler, fireResize) <- newAddHandler
  (keyAddHandler, fireKey) <- newAddHandler
  network <-
    compile $ do
      eTick <- fromAddHandler tickAddHandler
      eResize <- fromAddHandler resizeAddHandler
      eKey <- fromAddHandler keyAddHandler
      bGameState <- moment words (r0, c0) eTick eResize eKey
      let render :: GameState -> IO ()
          render =
            Vty.update vty . Vty.picForImage . view
      liftIO . render =<< valueB bGameState
      eGameState <- changes bGameState
      reactimate' ((fmap.fmap) render eGameState)
  actuate network
  void . forkIO . forever $ do
    fireTick ()
    threadDelay 1000000
  fix $ \loop ->
    Vty.nextEvent vty >>= \case
      Vty.EvKey Vty.KEsc [] ->
        pure ()
      Vty.EvKey key _ -> do
        fireKey key
        loop
      Vty.EvResize c r -> do
        fireResize (r, c)
        loop
      _ ->
        loop

data GameState = GameState
  { gameTerminalSize :: (Int, Int)
  , gameScore :: Int
  , gameSupply :: [Char]
  , gameTyped :: [Char]
  , gameEntered :: [[Char]]
  }

moment
  :: Trie ()
  -> (Int, Int)
  -> Event ()
  -> Event (Int, Int)
  -> Event Vty.Key
  -> MomentIO (Behavior GameState)
moment words size0 eTick eResize eKey = mdo
  let isWord :: [Char] -> Bool
      isWord cs =
        Trie.member (Str.Latin1.pack cs) words

  gen :: GenIO <-
    liftIO createSystemRandom

  let eChar :: Event Char
      eChar =
        filterJust
          ((\case
            Vty.KChar c ->
              Just c
            _ -> Nothing)
          <$> eKey)

  let eEnter :: Event Vty.Key
      eEnter =
        filterE (== Vty.KEnter) eKey

  let eBackspace :: Event Vty.Key
      eBackspace =
        filterE (== Vty.KBS) eKey

  let eTab :: Event Char
      eTab =
        filterE (== '\t') eChar

  let eReset :: Event Char
      eReset =
        filterE (== 'R') eChar

  bTerminalSize :: Behavior (Int, Int) <-
    stepper size0 eResize

  bSupply :: Behavior [Char] <- do
    let newSupply :: Int -> MomentIO [Char]
        newSupply n =
          liftIO (replicateM n (randomChar gen))

    supply0 :: [Char] <-
      newSupply 6

    eNewSupply :: Event [Char] <- do
      let f :: [[Char]] -> Int
          f = \case
            [] ->
              6
            ws ->
              max 6 (2 + maximum (map length ws))

      execute
        (unionWith const
          (filterJust
            ((\xs ys zs -> do
              guard (length xs <= 2)
              pure (newSupply (f (zs:ys))))
              <$> bSupply
              <*> bEntered
              <@> eEnteredWord))
          ((\ws -> newSupply (f ws))
            <$> bEntered
            <@  eReset))

    eShuffled :: Event [Char] <-
      execute (liftIO . shuffleM <$> (bSupply <@ eTab))

    accumB supply0 (unions
      [ const . snd <$> eTyped1
      , (++) <$> bTyped <@ eBackspace
      , const <$> eNewSupply
      , (++) . List.reverse <$> eEnteredJunk
      , const <$> eShuffled
      ])

  let eTyped1 :: Event (Char, [Char])
      eTyped1 =
        filterJust ((\cs c -> (c,) <$> deleted c cs) <$> bSupply <@> eChar)

  let eEntered :: Event (Either [Char] [Char])
      eEntered =
        (\cs ->
          if isWord cs
            then Right cs
            else Left cs)
        <$> bTyped <@ eEnter

  let eEnteredJunk :: Event [Char]
      eEnteredWord :: Event [Char]
      (eEnteredJunk, eEnteredWord) =
        split eEntered

  bTyped :: Behavior [Char] <-
    accumB [] (unions
      [ filterJust $ (\cs c ->
          if elem c cs
            then Just (++ [c])
            else Nothing) <$> bSupply <@> eChar
      , const [] <$ eEnter
      , const [] <$ eBackspace
      , const [] <$ eReset
      ])

  bEntered :: Behavior [[Char]] <-
    accumB []
      (filterJust
        ((\case
          Right word ->
            Just (word:)
          _ ->
            Nothing)
        <$> eEntered))

  bScore :: Behavior Int <-
    accumB 0 (unions
      [ (\(length -> supply) word score ->
          score
            + (length word * 3 `div` 2)
            - (if supply <= 2 then supply else 0))
          <$> bSupply
          <@> eEnteredWord
      , subtract . length <$> bSupply <@ eReset
      ])

  let bGameState :: Behavior GameState
      bGameState =
        GameState
          <$> bTerminalSize
          <*> bScore
          <*> bSupply
          <*> bTyped
          <*> bEntered

  pure bGameState

randomChar :: MonadIO m => GenIO -> m Char
randomChar gen =
  liftIO (uniformR (1::Int, 182303) gen) >>= \case
    n | n <=  21912 -> pure 'e'
      | n <=  38499 -> pure 't'
      | n <=  53309 -> pure 'a'
      | n <=  67312 -> pure 'o'
      | n <=  80630 -> pure 'i'
      | n <=  93296 -> pure 'n'
      | n <= 104746 -> pure 's'
      | n <= 115723 -> pure 'r'
      | n <= 126518 -> pure 'h'
      | n <= 134392 -> pure 'd'
      | n <= 141645 -> pure 'l'
      | n <= 146891 -> pure 'u'
      | n <= 151834 -> pure 'c'
      | n <= 156595 -> pure 'm'
      | n <= 160795 -> pure 'f'
      | n <= 164648 -> pure 'y'
      | n <= 168467 -> pure 'w'
      | n <= 172160 -> pure 'g'
      | n <= 175476 -> pure 'p'
      | n <= 178191 -> pure 'b'
      | n <= 180210 -> pure 'v'
      | n <= 181467 -> pure 'k'
      | n <= 181782 -> pure 'x'
      | n <= 181987 -> pure 'q'
      | n <= 182175 -> pure 'j'
      | n <= 182303 -> pure 'z'
      | otherwise   -> undefined


view :: GameState -> Vty.Image
view game =
  Vty.string Vty.defAttr (List.reverse (gameSupply game))
  +-+
  Vty.string Vty.defAttr ("> " ++ gameTyped game)
  +-+
  Vty.string Vty.defAttr ("[Score: " ++ show (gameScore game) ++ "]")
  +-+
  viewEntered (gameTerminalSize game) (List.reverse (gameEntered game))

viewEntered :: (Int, Int) -> [[Char]] -> Vty.Image
viewEntered (r, c) entered =
  Vty.horizCat
    (List.intersperse (Vty.string Vty.defAttr " ")
      (map viewCol (List.chunksOf (r-2) entered)))
 where
  viewCol :: [[Char]] -> Vty.Image
  viewCol =
    Vty.vertCat . map (Vty.string Vty.defAttr)

(+-+) :: Vty.Image -> Vty.Image -> Vty.Image
(+-+) =
  Vty.vertJoin
infixr 4 +-+

(+|+) :: Vty.Image -> Vty.Image -> Vty.Image
(+|+) =
  Vty.horizJoin
infixr 4 +|+

deleted :: Char -> [Char] -> Maybe [Char]
deleted _ [] = Nothing
deleted x (y:ys)
  | x == y = Just ys
  | otherwise = (y:) <$> deleted x ys

--------------------------------------------------------------------------------

sendSIGUSR1 :: IO ()
sendSIGUSR1 =
  go <|> pure ()
 where
  go :: IO ()
  go = do
    pidstr <- readFile ".pidfile"
    Just pid <- pure (readMaybe (unpack pidstr))
    signalProcess sigUSR1 pid
