module ADTWorld.Entities
       ( Entity (..)
       , isKnight
       , fight
       ) where

data Entity
    = Knight  { name :: String, attack :: Int, health :: Int }
    | Monster { name :: String, attack :: Int, health :: Int }
    deriving (Show)

isKnight :: Entity -> Bool
isKnight Knight{} = True
isKnight _        = False

fight :: Entity -> Entity -> IO ()
fight fstEntity sndEntity
  | isKnight fstEntity = showResult $ fightImpl (fstEntity, sndEntity, 0)
  | otherwise          = showResult $ fightImpl (sndEntity, fstEntity, 0)
  where
    fightImpl :: (Entity, Entity, Int) -> (Entity, Entity, Int)
    fightImpl (e1, e2, rounds)
      | health e1 <= 0 = (e2, e1, rounds)
      | health e2 <= 0 = (e1, e2, rounds)
      | otherwise = fightImpl (e2 { health = health e2 - attack e1 }, e1, rounds + 1)
    showResult :: (Entity, Entity, Int) -> IO ()
    showResult (e1, _, rounds) = putStrLn $ "Winner is " ++ show e1 ++ "\n"
                                    ++ "Rounds: " ++ show rounds