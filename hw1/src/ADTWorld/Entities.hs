module ADTWorld.Entities
       ( Entity (..)
       , isKnight
       , fight
       , showFightResult
       ) where

data Entity
    = Knight  { name :: String, attack :: Int, health :: Int }
    | Monster { name :: String, attack :: Int, health :: Int }
    deriving (Eq, Show)

isKnight :: Entity -> Bool
isKnight Knight{} = True
isKnight _        = False

fight :: Entity -> Entity -> (Entity, Entity, Int)
fight fstEntity sndEntity
  | isKnight fstEntity = fightImpl (fstEntity, sndEntity, 0)
  | otherwise          = fightImpl (sndEntity, fstEntity, 0)
  where
    fightImpl :: (Entity, Entity, Int) -> (Entity, Entity, Int)
    fightImpl (e1, e2, rounds)
      | health e1 <= 0 = (e2, e1, rounds)
      | health e2 <= 0 = (e1, e2, rounds)
      | otherwise = fightImpl (e2 { health = health e2 - attack e1 }, e1, rounds + 1)

showFightResult :: (Entity, Entity, Int) ->  IO ()
showFightResult (winner, _, rounds) = putStrLn $ "Winner is " ++ show winner ++ "\n"
                                    ++ "Rounds: " ++ show rounds
