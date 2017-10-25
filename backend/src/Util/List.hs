{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Util.List (
    listHoldWithKey
  , listWithKey
  , listWithKey'
  , listWithKeyShallowDiff
  , listViewWithKey
  , selectViewListWithKey
  , selectViewListWithKey_
  , list
  , simpleList
  ) where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Data.Either
import Data.Functor.Misc

import Data.Align
import Data.These

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex

applyMap :: Ord k => Map k (Maybe v) -> Map k v -> Map k v
applyMap patch old = insertions `Map.union` (old `Map.difference` deletions)
  where (deletions, insertions) = mapPartitionEithers $ maybeToEither <$> patch
        maybeToEither = \case
          Nothing -> Left ()
          Just r -> Right r

mapPartitionEithers :: Map k (Either a b) -> (Map k a, Map k b)
mapPartitionEithers m = (fromLeft <$> ls, fromRight <$> rs)
  where (ls, rs) = Map.partition isLeft m
        fromLeft (Left l) = l
        fromLeft _ = error "mapPartitionEithers: fromLeft received a Right value; this should be impossible"
        fromRight (Right r) = r
        fromRight _ = error "mapPartitionEithers: fromRight received a Left value; this should be impossible"

listHoldWithKey :: forall t m k v a. (Ord k, MonadAdjust t m, MonadHold t m) => Map k v -> Event t (Map k (Maybe v)) -> (k -> v -> m a) -> m (Dynamic t (Map k a))
listHoldWithKey m0 m' f = do
  let dm0 = mapWithFunctorToDMap $ Map.mapWithKey f m0
      dm' = fmap (PatchDMap . mapWithFunctorToDMap . Map.mapWithKey (\k v -> ComposeMaybe $ fmap (f k) v)) m'
  (a0, a') <- sequenceDMapWithAdjust dm0 dm'
  fmap dmapToMap . incrementalToDynamic <$> holdIncremental a0 a' --TODO: Move the dmapToMap to the righthand side so it doesn't get fully redone every time

--TODO: Something better than Dynamic t (Map k v) - we want something where the Events carry diffs, not the whole value
listWithKey :: forall t k v m a. (Ord k, MonadAdjust t m, PostBuild t m, MonadFix m, MonadHold t m) => Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Dynamic t (Map k a))
listWithKey vals mkChild = do
  postBuild <- getPostBuild
  let childValChangedSelector = fanMap $ updated vals
      -- We keep track of changes to children values in the mkChild function we pass to listHoldWithKey
      -- The other changes we need to keep track of are child insertions and deletions. diffOnlyKeyChanges
      -- keeps track of insertions and deletions but ignores value changes, since they're already accounted for.
      diffOnlyKeyChanges olds news = flip Map.mapMaybe (align olds news) $ \case
        This _ -> Just Nothing
        These _ _ -> Nothing
        That new -> Just $ Just new
  rec sentVals :: Dynamic t (Map k v) <- foldDyn applyMap Map.empty changeVals
      let changeVals :: Event t (Map k (Maybe v))
          changeVals = attachWith diffOnlyKeyChanges (current sentVals) $ leftmost
                         [ updated vals
                         , tag (current vals) postBuild --TODO: This should probably be added to the attachWith, not to the updated; if we were using diffMap instead of diffMapNoEq, I think it might not work
                         ]
  listHoldWithKey Map.empty changeVals $ \k v ->
    mkChild k =<< holdDyn v (select childValChangedSelector $ Const2 k)

{-# DEPRECATED listWithKey' "listWithKey' has been renamed to listWithKeyShallowDiff; also, its behavior has changed to fix a bug where children were always rebuilt (never updated)" #-}
listWithKey' :: (Ord k, MonadAdjust t m, MonadFix m, MonadHold t m) => Map k v -> Event t (Map k (Maybe v)) -> (k -> v -> Event t v -> m a) -> m (Dynamic t (Map k a))
listWithKey' = listWithKeyShallowDiff

-- | Display the given map of items (in key order) using the builder function provided, and update it with the given event.  'Nothing' update entries will delete the corresponding children, and 'Just' entries will create them if they do not exist or send an update event to them if they do.
listWithKeyShallowDiff :: (Ord k, MonadAdjust t m, MonadFix m, MonadHold t m) => Map k v -> Event t (Map k (Maybe v)) -> (k -> v -> Event t v -> m a) -> m (Dynamic t (Map k a))
listWithKeyShallowDiff initialVals valsChanged mkChild = do
  let childValChangedSelector = fanMap $ fmap (Map.mapMaybe id) valsChanged
  sentVals <- foldDyn applyMap Map.empty $ fmap (fmap void) valsChanged
  let relevantPatch patch _ = case patch of
        Nothing -> Just Nothing -- Even if we let a Nothing through when the element doesn't already exist, this doesn't cause a problem because it is ignored
        Just _ -> Nothing -- We don't want to let spurious re-creations of items through
  listHoldWithKey initialVals (attachWith (flip (Map.differenceWith relevantPatch)) (current sentVals) valsChanged) $ \k v ->
    mkChild k v $ select childValChangedSelector $ Const2 k

--TODO: Something better than Dynamic t (Map k v) - we want something where the Events carry diffs, not the whole value
-- | Create a dynamically-changing set of Event-valued widgets.
--   This is like listWithKey, specialized for widgets returning (Event t a).  listWithKey would return 'Dynamic t (Map k (Event t a))' in this scenario, but listViewWithKey flattens this to 'Event t (Map k a)' via 'switch'.
listViewWithKey :: (Ord k, MonadAdjust t m, PostBuild t m, MonadHold t m, MonadFix m) => Dynamic t (Map k v) -> (k -> Dynamic t v -> m (Event t a)) -> m (Event t (Map k a))
listViewWithKey vals mkChild = switch . fmap mergeMap <$> listViewWithKey' vals mkChild

listViewWithKey' :: (Ord k, MonadAdjust t m, PostBuild t m, MonadHold t m, MonadFix m) => Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Behavior t (Map k a))
listViewWithKey' vals mkChild = current <$> listWithKey vals mkChild

-- | Create a dynamically-changing set of widgets, one of which is selected at any time.
selectViewListWithKey :: forall t m k v a. (MonadAdjust t m, Ord k, PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t k          -- ^ Current selection key
  -> Dynamic t (Map k v)  -- ^ Dynamic key/value map
  -> (k -> Dynamic t v -> Dynamic t Bool -> m (Event t a)) -- ^ Function to create a widget for a given key from Dynamic value and Dynamic Bool indicating if this widget is currently selected
  -> m (Event t (k, a))        -- ^ Event that fires when any child's return Event fires.  Contains key of an arbitrary firing widget.
selectViewListWithKey selection vals mkChild = do
  let selectionDemux = demux selection -- For good performance, this value must be shared across all children
  selectChild <- listWithKey vals $ \k v -> do
    let selected = demuxed selectionDemux k
    selectSelf <- mkChild k v selected
    return $ fmap ((,) k) selectSelf
  return $ switchPromptlyDyn $ leftmost . Map.elems <$> selectChild

selectViewListWithKey_ :: forall t m k v a. (MonadAdjust t m, Ord k, PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t k          -- ^ Current selection key
  -> Dynamic t (Map k v)  -- ^ Dynamic key/value map
  -> (k -> Dynamic t v -> Dynamic t Bool -> m (Event t a)) -- ^ Function to create a widget for a given key from Dynamic value and Dynamic Bool indicating if this widget is currently selected
  -> m (Event t k)        -- ^ Event that fires when any child's return Event fires.  Contains key of an arbitrary firing widget.
selectViewListWithKey_ selection vals mkChild = fmap fst <$> selectViewListWithKey selection vals mkChild

-- | Create a dynamically-changing set of widgets from a Dynamic key/value map.
--   Unlike the 'withKey' variants, the child widgets are insensitive to which key they're associated with.
list :: (Ord k, MonadAdjust t m, MonadHold t m, PostBuild t m, MonadFix m) => Dynamic t (Map k v) -> (Dynamic t v -> m a) -> m (Dynamic t (Map k a))
list dm mkChild = listWithKey dm (\_ dv -> mkChild dv)

-- | Create a dynamically-changing set of widgets from a Dynamic list.
simpleList :: (MonadAdjust t m, MonadHold t m, PostBuild t m, MonadFix m) => Dynamic t [v] -> (Dynamic t v -> m a) -> m (Dynamic t [a])
simpleList xs mkChild = fmap (fmap (map snd . Map.toList)) $ flip list mkChild $ fmap (Map.fromList . zip [(1::Int)..]) xs
