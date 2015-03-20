{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, GADTs, ScopedTypeVariables, FunctionalDependencies, RecursiveDo, UndecidableInstances, GeneralizedNewtypeDeriving, StandaloneDeriving, EmptyDataDecls, NoMonomorphismRestriction, TemplateHaskell, PolyKinds, TypeOperators, DeriveFunctor, LambdaCase, CPP, ForeignFunctionInterface, DeriveDataTypeable, ConstraintKinds #-}
module Reflex.Dom.Class where

import Prelude hiding (mapM, mapM_, sequence, concat)

import Reflex
import Reflex.Host.Class

import Control.Monad.Identity hiding (mapM, mapM_, forM, forM_, sequence)
import Control.Lens hiding ((<|))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable
import Data.Foldable
import Control.Monad.Ref
import Control.Monad.Reader hiding (mapM, mapM_, forM, forM_, sequence)
import Control.Monad.State hiding (mapM, mapM_, forM, forM_, sequence)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum (..))
import Data.IORef
import Data.These
import Data.Align
import GHCJS.DOM.Types hiding (Event)
import GHCJS.DOM.Document
import GHCJS.DOM.Node
import GHCJS.DOM.NamedNodeMap
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.HTMLSelectElement
import GHCJS.DOM.HTMLTextAreaElement
import GHCJS.DOM.UIEvent
import GHCJS.DOM.EventM (event, preventDefault)
import Data.Dependent.Map (DMap)
import Safe

-- | Alias for Data.Map.singleton
(=:) :: k -> a -> Map k a
(=:) = Map.singleton

keycodeEnter :: Int
keycodeEnter = 13

keycodeEscape :: Int
keycodeEscape = 27

class ( Reflex t, MonadHold t m, MonadIO m, Functor m, MonadReflexCreateTrigger t m
      , HasDocument m
      , MonadIO (WidgetHost m), Functor (WidgetHost m), MonadSample t (WidgetHost m)
      , HasPostGui t (GuiAction m) (WidgetHost m), HasPostGui t (GuiAction m) m, MonadRef m, MonadRef (WidgetHost m)
      , Ref m ~ Ref IO, Ref (WidgetHost m) ~ Ref IO --TODO: Eliminate this reliance on IO
      , MonadFix m
      ) => MonadWidget t m | m -> t where
  type WidgetHost m :: * -> *
  type GuiAction m :: * -> *
  askParent :: m Node
  subWidget :: Node -> m a -> m a
  subWidgetWithVoidActions :: Node -> m a -> m (a, Event t (WidgetHost m ()))
  liftWidgetHost :: WidgetHost m a -> m a --TODO: Is this a good idea?
  schedulePostBuild :: WidgetHost m () -> m ()
  addVoidAction :: Event t (WidgetHost m ()) -> m ()
  getRunWidget :: IsNode n => m (n -> m a -> WidgetHost m (a, WidgetHost m (), Event t (WidgetHost m ())))

class Monad m => HasDocument m where
  askDocument :: m HTMLDocument

instance HasDocument m => HasDocument (ReaderT r m) where
  askDocument = lift askDocument

instance HasDocument m => HasDocument (StateT r m) where
  askDocument = lift askDocument

class (MonadRef h, Ref h ~ Ref m, MonadRef m) => HasPostGui t h m | m -> t h where
  askPostGui :: m (h () -> IO ())
  askRunWithActions :: m ([DSum (EventTrigger t)] -> h ())

runFrameWithTriggerRef :: (HasPostGui t h m, MonadRef m, MonadIO m) => Ref m (Maybe (EventTrigger t a)) -> a -> m ()
runFrameWithTriggerRef r a = do
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  liftIO . postGui $ mapM_ (\t -> runWithActions [t :=> a]) =<< readRef r  

instance HasPostGui t h m => HasPostGui t h (ReaderT r m) where
  askPostGui = lift askPostGui
  askRunWithActions = lift askRunWithActions

instance MonadWidget t m => MonadWidget t (ReaderT r m) where
  type WidgetHost (ReaderT r m) = WidgetHost m
  type GuiAction (ReaderT r m) = GuiAction m
  askParent = lift askParent
  subWidget n w = do
    r <- ask
    lift $ subWidget n $ runReaderT w r
  subWidgetWithVoidActions n w = do
    r <- ask
    lift $ subWidgetWithVoidActions n $ runReaderT w r
  liftWidgetHost = lift . liftWidgetHost
  schedulePostBuild = lift . schedulePostBuild
  addVoidAction = lift . addVoidAction
  getRunWidget = do
    r <- ask
    runWidget <- lift getRunWidget
    return $ \rootElement w -> do
      (a, postBuild, voidActions) <- runWidget rootElement $ runReaderT w r
      return (a, postBuild, voidActions)

performEvent_ = addVoidAction

performEvent e = do
  (eResult, reResultTrigger) <- newEventWithTriggerRef
  addVoidAction $ ffor e $ \o -> do
    result <- o
    runFrameWithTriggerRef reResultTrigger result
  return eResult

performEventAsync :: forall t m a. MonadWidget t m => Event t ((a -> IO ()) -> WidgetHost m ()) -> m (Event t a)
performEventAsync e = do
  (eResult, reResultTrigger) <- newEventWithTriggerRef
  addVoidAction $ ffor e $ \o -> do
    postGui <- askPostGui
    runWithActions <- askRunWithActions
    o $ \a -> postGui $ mapM_ (\t -> runWithActions [t :=> a]) =<< readRef reResultTrigger
  return eResult

getPostBuild :: MonadWidget t m => m (Event t ())
getPostBuild = do
  (e, trigger) <- newEventWithTriggerRef
  schedulePostBuild $ runFrameWithTriggerRef trigger ()
  return e

