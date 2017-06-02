{-# LANGUAGE TemplateHaskell #-}

module Minilisp.State
  ( _parameterName
  , HasState
  , parameterName
  , State(State)
  ) where

import Control.Lens (makeClassy)

newtype State = State
  { _parameterName :: String
  }

makeClassy ''State
