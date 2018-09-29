{-|
Module      : Reflex.GLFW.Instances
Description : A GLFW-b adapter for the Haskell Reflex FRP implementation.  Rogue instances, import at your peril.
Copyright   : (c) Serge Kosyrev, 2017
License     : BSD-3
Maintainer  : _deepfire@feelingofgreen.ru (temporarily defunct)
Stability   : experimental
Portability : Unspecified

-}
{-# OPTIONS_GHC -Wextra -Wno-orphans #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}
module Reflex.GLFW.Instances
  (
    mappendModifierKeys
  , GL.ModifierKeys
  )
  where

import                    Prelude.Unicode
import qualified "GLFW-b" Graphics.UI.GLFW          as GL

instance Semigroup GL.ModifierKeys where
  (<>)   = mappendModifierKeys

instance Monoid GL.ModifierKeys where
  mempty = GL.ModifierKeys False False False False

mappendModifierKeys ∷ GL.ModifierKeys → GL.ModifierKeys → GL.ModifierKeys
mappendModifierKeys (GL.ModifierKeys a b c d) (GL.ModifierKeys a' b' c' d') =
  GL.ModifierKeys (a ∨ a') (b ∨ b') (c ∨ c') (d ∨ d')
