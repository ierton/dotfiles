import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import System.IO
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Actions.FlexibleResize as Flex
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.Magnifier
import XMonad.Layout.LayoutHints
import Data.Ratio

main = do
    xmonad $ config `additionalKeysP` keys `additionalMouseBindings` mousekeys where

    config = defaultConfig {
        modMask = mod4Mask,
        layoutHook = layoutHook
        }

    keys = [
        ("M-<Up>", windows W.swapUp),
        ("M-b", spawn "chrome"),
        ("M-e", spawn "urxvtc"),
        ("M-c", kill)
        ]

    mousekeys = [
        ((mod4Mask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)),
        ((mod4Mask, button4), (\_ -> windows W.focusUp )),
        ((mod4Mask, button5), (\_ -> windows W.focusDown))
        ]

    layoutHook = layoutHintsToCenter (Full) ||| tiled ||| magnify Grid
        where
          tiled = Tall nmaster delta ratio
          nmaster = 1
          delta = 3/100
          ratio = 1/2
          magnify = magnifiercz (12%10)

