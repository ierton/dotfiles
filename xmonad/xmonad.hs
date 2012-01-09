import System.IO
import Data.Ratio
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import XMonad.Actions.FlexibleResize as Flex
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.Magnifier
import XMonad.Layout.LayoutHints
import XMonad.Layout.Tabbed
import XMonad.Layout.Gaps
import XMonad.Actions.GridSelect
import XMonad.Prompt
import XMonad.Prompt.Ssh
import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Util.Themes
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
-- import XMonad.Hooks.FadeWindows
import XMonad.Layout.TabBarDecoration

main = do
    xmonad $ config `additionalKeysP` keys `additionalMouseBindings` mousekeys where

    config = defaultConfig {
        modMask = mod4Mask,
        normalBorderColor = activeColor $ theme $ myTheme,
        focusedBorderColor = inactiveColor $ theme $ myTheme,
        layoutHook = myLayoutHook,
        terminal = "urxvtc"
--         logHook = fadeWindowsLogHook myFadeHook,
--         handleEventHook = fadeWindowsEventHook
        }

--     myFadeHook = composeAll [
--         isUnfocused --> transparency 0.2,
--         opaque ]

    keys = [
        ("M-<Up>", windows W.swapUp),
        ("M-b", spawn "chrome"),
        ("M-c", kill),
        ("M-e", spawn "urxvtc"),
        ("M-g", goToSelected defaultGSConfig),
--         ("M-g", windowPromptGoto  defaultXPConfig),
        ("M-s", sshPrompt defaultXPConfig)
        ]

    mousekeys = [
        ((mod4Mask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)),
        ((mod4Mask, button4), (\_ -> windows W.focusUp )),
        ((mod4Mask, button5), (\_ -> windows W.focusDown))
        ]

    myTheme = kavonFireTheme

--     myLayoutHook = gaps [(U,g), (D,g), (R,g), (L,g)] layoutHookNoGaps where
--         g = 5
--     
--     layoutHookNoGaps = layoutHintsToCenter (Full) ||| tiled ||| magnify Grid

    myTabbedLayout = tabbedAlways shrinkText (theme myTheme)
    myMagnifyLayout = magnifiercz (12%10) Grid

    myLayoutHook = myTabbedLayout ||| myMagnifyLayout

--     myLayoutHook = simpleTabBar $ layoutHook defaultConfig

--     myLayoutHook = layoutHintsToCenter (Full) ||| tiled ||| magnify Grid where
--         tiled = Tall nmaster delta ratio
--         nmaster = 1
--         delta = 3/100
--         ratio = 1/2
--         magnify = magnifiercz (12%10)

