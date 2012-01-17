import Control.Applicative ((<$>)) -- , liftA2)
import Control.Monad (liftM2, (>=>))
import Control.Arrow ((&&&),first)
import qualified Data.Map as Map
import System.IO
import Data.Ratio
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.WindowNavigation
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
import XMonad.Util.XSelection
-- import XMonad.Hooks.FadeWindows
import XMonad.Layout.TabBarDecoration

main = do
    xmonad myConfig where

    myConfig = myBaseConfig `additionalKeysP` myKeys `additionalMouseBindings` myMousekeys

    myBaseConfig = defaultConfig {
        modMask = mod4Mask,
        normalBorderColor = inactiveColor $ theme $ myTheme,
        focusedBorderColor = activeColor $ theme $ myTheme,
        layoutHook = myLayoutHook,
        terminal = "urxvtc"
--         logHook = fadeWindowsLogHook myFadeHook,
--         handleEventHook = fadeWindowsEventHook
        } 

--     myFadeHook = composeAll [
--         isUnfocused --> transparency 0.2,
--         opaque ]

    myKeys = [
        ("M-<Up>", windows W.swapUp),
        ("M-b", spawn "chrome"),
        ("M-c", kill),
        ("M-e", spawn "urxvtc"),
        ("M-g", goToSelected defaultGSConfig),
        ("M-j", windows W.focusUp),
        ("M-k", windows W.focusDown),
--         ("M-g", windowPromptGoto  defaultXPConfig),
        ("M-s", sshPrompt myXPConfig)
        ]

    myMousekeys = [
        ((mod4Mask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)),
        ((mod4Mask, button4), (\_ -> windows W.focusUp )),
        ((mod4Mask, button5), (\_ -> windows W.focusDown))
        ]

--     myLayoutHook = gaps [(U,g), (D,g), (R,g), (L,g)] layoutHookNoGaps where
--         g = 5
--     
--     layoutHookNoGaps = layoutHintsToCenter (Full) ||| tiled ||| magnify Grid

    myTabbedLayout = tabbedAlways shrinkText (theme myTheme)
    myMagnifyLayout = magnifiercz (12%10) Grid

    myGaps = let g = 7 in gaps [(U,0), (D,g), (R,g), (L,g)]

    myLayoutHook = (myGaps myTabbedLayout) ||| Full ||| tiled where
        tiled = Tall 1 (3/100) (1/2)

--     myLayoutHook = simpleTabBar $ layoutHook defaultConfig

--     myLayoutHook = layoutHintsToCenter (Full) ||| tiled ||| magnify Grid where
--         tiled = Tall nmaster delta ratio
--         nmaster = 1
--         delta = 3/100
--         ratio = 1/2
--         magnify = magnifiercz (12%10)

    myTheme :: ThemeInfo
    myTheme =
        TI { themeName        = "Ierton's orange"
           , themeAuthor      = "Sergey Mironov"
           , themeDescription = "Fire (orange + red) colours"
           , theme            = defaultTheme { activeColor         = "#660000"
                                             , activeBorderColor   = "#660000"
                                             , activeTextColor     = "white"
                                             , inactiveColor       = "#ff8000"
                                             , inactiveBorderColor = "#ff8000"
                                             , inactiveTextColor   = "black"
                                             }
           }

    myXPConfig = defaultXPConfig { promptKeymap = myXPKeymap } 
    myXPKeymap = defaultXPKeymap `Map.union` myXPKeys where
        myXPKeys = Map.fromList $ map (first $ (,) mod1Mask) -- alt + <key>
                   [ (xK_b, moveWord Prev)
                   , (xK_f, moveWord Next)
                   , (xK_d, killWord Next)
                   ] ++
                   map (first $ (,) controlMask) -- control + <key>
                   [ (xK_p, moveHistory W.focusDown')
                   , (xK_n, moveHistory W.focusUp')
                   , (xK_y, getSelection >>= setInput )
                   ]

