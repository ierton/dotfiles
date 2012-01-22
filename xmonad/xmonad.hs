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
import XMonad.Actions.FocusNth
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.Magnifier
import XMonad.Layout.LayoutHints
import XMonad.Layout.Tabbed
import XMonad.Layout.Gaps
import XMonad.Layout.LayoutModifier
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.DecorationAddons
import XMonad.Layout.ButtonDecoration
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.Monitor
import XMonad.Actions.GridSelect
import XMonad.Prompt
import XMonad.Prompt.Ssh
import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Util.Themes
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.XSelection
import XMonad.Layout.ShowWName
import XMonad.Hooks.FadeInactive

main = do
    spawn "cairo-clock"
    xmonad myConfig

    where

    myConfig = myBaseConfig `additionalKeysP` myKeys `additionalMouseBindings` myMousekeys

    myBaseConfig = defaultConfig {
        modMask = mod4Mask,
        normalBorderColor = "white",
        focusedBorderColor = activeColor $ theme $ myTheme,
        layoutHook = showWName myLayoutHook,
        manageHook = myManageHook,
        logHook = myLogHook,
        terminal = "urxvtc"
        } 

    myKeys = [
        ("M-<Up>", windows W.swapUp),
        ("M-b", spawn "chrome"),
        ("M-c", kill),
        ("M-<Return>", spawn "urxvtc"),
        ("M-g", goToSelected defaultGSConfig),
        ("M-j", windows W.focusUp),
        ("M-k", windows W.focusDown),
        ("M-s", sshPrompt myXPConfig),
        ("M-C-t", withFocused $ windows . W.sink),
        ("M-u", broadcastMessage ToggleMonitor >> refresh)
        ] ++ [
        ("M-"++[j], focusNth i) | (i,j) <- [0..4]`zip`"qwert"
        ]

    myMousekeys = [
        ((mod4Mask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)),
        ((mod4Mask, button4), (\_ -> windows W.focusUp )),
        ((mod4Mask, button5), (\_ -> windows W.focusDown))
        ]

--     myLayoutHook = gaps [(U,g), (D,g), (R,g), (L,g)] layoutHookNoGaps where
--         layoutHookNoGaps = layoutHintsToCenter (Full) ||| tiled ||| magnify Grid
--         g = 5

    myTabbedLayout = tabbedAlways shrinkText (theme myTheme)

    myMagnifyLayout = magnifiercz (12%10) Grid

    myGaps = let g = 7 in gaps [(U,0), (D,g), (R,g), (L,g)] myTabbedLayout

    myLayoutHook = ModifiedLayout myClockMonitor layouts where 
        layouts = myGaps ||| Full ||| tiled ||| myMagnifyLayout
        tiled = Tall 1 (3/100) (1/2)

--     myLayoutHook = imageButtonDeco shrinkText defaultThemeWithImageButtons (layoutHook defaultConfig)

--     myLayoutHook = buttonDeco shrinkText defaultThemeWithButtons (layoutHook defaultConfig)
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

    myManageHook = manageMonitor myClockMonitor

    myClockMonitor = monitor {
         -- Cairo-clock creates 2 windows with the same classname, thus also using title
         prop = ClassName "Cairo-clock" `And` Title "MacSlow's Cairo-Clock"
       , rect = Rectangle (1024-100) (600 - 100) 100 100
       , persistent = True
       , opacity = 0.6
       , visible = True
       , name = "clock"
       }

    myLogHook = fadeInactiveLogHook fadeAmount
        where fadeAmount = 0.8

