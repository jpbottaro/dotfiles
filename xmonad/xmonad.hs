import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LayoutHints
--import XMonad.Layout.Fullscreen
import XMonad.Util.Run
import XMonad.Util.Run(spawnPipe)
import System.IO

import Data.Monoid
import qualified Data.Map as M
import qualified XMonad.StackSet as W

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    xmonad $ defaultConfig
        { terminal        = myTerminal
        , modMask         = myModMask
        , borderWidth     = myBorderWidth
        , layoutHook      = myLayoutHook
        , manageHook      = myManageHook
        , workspaces      = myWorkspaces
        , keys            = myKeys
        , mouseBindings   = myMouseBindings
--        , handleEventHook = myHandleEventHook
        , logHook         = myLogHook xmproc
        }

myTerminal        = "urxvt"
myModMask         = mod4Mask
myBorderWidth     = 1
myWorkspaces      = ["web", "term", "code"] ++ map show [4..9]
myLayoutHook      = layoutHints $ avoidStruts $ layoutHook defaultConfig
myManageHook      = manageHook defaultConfig <+> manageDocks
--myHandleEventHook = fullscreenEventHook

-- Pipe to xmobar
myLogHook h = dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn h
                , ppTitle  = xmobarColor "green" "" . shorten 50
                }



-- Mouse Bindings
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
        [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                           >> windows W.shiftMaster))
        , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
        , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                           >> windows W.shiftMaster))
        ]

-- Key Bindings
myKeys x       = foldr M.delete (newKeys x) (keysToRemove x)
newKeys x      = M.union (keys defaultConfig x) (M.fromList (keysToAdd x))
keysToRemove x =
    [ (modMask x, xK_c)
    ]
keysToAdd x =
    [ ((modMask x, xK_z), spawn "urxvt")
    , ((modMask x, xK_x), spawn "urxvt -e su")
    , ((modMask x, xK_p), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((modMask x, xK_v), spawn "gvim; xdotool key Super+n")
    , ((modMask x, xK_f), spawn "chromium")
    , ((modMask x, xK_d), spawn "urxvt -e mutt")
    , ((modMask x, xK_b), spawn "randombg")
    , ((0 , 0x1008ff12), spawn "amixer -q sset Master toggle"   ) --XF86AudioToggleVolume
    , ((0 , 0x1008ff11), spawn "amixer -q sset Master 5- unmute") --XF86AudioLowerVolume
    , ((0 , 0x1008ff13), spawn "amixer -q sset Master 5+ unmute") --XF86AudioRaiseVolume
    ]
