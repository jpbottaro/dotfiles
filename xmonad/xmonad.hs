import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Util.Run(spawnPipe)
import System.IO

import Data.Monoid
import qualified Data.Map as M

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    xmonad $ defaultConfig
        { terminal    = myTerminal
        , modMask     = myModMask
        , borderWidth = myBorderWidth
        , layoutHook  = myLayoutHook
        , manageHook  = myManageHook
        , keys        = myKeys
        , logHook     = dynamicLogWithPP xmobarPP
                            { ppOutput = hPutStrLn xmproc
                            , ppTitle  = xmobarColor "green" "" . shorten 50
                            }

        }

myTerminal      = "xterm"
myModMask       = mod4Mask -- Super key
myBorderWidth   = 1
myLayoutHook    = avoidStruts $ layoutHook defaultConfig
myManageHook    = manageHook defaultConfig <+> manageDocks

myKeys x = foldr M.delete (newKeys x) (keysToRemove x)
newKeys x = M.union (keys defaultConfig x) (M.fromList (keysToAdd x))
keysToRemove x =
    [ (modMask x, xK_c)
    ]
keysToAdd x =
    [ ((modMask x, xK_z), spawn "xterm")
    , ((modMask x, xK_p), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((modMask x, xK_v), spawn "gvim")
    , ((modMask x, xK_f), spawn "chromium")
    , ((modMask x, xK_d), spawn "xterm -e mutt")
    ]
