import XMonad
import XMonad.Config.Gnome
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS 

main = xmonad $ gnomeConfig {
          terminal          = "x-terminal-emulator"
        , layoutHook        = (fullscreenFloat . fullscreenFull) $ smartBorders $ layoutHook gnomeConfig
        , handleEventHook   = handleEventHook gnomeConfig <+> fullscreenEventHook
        , manageHook        = fullscreenManageHook <+> manageHook gnomeConfig
        , focusFollowsMouse = False
        }
    `additionalKeys`
        [ ((mod1Mask, xK_f), spawn "chromium")
        , ((mod1Mask, xK_v), spawn "gvim")
        , ((mod1Mask, xK_p), spawn "kupfer")
        , ((mod1Mask, xK_z), spawn "x-terminal-emulator")
        , ((mod1Mask, xK_m), spawn "x-terminal-emulator -e mutt")
        , ((mod1Mask, xK_e), prevScreen)
        , ((mod1Mask, xK_w), nextScreen)
        , ((mod1Mask .|. shiftMask, xK_e), shiftNextScreen)
        , ((mod1Mask .|. shiftMask, xK_w), shiftPrevScreen)
        ]
