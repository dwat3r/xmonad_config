import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
  xmproc <- spawnPipe "xmobar /home/dwat3r/.xmonad/xmobar.hs"
  xmonad $ fullscreenSupport $ def
    { modMask             = mod4Mask
    , normalBorderColor   = "#7c7c7c"
    , focusedBorderColor  = "#ffb6b0"
    , terminal            = "urxvtc"
    , startupHook         = setWMName "LG3D"
    , manageHook          = manageDocks <+> manageHook def
    , layoutHook          = avoidStruts 
                          $ toggleLayouts (noBorders Full) 
                          $ smartBorders 
                          $ layoutHook def
    , handleEventHook     = handleEventHook def <+> docksEventHook
    , logHook             = dynamicLogWithPP xmobarPP
      { ppOutput            = hPutStrLn xmproc
      , ppTitle             = xmobarColor "grey" "black" . shorten 100
      , ppCurrent           = xmobarColor "#CEFFAC" "" . wrap "<" ">"
      , ppSep               = "   "
      }
    }
