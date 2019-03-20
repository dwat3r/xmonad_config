import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import System.IO
{-
todo: 
 -fix "xrandr: Configure crtc 1 failed" when reconnecting external work monitors after sleep
  -> watch for xorg issue and see if it fixed yet in latest
 - make a systemd trigger for the monitor plug-in/out -> make it fail (Xorg and random cable unplug) safe
 - name workspaces
 - experiment with sessions
 - install battery power saving -> configure tlp service
 - make openvpns to a systemd service
 - find out how to connect to a new network easily with netctl
 - investigate mouse cursor X strangeness
 - configure mouse buttons to work in intellij too
 - configure aws keychain
 - try st
 - also icons to xmobar
 - figure out what the heck happens with fish colors inside urxvt
-}
mmask = mod4Mask

main = dom
  xmproc <- spawnPipe "xmobar /home/oliver/.xmonad/xmobar.hs"
  xmonad $ fullscreenSupport $ def
    { modMask             = mmask
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
      , ppTitle             = xmobarColor "grey" "black" . shorten 200
      , ppCurrent           = xmobarColor "#CEFFAC" "" . wrap "<" ">"
      , ppSep               = "   "
      }
    } `additionalKeysP` [
      ("M-p",     spawn "yegonesh"),
      ("M-]",     spawn "slock"),
      ("M-<D>",   nextWS),
      ("M-<U>",   prevWS),
      ("M-S-<D>", shiftToNext),
      ("M-S-<U>", shiftToPrev),
      ("M-<R>",   nextScreen),
      ("M-<L>",   prevScreen),
      ("M-S-<R>", shiftNextScreen),
      ("M-S-<L>", shiftPrevScreen),
      ("M-z",     toggleWS),
      ("M-C-<D>", shiftTo Next EmptyWS),
      ("M-C-<U>", shiftTo Prev EmptyWS),
      ("M-C-c",   spawn "/home/oliver/scripts/keylayoutchanger.sh"),
      ("M-S-C-]", spawn "/home/oliver/scripts/monitor_switcher.sh"),
      -- mpd
      ("<XF86AudioPlay>", spawn "mpc toggle"),
      ("<XF86AudioPrev>", spawn "mpc prev"),
      ("<XF86AudioNext>", spawn "mpc next"),
      ("<XF86AudioLowerVolume>", spawn "mpc volume -2"),
      ("<XF86AudioRaiseVolume>", spawn "mpc volume +2")
    ]
