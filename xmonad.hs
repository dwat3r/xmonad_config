{-# OPTIONS_GHC -Wno-deprecations #-}
import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.WorkspaceNames
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ToggleLayouts
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import Data.Maybe (maybeToList)
import Control.Monad ((>=>), join, liftM, when)
import XMonad.Layout.Tabbed (simpleTabbed)

{-
todo:
 -fix "xrandr: Configure crtc 1 failed" when reconnecting external work monitors after sleep
  -> watch for xorg issue and see if it fixed yet in latest
 - make a systemd trigger for the monitor plug-in/out -> make it fail (Xorg and random cable unplug) safe
 - experiment with sessions
 - find out how to connect to a new network easily with netctl
 - try st
 - also icons to xmobar
 - figure out what the heck happens with fish colors inside urxvt
-}
mmask = mod4Mask

manageZoomHook =
  composeAll
    [ (className =? zoomClassName) <&&> shouldFloat <$> title --> doFloat,
      (className =? zoomClassName) <&&> shouldSink <$> title --> doSink
    ]
  where
    zoomClassName = "zoom"
    tileTitles =
      [ "Zoom - Free Account", -- main window,
        "Zoom - Licensed Account", -- main window,
        "Zoom", -- meeting window on creation,
        "Zoom Meeting" -- meeting window shortly after creation
      ]
    shouldFloat title = title `notElem` tileTitles
    shouldSink title = title `elem` tileTitles
    doSink = (ask >>= doF . W.sink) <+> doF W.swapDown

manageZenityHook = className =? "Zenity" --> doFloat

addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- join . maybeToList <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

main = do
  xmproc <- spawnPipe "xmobar /home/oliver/.xmonad/xmobar.hs"
  xmonad $
    def
      { modMask = mmask,
        normalBorderColor = "#7c7c7c",
        focusedBorderColor = "#ffb6b0",
        terminal = "alacritty",
        startupHook = setWMName "LG3D" >> addEWMHFullscreen,
        manageHook =
          manageZoomHook
            <+> manageZenityHook
            <+> placeHook (withGaps (16, 0, 16, 0) (smart (0.5, 0.5)))
            <+> manageDocks
            <+> (isFullscreen --> doFullFloat)
            <+> manageHook def,
        layoutHook =
          avoidStruts $
            toggleLayouts (noBorders Full) $
              smartBorders $
        simpleTabbed |||
                layoutHook def ||| simplestFloat,
        handleEventHook = fullscreenEventHook <+> dynamicTitle manageZoomHook <+> docksEventHook <+> handleEventHook def,
        logHook =
          workspaceNamesPP
            xmobarPP
              { ppOutput = hPutStrLn xmproc,
                ppTitle = xmobarColor "grey" "black" . shorten 200,
                ppCurrent = xmobarColor "#CEFFAC" "black" . wrap "<" ">",
                ppSep = "   "
              }
            >>= dynamicLogWithPP
      }
      `additionalKeysP` [ ("M-p", spawn "yegonesh"),
                          ("M-]", spawn "slock"),
                          ("M-<D>", nextWS),
                          ("M-<U>", prevWS),
                          ("M-S-<D>", shiftToNext),
                          ("M-S-<U>", shiftToPrev),
                          ("M-<R>", nextScreen),
                          ("M-<L>", prevScreen),
                          ("M-S-<R>", shiftNextScreen),
                          ("M-S-<L>", shiftPrevScreen),
                          ("M-z", toggleWS),
                          ("M-C-<D>", shiftTo Next emptyWS),
                          ("M-C-<U>", shiftTo Prev emptyWS),
                          ("M-C-c", spawn "/home/oliver/scripts/keylayoutchanger.sh"),
                          ("M-S-C-]", spawn "env fish -c monitor_switcher"),
                          ("M-S-r", renameWorkspace def),
                          ("M-S-h", spawn "sh -c 'systemctl hibernate"),
                          -- mpd
                          ("M-<XF86AudioPlay>", spawn "mpc toggle"),
                          ("M-<XF86AudioPrev>", spawn "mpc prev"),
                          ("M-<XF86AudioNext>", spawn "mpc next"),
                          ("M-<XF86AudioLowerVolume>", spawn "mpc volume -2"),
                          ("M-<XF86AudioRaiseVolume>", spawn "mpc volume +2"),
                          ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 3%-"),
                          ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 3%+")
                          -- ("<XF86MonBrightnessDown>", spawn ""),
                          -- ("<XF86MonBrightnessUp>",   spawn ""),
                          -- ("M-<F10>", spawn ""),
                          -- ("M-<F11>", spawn "")
                        ]
