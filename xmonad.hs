{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PatternGuards #-}

import qualified Control.Monad.RWS as Data.Semigroup.Internal
import qualified GHC.IO.Handle as GHC.IO.Handle.Types
import qualified Graphics.X11 as Graphics.X11.Types
import System.IO (hPutStrLn)
import XMonad
  ( Default (def),
    Full (Full),
    XConfig
      ( focusedBorderColor,
        handleEventHook,
        layoutHook,
        logHook,
        manageHook,
        modMask,
        normalBorderColor,
        startupHook,
        terminal
      ),
    className,
    doFloat,
    mod4Mask,
    sendMessage,
    spawn,
    withFocused,
    xmonad,
    (-->),
    (<+>),
    (=?), (|||)
  )
import XMonad.Actions.CycleWS
  ( Direction1D (Next, Prev),
    emptyWS,
    nextScreen,
    nextWS,
    prevScreen,
    prevWS,
    shiftNextScreen,
    shiftPrevScreen,
    shiftTo,
    shiftToNext,
    shiftToPrev,
    toggleWS,
  )
import XMonad.Actions.WorkspaceNames
  ( renameWorkspace,
    workspaceNamesPP,
  )
import qualified XMonad.Core (Query, WindowSet)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks
  ( avoidStruts,
    docks,
    manageDocks,
  )
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Hooks.Place (placeHook, smart, withGaps)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.StatusBar.PP
  ( PP (ppCurrent, ppOutput, ppSep, ppTitle),
    dynamicLogWithPP,
    shorten,
    wrap,
    xmobarColor,
    xmobarPP,
  )
import XMonad.Layout.BoringWindows (boringWindows, focusDown, focusUp)
import XMonad.Layout.Decoration (Theme (activeColor, activeTextColor, decoHeight, fontName, inactiveColor, inactiveTextColor), shrinkText)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Simplest (Simplest (Simplest))
import XMonad.Layout.SubLayouts (GroupMsg (MergeAll, UnMerge), onGroup, pullGroup, subLayout)
import XMonad.Layout.Tabbed (addTabs)
import XMonad.Layout.ToggleLayouts (toggleLayouts)
import XMonad.Layout.TrackFloating
import XMonad.Layout.WindowNavigation (windowNavigation)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Types (Direction2D (D, L, R, U))
import XMonad.Layout.SimplestFloat

{-
 -fix "xrandr: Configure crtc 1 failed" when reconnecting external work monitors after sleep
  -> watch for xorg issue and see if it fixed yet in latest
 - make a systemd trigger for the monitor plug-in/out -> make it fail (Xorg and random cable unplug) safe
 todo:
 - experiment with sessions
 - find out how to connect to a new network easily with netctl
 - try st
 - also icons to xmobar
 - figure out what the heck happens with fish colors inside urxvt
-}
mmask :: Graphics.X11.Types.KeyMask
mmask = mod4Mask

manageZenityHook ::
  XMonad.Core.Query
    (Data.Semigroup.Internal.Endo XMonad.Core.WindowSet)
manageZenityHook = className =? "Zenity" --> doFloat

xmobar :: IO GHC.IO.Handle.Types.Handle
xmobar = spawnPipe "xmobar /home/oliver/.xmonad/xmobar.hs"

tabConfig :: Theme
tabConfig =
  def
    { fontName = "xft:Terminus",
      decoHeight = 20,
      activeColor = "#818380",
      inactiveColor = "#333333",
      activeTextColor = "#000000",
      inactiveTextColor = "#bbbbbb"
    }

subTabbed' x = addTabs shrinkText tabConfig $ subLayout [] Simplest x

layout =
  trackFloating
    $ useTransientFor $
      windowNavigation $
        (subTabbed' $
          boringWindows $
            avoidStruts $
              toggleLayouts (noBorders Full) $
                smartBorders $
                  layoutHook def) ||| simplestFloat

main :: IO ()
main = do
  xmproc <- xmobar
  xmonad $
    ewmhFullscreen $
      ewmh $
        docks $
          def
            { modMask = mmask,
              normalBorderColor = "#000000",
              focusedBorderColor = "#ffb6b0",
              terminal = "alacritty",
              startupHook = setWMName "LG3D",
              manageHook =
                manageZenityHook
                  <+> placeHook (withGaps (16, 0, 16, 0) (smart (0.5, 0.5)))
                  <+> manageDocks
                  <+> (isFullscreen --> doFullFloat)
                  <+> manageHook def,
              layoutHook = layout,
              handleEventHook = handleEventHook def,
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
            `additionalKeysP` [ ("M-p", spawn "rofi -show drun"),
                                -- ("M-S-p", spawnAndDo (liftX (withFocused (sendMessage $ pullGroup L))) "rofi -show drun"),
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
                                ("M-S-C-[", spawn "env fish -c fix_mouse"),
                                ("M-S-r", renameWorkspace def),
                                -- mpd
                                ("M-<XF86AudioPlay>", spawn "mpc toggle"),
                                ("M-<XF86AudioPrev>", spawn "mpc prev"),
                                ("M-<XF86AudioNext>", spawn "mpc next"),
                                ("M-<XF86AudioLowerVolume>", spawn "mpc volume -2"),
                                ("M-<XF86AudioRaiseVolume>", spawn "mpc volume +2"),
                                ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 3%-"),
                                ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 3%+"),
                                ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10"),
                                ("<XF86MonBrightnessUp>",   spawn "xbacklight -inc 10"),
                                -- ("M-<F10>", spawn ""),
                                -- ("M-<F11>", spawn "")
                                ("M-C-h", sendMessage $ pullGroup L),
                                ("M-C-l", sendMessage $ pullGroup R),
                                ("M-C-k", sendMessage $ pullGroup U),
                                ("M-C-j", sendMessage $ pullGroup D),
                                ("M-C-m", withFocused (sendMessage . MergeAll)),
                                ("M-C-u", withFocused (sendMessage . UnMerge)),
                                ("M-<Tab>", onGroup W.focusDown'),
                                ("M-S-<Tab>", onGroup W.focusUp'),
                                ("M-j", focusDown),
                                ("M-k", focusUp)
                              ]
