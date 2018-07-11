import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
	xmproc <- spawnPipe "xmobar /home/oliver/.xmonad/xmobar.hs"
	xmonad defaultConfig
		{ modMask 	= mod4Mask
        , terminal 	= "urxvt"
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
        	{ ppOutput = hPutStrLn xmproc
        	, ppTitle = xmobarColor "black" "" . shorten 50
        	}
        }
