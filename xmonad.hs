import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig
import XMonad.Util.Run

import Control.Exception
import System.Process

myFont = "-*-terminesspowerline-medium-*-*-*-16-*-*-*-*-*-*-*"
myStatusBar = "dzen2 -dock -ta l -e 'onstart=lower' -x 0 -y 0 -h 24 -w 2200 -fn '" ++ myFont ++ "'"
myClock = "(set -e ; while true ; do date +\"%a %Y-%m-%d %I:%M%p %Z\" ; sleep 10 ; done) | dzen2 -dock -ta r -e 'onstart=lower' -x 2200 -y 0 -h 24 -w 360 -fn '" ++ myFont ++ "'"

myTerminal = "x-terminal-emulator"

myModMask = mod4Mask

myKeys =
  [ ((myModMask .|. mod1Mask, xK_Return), safeSpawn "google-chrome" [])
  , ((mod1Mask .|. controlMask, xK_l), safeSpawn "xscreensaver-command" ["-lock"])
  ]

myPrettyPrinter handle = def
  { ppOutput = hPutStrLn handle
  }

myManageHook = composeAll
  [ className =? "feh" --> doFloat
  -- The magic name for hangouts windows, maybe?
  , className =? "Google-chrome" <&&> appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd" --> doFloat
  -- Security Key SSH prompt should float.
  , className =? "gnubby_ssh_prompt" --> doFloat
  ]

myConfig statusHandle = desktopConfig
  { terminal = myTerminal
  , modMask = myModMask
  , logHook = dynamicLogWithPP $ myPrettyPrinter statusHandle
  , manageHook = myManageHook <+> manageHook desktopConfig
  } `additionalKeys` myKeys

main = do
  -- Try to kill any leftover trays before we start. We don't care if it fails.
  i <- try $ callProcess "killall" ["dzen2"] :: IO (Either SomeException ())
  statusHandle <- spawnPipe myStatusBar
  spawn myClock
  xmonad $ myConfig statusHandle
