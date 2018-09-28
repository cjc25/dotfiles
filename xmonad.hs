import Graphics.X11.Types
import XMonad hiding (workspaces)
import qualified XMonad as X (workspaces)
import XMonad.Config.Gnome
import XMonad.Hooks.ManageDocks
import XMonad.Layout
import XMonad.Layout.Fullscreen
import XMonad.Layout.IndependentScreens
import XMonad.Layout.PerWorkspace
import XMonad.Util.EZConfig
import XMonad.Util.Run

import qualified Data.List as L
import XMonad.StackSet hiding (filter)

move2ScreensTo :: (Eq sid, Num sid) => VirtualWorkspace -> StackSet PhysicalWorkspace l a sid sd -> StackSet PhysicalWorkspace l a sid sd
move2ScreensTo i s
    -- Check if we're already on workspace i on the current screen
    | i == currentTag s = s
    -- We're not on it. Let's work our way through all workspaces and find the
    -- two we want, abusing that we know that screen ids are integers.
    -- TODO handle numbers of screens other than 2!
    | (Just s0ws, Just s1ws, Just s0, Just s1)
    <- ( L.find (((marshall 0 i)==) . tag) (workspaces s)
       , L.find (((marshall 1 i)==) . tag) (workspaces s)
       , L.find ((0==) . screen) (screens s)
       , L.find ((1==) . screen) (screens s)
       )
    -- TODO keep the current screen current
    = s { current = s0 { workspace = s0ws }
        , visible = s1 { workspace = s1ws } : []
        , hidden = [ ws | ws <- workspaces s, tag ws `notElem` map tag [s0ws, s1ws] ]
        }
    | otherwise = s  -- i is not in the StackSet.

-- Move 1 screen is just the default view.
moveNScreensTo n
    | n <= 1 = onCurrentScreen view
    | otherwise = move2ScreensTo

layouts nScreens = onWorkspaces vimWorkspaces vimOrder $
                   onWorkspaces vertWorkspaces vertOrder $
                   genOrder
    where
      allWorkspaces  = withScreens nScreens myWorkspaces
      -- The first half the workspaces on screen 0 should start with vim splits.
      vimWorkspaces  = take ((length myWorkspaces) `div` 2) [ ws | ws <- allWorkspaces, (unmarshallS ws) == 0 ]
      -- All workspaces on screens >0 should assume vertical splits.
      vertWorkspaces = [ ws | ws <- allWorkspaces, (unmarshallS ws) > 0 ]
      vimOrder       = vim ||| Mirror vim ||| Full
      genOrder       = even ||| Mirror even ||| Full
      vertOrder      = Mirror even ||| even ||| Full
      vim  = Tall nmaster delta vimratio
      even = Tall nmaster delta evenratio
      nmaster = 1
      delta = 1/100
      evenratio = 1/2
      -- 54% of 2560 pixels with my vim setup is just a little more than 2
      -- side-by-side 80-char splits.
      vimratio = 54/100

main = do
    nScreens <- countScreens
    xmonad $ docks gnomeConfig
     { modMask = mod4Mask
     , handleEventHook = fullscreenEventHook
     , manageHook = myManageHook <+> manageHook gnomeConfig <+> fullscreenManageHook
     , layoutHook = fullscreenFull $ avoidStruts $ layouts nScreens
     , terminal = "x-terminal-emulator"
     , X.workspaces = withScreens nScreens myWorkspaces
     } `additionalKeysP` (myKeys nScreens) `additionalKeys` shortcutKeys

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
myKeys nScreens = [ (otherModMasks ++ "M-" ++ [key], windows $ action tag)
                    | (tag, key) <- zip myWorkspaces "123456789"
                    , (otherModMasks, action) <- [ ("", moveNScreensTo nScreens)
                                                 , ("S-", onCurrentScreen shift)
                                                 , ("C-", onCurrentScreen view)
                                                 ]
                  ]

shortcutKeys = [ ( (mod1Mask .|. controlMask, xK_l)
                 , safeSpawn "gnome-screensaver-command" ["--lock"]
               ) ]

myManageHook = composeAll
        [ className =? "feh" --> doFloat
        -- The magic name for hangouts windows, maybe?
        , (className =? "Google-chrome" <||> className =? "Google-chrome-stable" <||> className =? "google-chrome" <||> className =? "google-chrome-stable") <&&> appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd" --> doFloat
        -- Security Key SSH prompt should float.
        , className =? "gnubby_ssh_prompt" --> doFloat
        ]

