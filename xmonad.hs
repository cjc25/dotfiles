import XMonad hiding (workspaces)
import qualified XMonad as X (workspaces)
import XMonad.Config.Gnome
import XMonad.Layout.Fullscreen
import XMonad.Layout.IndependentScreens
import XMonad.Util.EZConfig

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

main = do
    nScreens <- countScreens
    xmonad $ gnomeConfig
     { modMask = mod4Mask
     , handleEventHook = fullscreenEventHook
     , manageHook = myManageHook <+> manageHook gnomeConfig <+> fullscreenManageHook
     , layoutHook = fullscreenFull $ layoutHook gnomeConfig
     , terminal = "x-terminal-emulator"
     , X.workspaces = withScreens nScreens myWorkspaces
     } `additionalKeysP` (myKeys nScreens)

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
myKeys nScreens = [ (otherModMasks ++ "M-" ++ [key], windows $ action tag)
                    | (tag, key) <- zip myWorkspaces "123456789"
                    , (otherModMasks, action) <- [ ("", moveNScreensTo nScreens)
                                                 , ("S-", onCurrentScreen shift)
                                                 , ("C-", onCurrentScreen view)
                                                 ]
                  ]

myManageHook = composeAll
        [ className =? "feh" --> doFloat
        -- The magic name for hangouts windows, maybe?
        , (className =? "Google-chrome" <||> className =? "Google-chrome-stable") <&&> appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd" --> doFloat
        ]

