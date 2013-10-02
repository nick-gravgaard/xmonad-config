-- originally based on http://www.haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen%27s_Configuration

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowNavigation
-- import XMonad.Actions.Navigation2D -- switch to this once new version of XMonad's available
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig

import Control.Monad (liftM2)

main = xmonad =<< statusBar "xmobar" myPP toggleStrutsKey (ewmh myConfig)

normalBorderCol, focusedBorderCol, currentCol, layoutCol :: String
currentCol       = "#09f"
layoutCol        = "#f90"
normalBorderCol  = "#000"
focusedBorderCol = "#fff" -- and current window's title in status bar

-- what's displayed in the status bar
myPP :: PP
myPP = defaultPP
        { ppCurrent = xmobarColor currentCol "" . wrap "[" "]"
        , ppLayout = xmobarColor layoutCol ""
        , ppTitle = xmobarColor focusedBorderCol ""
        }

-- key binding to toggle the gap for the bar
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myManageHook = composeAll
        [ className =? "VirtualBox" --> viewShift "9:maxed"
        , manageDocks
        ] <+> manageHook defaultConfig
    where viewShift = doF . liftM2 (.) W.greedyView W.shift

tiledLayout = avoidStruts $
        smartBorders $
        windowNavigation $
        renamed [Replace "RTall"] (rTall) ||| renamed [Replace "RWide"] (Mirror rTall)
    where
        rTall = ResizableTall 1 (1/16) (1/2) []

fullscreenLayout = avoidStruts $
        smartBorders $
        Full

myLayoutHook = onWorkspace "9:maxed" fullscreenLayout $ tiledLayout

-- startup applications - these are run each time XMonad is (re)started.
myStartupHook = do
        spawn "xrdb -merge ${HOME}/.Xresources"
        spawn "trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand false --widthtype percent --width 10 --heighttype pixel --height 21 --transparent true --alpha 0 --tint 0x222222"
        spawn "nm-applet --sm-disable"
        spawn "gnome-sound-applet"
        spawn "hsetroot -solid #000000"
        spawn "xscreensaver -no-splash"
        spawn "compton -b"
        spawn "syndaemon -d -t"

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9:maxed"]

keysToMoveWindowAndSwitchWorkspaces =
        [ ("M-"++m++show k, f i)
        | (i, k) <- zip myWorkspaces ([1 .. 9]++[0])
        , (f, m) <-
                [ (windows . W.greedyView, ""),
                  (\i -> windows (W.shift i) >> windows (W.greedyView i), "S-")
                ]
        ]

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
        where fadeAmount = 0.625

-- main configuration
myConfig = defaultConfig
        { manageHook         = myManageHook
        , layoutHook         = myLayoutHook
        , modMask            = mod4Mask -- Rebind Mod to the Windows key
        , borderWidth        = 3
        , normalBorderColor  = normalBorderCol
        , focusedBorderColor = focusedBorderCol
        , workspaces         = myWorkspaces
        , startupHook        = myStartupHook
        , logHook = myLogHook
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_h), sendMessage MirrorShrink)
        , ((mod4Mask .|. shiftMask, xK_l), sendMessage MirrorExpand)
        , ((mod4Mask,               xK_Right), sendMessage $ Go R)
        , ((mod4Mask,               xK_Left ), sendMessage $ Go L)
        , ((mod4Mask,               xK_Up   ), sendMessage $ Go U)
        , ((mod4Mask,               xK_Down ), sendMessage $ Go D)
        , ((mod4Mask .|. shiftMask, xK_Right), sendMessage $ Swap R)
        , ((mod4Mask .|. shiftMask, xK_Left ), sendMessage $ Swap L)
        , ((mod4Mask .|. shiftMask, xK_Up   ), sendMessage $ Swap U)
        , ((mod4Mask .|. shiftMask, xK_Down ), sendMessage $ Swap D)
        , ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        --, ((mod4Mask .|. shiftMask, xK_z), spawn "gnome-screensaver-command -l")
        ]
        `additionalKeysP`
        keysToMoveWindowAndSwitchWorkspaces

