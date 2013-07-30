-- originally based on http://www.haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen%27s_Configuration

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Util.EZConfig(additionalKeys)

-- so we can toggle fullscreen
import XMonad.Layout.MultiToggle

import XMonad.Layout.NoBorders

import XMonad.Layout.ToggleLayouts

import XMonad.Layout.WindowNavigation
-- import XMonad.Actions.Navigation2D -- switch to this once new version of XMonad's available

-- so "Mirror Tall" gets displayed as "Wide"
import XMonad.Layout.Renamed

import XMonad.Layout.PerWorkspace

import qualified XMonad.StackSet as W
import Control.Monad (liftM2)

main = xmonad =<< statusBar "xmobar" myPP toggleStrutsKey myConfig

normalBorderCol, focusedBorderCol, currentCol, layoutCol :: String
currentCol       = "#09f"
layoutCol        = "#f90"
normalBorderCol  = "#000"
focusedBorderCol = "#0f5" -- and current window's title in status bar

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
        [ className =? "VirtualBox" --> viewShift "2:vbox"
        , manageDocks
        ] <+> manageHook defaultConfig
    where viewShift = doF . liftM2 (.) W.greedyView W.shift

tiledLayout = avoidStruts $
        smartBorders $
        toggleLayouts Full $
        tall ||| renamed [Replace "Wide"] (Mirror tall)
    where
        tall = windowNavigation (Tall 1 (3/100) (1/2))

fullscreenLayout = avoidStruts $
        smartBorders $
        Full

myLayoutHook = onWorkspace "2:vbox" fullscreenLayout $ tiledLayout

-- startup applications - these are run each time XMonad is (re)started.
myStartupHook = do
    spawn "xrdb -merge ${HOME}/.Xresources"
    spawn "xsetroot -cursor_name left_ptr"
    spawn "xsetroot -solid #222222"

-- main configuration
myConfig = defaultConfig
        { manageHook         = myManageHook
        , layoutHook         = myLayoutHook
        , modMask            = mod4Mask -- Rebind Mod to the Windows key
        , borderWidth        = 6
        , normalBorderColor  = normalBorderCol
        , focusedBorderColor = focusedBorderCol
        , workspaces         = ["1:main", "2:vbox", "3", "4", "5", "6", "7", "8", "9"]
        , startupHook        = myStartupHook
        } `additionalKeys`
        [ ((mod4Mask,               xK_f), sendMessage ToggleLayout)
        , ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        --, ((mod4Mask .|. shiftMask, xK_z), spawn "gnome-screensaver-command -l")
        , ((mod4Mask,               xK_Right), sendMessage $ Go R)
        , ((mod4Mask,               xK_Left ), sendMessage $ Go L)
        , ((mod4Mask,               xK_Up   ), sendMessage $ Go U)
        , ((mod4Mask,               xK_Down ), sendMessage $ Go D)
        , ((mod4Mask .|. shiftMask, xK_Right), sendMessage $ Swap R)
        , ((mod4Mask .|. shiftMask, xK_Left ), sendMessage $ Swap L)
        , ((mod4Mask .|. shiftMask, xK_Up   ), sendMessage $ Swap U)
        , ((mod4Mask .|. shiftMask, xK_Down ), sendMessage $ Swap D)
        ]

