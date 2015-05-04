{-# OPTIONS_GHC -fno-warn-missing-signatures -fglasgow-exts -fno-warn-orphans #-}

import XMonad hiding (keys, config, (|||))
import qualified XMonad (keys)
import XMonad.Config ( defaultConfig )

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed ( tabbed, defaultTheme,
                              shrinkText, Shrinker, shrinkIt, CustomShrink(CustomShrink) )
import XMonad.Layout.Combo ( combineTwo )
import XMonad.Layout.Named ( named )
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Simplest ( Simplest(Simplest) )
import XMonad.Layout.Square ( Square(Square) )
-- import XMonad.Layout.WindowNavigation ( Navigate(Move,Swap,Go), Direction(U,D,R,L),
                                        -- windowNavigation )
import XMonad.Layout.NoBorders ( smartBorders )
import XMonad.Layout.WorkspaceDir ( changeDir, workspaceDir )
import XMonad.Layout.ToggleLayouts ( toggleLayouts, ToggleLayout(ToggleLayout) )
import XMonad.Layout.ShowWName ( showWName )
-- import XMonad.Layout.ScratchWorkspace ( toggleScratchWorkspace )
import XMonad.Layout.Maximize 
import XMonad.Layout.MosaicAlt
import XMonad.Layout.Grid
import XMonad.Prompt ( defaultXPConfig, font, height, XPConfig )
import XMonad.Prompt.Layout ( layoutPrompt )
import XMonad.Prompt.Shell ( shellPrompt )
import XMonad.Prompt.Window
import XMonad.Prompt.Ssh
import XMonad.Actions.FloatKeys 
import XMonad.Actions.NoBorders
import XMonad.Config.Gnome


import XMonad.Actions.CopyWindow ( kill1, copy )
import XMonad.Actions.DynamicWorkspaces ( withNthWorkspace, withWorkspace,
                                          selectWorkspace, renameWorkspace, removeWorkspace )
import XMonad.Actions.CycleWS ( moveTo, 
                                nextWS, prevWS, nextScreen, prevScreen, shiftNextScreen, shiftPrevScreen, swapNextScreen )

import XMonad.Hooks.ManageDocks ( avoidStruts, manageDocks )
-- import XMonad.Hooks.EwmhDesktops ( ewmhDesktopsLogHook,
--                                   ewmhDesktopsLayout )
import XMonad.Layout.WindowNavigation
--import XMonad.ManageHook

modMask' :: KeyMask
modMask' = mod4Mask

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig {font="-*-lucida-medium-r-*-*-14-*-*-*-*-*-*-*"
                             ,height=22}

myLayouts = windowNavigation tall ||| windowNavigation (Mirror tall) ||| Full
    where tall = Tall 1 (3/100) (1/2) 
    
newkeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
newkeys x = M.fromList $
    -- launching and killing programs
    [ ((modMask x .|. shiftMask, xK_c     ), kill1) -- %! Close the focused window

    , ((modMask x,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask x .|. shiftMask, xK_space ), setLayout $ layoutHook x) -- %!  Reset the layouts on the current workspace to default

    -- move focus up or down the window stack
    , ((modMask x,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window , ((modMask x,               xK_j     ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask x,               xK_k     ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask x .|. shiftMask, xK_u     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask x .|. shiftMask, xK_i     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- floating layer support
    , ((modMask x,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- quit, or restart
    , ((modMask x .|. shiftMask, xK_Escape), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modMask x              , xK_Escape), restart "xmonad" True) -- %! Restart xmonad

    , ((modMask x, xK_Right), nextWS)
    , ((modMask x, xK_Left), prevWS)
    -- Move float windows commands
--    , ((modMask x .|. controlMask, xK_Right), withFocused (keysMoveWindow (15,0)))
--    , ((modMask x .|. controlMask, xK_Left), withFocused (keysMoveWindow (-15,0)))
    , ((modMask x .|. controlMask, xK_Right), nextScreen)
    , ((modMask x .|. controlMask, xK_Left), prevScreen)
    , ((modMask x .|. controlMask, xK_z), swapNextScreen)
    , ((modMask x .|. controlMask, xK_z), swapNextScreen)
    , ((modMask x .|. controlMask, xK_x), shiftPrevScreen)
    , ((modMask x .|. controlMask, xK_c), shiftNextScreen)

    , ((modMask x .|. controlMask, xK_Up), withFocused (keysMoveWindow (0,-15)))
    , ((modMask x .|. controlMask, xK_Down), withFocused (keysMoveWindow (0,15)))
    , ((modMask x .|. shiftMask, xK_Right), withFocused (keysMoveWindow (30,0)))
    , ((modMask x .|. shiftMask, xK_Left), withFocused (keysMoveWindow (-30,0))) , ((modMask x .|. shiftMask, xK_Up), withFocused (keysMoveWindow (0,-30))) , ((modMask x .|. shiftMask, xK_Down), withFocused (keysMoveWindow (0,30))) 
    -- Remove Border from window 
    , ((modMask x,  xK_g ),   withFocused toggleBorder) 
    , ((0, xK_F1  ), spawn "urxvt") 
    , ((0, xK_F2  ), spawn "dmenu_run") 
    , ((modMask x .|. shiftMask, xK_x     ), changeDir myXPConfig)
    , ((modMask x .|. shiftMask, xK_k     ), spawn "gnome-screensaver-command -l") 
    , ((modMask x .|. shiftMask, xK_BackSpace), removeWorkspace)
    , ((modMask x .|. shiftMask, xK_v     ), selectWorkspace myXPConfig)
    , ((modMask x, xK_m     ),  windowPromptGoto myXPConfig)
    , ((modMask x, xK_n     ),  windowPromptBring myXPConfig)
    , ((modMask x .|. shiftMask, xK_s     ),  sshPrompt myXPConfig)
    , ((modMask x .|. shiftMask, xK_m     ), withWorkspace myXPConfig (windows . copy))
    , ((modMask x .|. shiftMask, xK_r), renameWorkspace myXPConfig)
    , ((modMask x, xK_l ), layoutPrompt myXPConfig)
    , ((modMask x .|. controlMask, xK_space), sendMessage ToggleLayout)
    , ((modMask x,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask x,               xK_l     ), sendMessage Expand) -- %! Expand the master area
{-    , ((modMask x .|. controlMask .|. shiftMask, xK_space),
       toggleScratchWorkspace (Simplest */* Simplest) )
-}
    -- Window Navigation
    , ((mod1Mask,  xK_Right), sendMessage $ Go R)
    , ((mod1Mask,  xK_Left ), sendMessage $ Go L)
    , ((mod1Mask,  xK_Up   ), sendMessage $ Go U)
    , ((mod1Mask,  xK_Down ), sendMessage $ Go D)
    , ((modMask x .|. mod1Mask, xK_Right), sendMessage $ Swap R)
    , ((modMask x .|. mod1Mask, xK_Left ), sendMessage $ Swap L)
    , ((modMask x .|. mod1Mask, xK_Up   ), sendMessage $ Swap U)
    , ((modMask x .|. mod1Mask, xK_Down ), sendMessage $ Swap D)

    ]
 
    ++
    zip (zip (repeat $ modMask x) [xK_F1..xK_F12]) (map (withNthWorkspace W.view) [0..])
    ++
    zip (zip (repeat (modMask x .|. shiftMask)) [xK_F1..xK_F12]) (map (withNthWorkspace copy) [0..])

main = xmonad $ gnomeConfig 
    { borderWidth = 1 
    , workspaces = ["1:mutt","2:web","3:dev","4:evince","5:img","6:video","7:tmp","8","9","0","-","="]
    , focusedBorderColor = "#00ff00"
    , normalBorderColor = "#000000"
    , terminal = "urxvt"
    , XMonad.keys = newkeys
    , layoutHook = myLayouts
    , XMonad.manageHook = Main.manageHook
    , modMask = modMask'
    } 

manageHook :: ManageHook
manageHook = composeAll
                [ className =? "MPlayer"        --> doFloat
                , className =? "Gimp"           --> doShift "5:img" 
                , className =? "Gimp"           --> doFloat
                , className =? "Pidgin"           --> doFloat]




