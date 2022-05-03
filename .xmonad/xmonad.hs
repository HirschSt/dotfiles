import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Layout.ResizableTile
import System.IO
import Data.List
import XMonad.Layout.Spacing
import XMonad.Actions.GridSelect
import XMonad.Layout.ShowWName
import XMonad.Layout.IndependentScreens
import XMonad.Layout.NoBorders
import XMonad.Util.WorkspaceCompare
import XMonad.Layout.ThreeColumns
import XMonad.Util.NamedScratchpad

-------------------------------------
-- CONFIG
------------------------------------
myConf = defaultConfig {
  manageHook = manageDocks <+> manageHook defaultConfig
    , workspaces = myWorkspaces
}

-------------------------------------
-- LAYOUT
-------------------------------------
myLayout = smartSpacing 8 $ ThreeColMid 1 (3/100) (1/2) ||| ResizableTall 1 (3/100) (1/2) [] ||| noBorders Full
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

-------------------------------------
-- LOGHOOK
-------------------------------------
myLogHook h    = dynamicLogWithPP $ wsPP{ ppOutput = hPutStrLn h }

-------------------------------------
-- XMOBAR
-------------------------------------
myWsBar        = "xmobar -x 1"
wsPP = xmobarPP { 
    ppOrder               = \(ws:l:t:_)   -> [ws]
    , ppCurrent = xmobarColor "#f8f8f8" "DodgerBlue4" . wrap " " " "
    , ppVisible = xmobarColor "#f8f8f8" "#A9A9A9" . wrap " " " "
    , ppUrgent  = xmobarColor "#f8f8f8" "red4" . wrap " " " " . xmobarStrip
    , ppLayout  = wrap "" "" . xmobarColor "DarkOrange" "" . wrap " [" "] "
    , ppHidden  = (xmobarColor "grey" "" . wrap "•" "")
    , ppHiddenNoWindows = (xmobarColor "#696969" "" )
    , ppTitle   = xmobarColor "#61ce3c" "" . shorten 50
    , ppWsSep               = "   |   "
    , ppSep                 = " "
    , ppSort =  getSortByTag
}
  where
    currentWsIndex w        = case (elemIndex w myWorkspaces) of
      Nothing         -> "1"
      Just n          -> show (n+1)
    

-------------------------------------
-- COLOR
-------------------------------------
gsconfig2 colorizer = (buildDefaultGSConfig colorizer) { gs_cellheight = 90, gs_cellwidth = 160  }
greenColorizer = colorRangeFromClassName
    black            -- lowest inactive bg
    (0x70,0xFF,0x70) -- highest inactive bg
    (0x44,0xAA,0xCC) -- active bg
    white            -- inactive fg
    white            -- active fg
  where 
    black = minBound
    white = maxBound

-------------------------------------
-- WORKSPACES
-------------------------------------
xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x    = [x]
myWorkspaces :: [String]
myWorkspaces = clickable . (map xmobarEscape) $ ["Terminal ","Browser","Banking","Mail","Scan","Utils","Monitoring","Audio/Video","RISM"]
  where
    clickable l = [ "<action=xdotool key alt+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
      (i,ws) <- zip [1..9] l,                                        
      let n = i ]

-------------------------------------
-- KEYS
-------------------------------------
myKeys =
  [ ((mod1Mask .|. shiftMask, xK_x), spawn "xscreensaver-command -lock")
    , ((mod1Mask .|. shiftMask, xK_b), spawn "google-chrome")
    , ((mod1Mask .|. shiftMask, xK_f), spawn "nemo")
--  , ((mod1Mask .|. shiftMask, xK_d), spawn "gnome-calculator")
    , ((mod1Mask, xK_a), sendMessage MirrorShrink)
    , ((mod1Mask, xK_z), sendMessage MirrorExpand)
    , ((mod1Mask, xK_b     ), sendMessage ToggleStruts)
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
    , ((0, xK_Print), spawn "scrot")
    , ((mod1Mask, xK_g), goToSelected  $ gsconfig2 greenColorizer)
    , ((mod1Mask .|. shiftMask, xK_t), namedScratchpadAction scratchpads "term")
    , ((mod1Mask .|. shiftMask, xK_p), namedScratchpadAction scratchpads "pavucontrol")
    , ((mod1Mask .|. shiftMask, xK_m), namedScratchpadAction scratchpads "scrcpy")
    , ((mod1Mask .|. shiftMask, xK_n), namedScratchpadAction scratchpads "nemo")
    , ((mod1Mask .|. shiftMask, xK_d), namedScratchpadAction scratchpads "calc")
  ]
  ++
  [
  -- workspaces are distinct by screen
    ((m .|. mod1Mask, k), windows $ onCurrentScreen f i)
    | (i, k) <- zip (workspaces' myConf) [xK_0 .. xK_9]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]
  ++
  [
    -- swap screen order
    ((m .|. mod1Mask, key), screenWorkspace sc >>= flip whenJust
     (windows . f))
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0,1,2]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]

-------------------------------------
-- MANAGEHOOK 
-------------------------------------
myManageHook =  composeAll
  [ className =? "MPlayer"        --> doFloat
  , className =? "Gnome-calculator"           --> doFloat
  , className =? "Evince"           --> doFloat
  ]

-------------------------------------
-- SCRATCHPADS
-------------------------------------
scratchpads :: [NamedScratchpad]
scratchpads = [
-- run htop in xterm, find it by title, use default floating window placement

    NS "term" "urxvtc -name scratchpad" (resource =? "scratchpad")
        (customFloating $ W.RationalRect (1/4) (1/4) (2/4) (2/4)),

    NS "calc" "gnome-calculator" (className =? "Gnome-calculator")
        (customFloating $ W.RationalRect (1/4) (1/4) (2/4) (2/4)),

    NS "scrcpy" "scrcpy" (className =? "scrcpy") defaultFloating,
    
    NS "nemo" "nemo" (className =? "Nemo") 
        (customFloating $ W.RationalRect  (2/6) (2/6) (2/6) (3/6)),

    NS "pavucontrol" "pavucontrol" (className =? "Pavucontrol")
        (customFloating $ W.RationalRect  (1/6) (1/6) (2/3) (2/3))
  ]

-------------------------------------
-- MAIN
-------------------------------------
main = do 
  wsBar <- spawnPipe myWsBar
  xmonad $ myConf{
    workspaces = myWorkspaces
      , modMask = mod1Mask
      , terminal = "urxvt"
      , borderWidth        = 2
      , normalBorderColor  = "black"
      , focusedBorderColor = "#6795bf"
      , layoutHook  = avoidStruts $ showWName myLayout
      , manageHook = myManageHook <+> namedScratchpadManageHook scratchpads 
      , handleEventHook    = handleEventHook defaultConfig <+> docksEventHook
      , logHook     = myLogHook wsBar
  } `additionalKeys` myKeys


