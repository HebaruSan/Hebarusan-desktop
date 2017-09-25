-- Core Haskell libraries
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import Control.Monad
import Data.Ratio ((%))
import Data.Time
import qualified Data.String.Utils as Str

-- XMonad libraries
import XMonad hiding ((|||))
import XMonad.Layout.LayoutCombinators ((|||))
import qualified XMonad.StackSet as W
import XMonad.Util.Replace
import XMonad.Util.Stack
import XMonad.Config.Gnome
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.FadeWindows
import XMonad.Layout.Named
import XMonad.Layout.HintedGrid
import XMonad.Layout.NoBorders
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.BoringWindows
import XMonad.Layout.LayoutHints
import XMonad.Actions.UpdatePointer
import XMonad.Actions.CycleSelectedLayouts
import XMonad.Actions.CycleWS
import XMonad.Actions.Volume
import XMonad.Hooks.SetWMName
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Util.WorkspaceCompare
import XMonad.Actions.FloatKeys
import XMonad.Actions.NoBorders
import XMonad.Operations

main :: IO ()
main = do

    -- Forcibly replace current window manager
    replace

    -- Start with GNOME compatible base
    xmonad $ gnomeConfig {

        -- Turn off borders
        borderWidth = 0,

        -- Windows key ("Super") not used by apps
        modMask = mod4Mask,

        -- Borrow the default GNOME startup hook as a base
        startupHook = startupHook gnomeConfig

            -- Tell Java programs we don't re-parent
            <+> setWMName "LG3D",

        -- Use Debian/Ubuntu system setting for terminal
        terminal = "x-terminal-emulator",

        -- Append workspace #0 for an even ten
        workspaces = workspaces gnomeConfig ++ [show (0::Int)],

        mouseBindings = \cfg -> M.union

            -- Extend default GNOME behavior
            (mouseBindings gnomeConfig cfg)

            -- Add our own mouse bindings
            (M.fromList [

                -- Super + mouse wheel changes to prev/next non empty wkspc
                ((                modMask cfg, button4), \_w -> moveTo Prev NonEmptyWS),
                ((                modMask cfg, button5), \_w -> moveTo Next NonEmptyWS),

                -- Super + shift + mouse wheel moves window to prev/next empty wkspc
                ((shiftMask   .|. modMask cfg, button4), \_w -> shiftTo Prev EmptyWS),
                ((shiftMask   .|. modMask cfg, button5), \_w -> shiftTo Next EmptyWS),

                -- Super + ctrl + mouse wheel moves window and switches to its new wkspc
                ((controlMask .|. modMask cfg, button4), \_w -> doTo Prev EmptyWS getSortByIndex $
                    \ws -> windows (W.shift ws) >>
                        windows (W.greedyView ws)),
                ((controlMask .|. modMask cfg, button5), \_w -> doTo Next EmptyWS getSortByIndex $
                    \ws -> windows (W.shift ws) >>
                        windows (W.greedyView ws))

            ]),

        keys = \cfg -> M.union (M.fromList $ [

            -- Multimedia keys that can map to applications
            ((0, xF86XK_HomePage),         spawn "x-www-browser"),
            ((0, xF86XK_WWW),              spawn "x-www-browser"),
            ((0, xF86XK_Mail),             spawn "evolution"),
            ((0, xF86XK_Search),           spawn "gnome-search-tool"),

            -- Music playing keys to control Rhythmbox
            ((0, xF86XK_Music),            spawn "rhythmbox"),
            ((0, xF86XK_AudioPrev),        spawn "rhythmbox-client --previous"),
            ((0, xF86XK_AudioPlay),        spawn "rhythmbox-client --play-pause"),
            ((0, xF86XK_AudioPause),       spawn "rhythmbox-client --pause"),
            ((0, xF86XK_AudioNext),        spawn "rhythmbox-client --next"),
            ((0, xF86XK_Eject),            spawn "rhythmbox-client --quit"),

            -- Volume keys
            ((0,        xF86XK_AudioMute),        void toggleHeadphoneMute),
            -- Alt-mute toggles speakers independently of Master/Headphone
            ((mod1Mask, xF86XK_AudioMute),        void toggleSpeakerMute),
            ((mod3Mask, xF86XK_AudioMute),        void toggleSpeakerMute),
            ((0,        xF86XK_AudioLowerVolume), void $ lowerVolume 3),
            ((0,        xF86XK_AudioRaiseVolume), void $ raiseVolume 3),

            -- Print screen: borderless, to clipboard
            ((0,        xK_Print), spawn "gnome-screenshot -c -B"),
            -- Alt-print screen: borderless, to clipboard, active window only
            ((mod1Mask, xK_Print), spawn "gnome-screenshot -c -B -w"),
            ((mod3Mask, xK_Print), spawn "gnome-screenshot -c -B -w"),

            -- Super + minus jumps to next empty workspace, a la GNU screen
            ((              modMask cfg, xK_minus), viewEmptyWorkspace),

            -- Super + shift + minus moves window to next empty workspace
            ((shiftMask .|. modMask cfg, xK_minus), tagToEmptyWorkspace),

            -- Super + equals should set the main splitter to 50/50, but I don't think SetMasterFraction is exposed
            --((              modMask cfg, xK_equal), sendMessage (SetMasterFraction (1/2))),

            -- Super + y should float a window and set its size to 1920x1080 for Youtube recording.
            ((              modMask cfg, xK_y),     withFocused $ recordYoutube cfg),

            -- Alternate window swapping keys because I find super-shiftj and super-shift-k unintuitive
            ((              modMask cfg, xK_Left),  windows W.swapUp),
            ((              modMask cfg, xK_Right), windows W.swapDown),

            ((              modMask cfg, xK_Up),    sendMessage ShrinkSlave),
            ((              modMask cfg, xK_Down),  sendMessage ExpandSlave),

            ((              modMask cfg, xK_u),     sendMessage ShrinkSlave),
            ((              modMask cfg, xK_i),     sendMessage ExpandSlave)

        ] ++ workspaceHotkeys cfg xK_0 0
          ++ workspaceHotkeys cfg xK_1 1
          ++ workspaceHotkeys cfg xK_2 2
          ++ workspaceHotkeys cfg xK_3 3
          ++ workspaceHotkeys cfg xK_4 4
          ++ workspaceHotkeys cfg xK_5 5
          ++ workspaceHotkeys cfg xK_6 6
          ++ workspaceHotkeys cfg xK_7 7
          ++ workspaceHotkeys cfg xK_8 8
          ++ workspaceHotkeys cfg xK_9 9) (keys gnomeConfig cfg),

        manageHook =

            -- Support fullscreen
            (isFullscreen --> doFullFloat)

            -- Tile various windows that default to floating
            <+> (className =? "Chromium-browser"     --> doSink)
            <+> (className =? "steam"                --> doSink)
            <+> (resource  =? "steam"                --> doSink)
            <+> (className =? "MassEffect.exe"       --> doSink)
            <+> (className =? "Rogue Legacy"         --> doSink)
            <+> (resource  =? "Rogue Legacy"         --> doSink)
            <+> (resource  =? "DARK SOULS"           --> doSink)
            <+> (className =? "DARK SOULS"           --> doSink)

            -- Try to hide Synapse's border because it's shaped
            -- Doesn't work yet, no idea why
            -- (All floating windows have borders)
            -- <+> (resource  =? "synapse"              --> doSuppressBorder)
            -- <+> (className =? "synapse"              --> doSuppressBorder)

            -- Insert new windows after current instead of before
            <+> insertPosition Below Newer

            -- Allow space for gnome-panel
            <+> manageDocks,

        logHook = logHook gnomeConfig
            -- Switch to grid and back depending on number of windows
            <+> pickLayout [((<=), 8, ["mrt"]),
                            ((>=), 9, ["Grid"])]

            -- Warp mouse when changing focus with keyboard
            <+> updatePointer (0.5, 0.5) (0, 0)

            -- <+> dynamicLog
            ,

        handleEventHook = handleEventHook gnomeConfig
            -- Resize hint gaps when term font size changes
            <+> hintsEventHook
            ,

        -- No window decorations
        layoutHook =

            -- No red border around fullscreen Youtube videos
            -- smartBorders $
            -- noBorders $

            -- Leave space for gnome-panel
            avoidStruts $

            -- I think this is for dialog windows
            boringWindows $

            -- Padding around gvim and terminals to fit font
            layoutHintsWithPlacement (0.5, 0.5) $

            -- Make it possible to jump to this
            named "mrt" (

                -- Clicking screen edge hits scrollbar instead of border
                --noBorders $

                -- Two column tiled layout
                mouseResizableTile {

                    -- Customize gaps between windows
                    draggerType = FixedDragger {

                        -- Big enough to see and click,
                        gapWidth = 4,

                        -- Small enough to stay out of the way
                        draggerWidth = 4

            }}) ||| ThreeCol 1 (3/100) (1/2) ||| Grid False -- 3x3 for >=9 windows (names itself)

    } where

        -- Mute/unmute headphones/speakers separately.
        -- The channel names are probably system-dependent and may need updating.
        -- Needs to work around some quirks with PulseAudio, namely that muting the Headphone channel
        -- also mutes Master.
        -- Also set the speaker volumes to 100 because the default is 0.
        -- Note that it's actually impossible to mute Headphones without muting everything else.
        -- I recommend uninstalling PulseAudio in all circumstances if you possibly can.
        -- Based on http://code.haskell.org/xmonad-extras/documentation/src/XMonad-Actions-Volume.html#toggleMute
        speakerChannels     :: [String]
        speakerChannels     = ["Surround", "Center", "LFE", "Side"]
        headphoneChannels   :: [String]
        headphoneChannels   = ["Headphone", "Master", "Front"]
        toggleSpeakerMute   = modifyVolumeMuteChannels speakerChannels (\v m -> (100.0, not m))
        toggleHeadphoneMute :: MonadIO m => m Bool
        toggleHeadphoneMute = toggleMuteChannels headphoneChannels

        -- Put the current window in the middle of the screen at 1920x1080 and start recording it,
        -- then convert it to MP4 and play it back.
        -- Ctrl-alt-s stops the recording.
        recordYoutube :: XConfig Layout -> Window -> X ()
        recordYoutube cfg win = do
            centerFloatWithDimensions 1920 1080 cfg win
            curTime <- io $ getZonedTime
            let filename = (Str.replace " " "_" (formatTime defaultTimeLocale "%0Y%m%d_%H%M%S" curTime))
            let filepath = "/media/DataTrove/Movies/Youtube-" ++ filename
            spawn $ "recordmydesktop"
                ++ " --device pulse"
                ++ " --workdir /media/DataTrove/Movies"
                ++ " --windowid " ++ (show win)
                ++ " -o " ++ filepath ++ ".ogv"
                ++ " --fps 30"
                ++ ";"
                ++ "ffmpeg -i " ++ filepath ++ ".ogv"
                ++ " -c:v libx264 -preset veryslow -crf 22 -c:a libmp3lame -qscale:a 2 -ac 2 -ar 44100"
                ++ " " ++ filepath ++ ".mp4"
                ++ ";"
                ++ "rm " ++ filepath ++ ".ogv"
                ++ ";"
                ++ "mplayer " ++ filepath ++ ".mp4"

        -- Float a window to the middle of the screen, at the given width and height
        centerFloatWithDimensions :: Dimension -> Dimension -> XConfig Layout -> Window -> X ()
        centerFloatWithDimensions width height cfg win = withDisplay $ \disp -> do
            io $ moveResizeWindow disp win
                (((fromIntegral $ displayWidth  disp (defaultScreen disp)) - fromIntegral width ) `div` 2)
                (((fromIntegral $ displayHeight disp (defaultScreen disp)) - fromIntegral height) `div` 2)
                (width + 2 * borderWidth cfg) (height + 2 * borderWidth cfg)
            float win

        -- Put a floating window into the tiled layout, akin to doFloat
        doSink :: ManageHook
        doSink = ask >>= doF . W.sink

        -- Suppress the border in a manage hook
        doSuppressBorder :: ManageHook
        doSuppressBorder = ask >>= \w -> liftX (withDisplay $ \disp -> io $ setWindowBorderWidth disp w 0) >> idHook

        -- Generate a jump-to and send-window-to hotkey for a given workspace number
        workspaceHotkeys :: XConfig l -> KeySym -> Int -> [((KeyMask, KeySym), X ())]
        workspaceHotkeys cfg key num = [
            ((              modMask cfg, key), windows $ W.greedyView $ show num),
            ((shiftMask .|. modMask cfg, key), windows $ shiftBottom  $ show num)]

        -- I don't like newly shifted windows replacing the master; among other reasons,
        -- it prevents me from preserving the ordering when shifting multiple windows.
        -- This behavior doesn't seem to be exposed very easily, so we duplicate a lot
        -- of code to make it always append.
        shiftBottom :: (Ord a, Eq s, Eq i) => i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
        shiftBottom n s = maybe s (\w -> shiftWinBottom n w s) (W.peek s)

        shiftWinBottom :: (Ord a, Eq a, Eq s, Eq i) => i -> a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
        shiftWinBottom n w s = case W.findTag w s of
            Just from | n `W.tagMember` s && n /= from -> go from s
            _                                          -> s
            where go from = onWorkspace n (insertBottom w) . onWorkspace from (W.delete' w)

        onWorkspace :: (Eq i, Eq s) => i -> (W.StackSet i l a s sd -> W.StackSet i l a s sd)
                    -> W.StackSet i l a s sd -> W.StackSet i l a s sd
        onWorkspace n f s = W.view (W.currentTag s) . f . W.view n $ s

        insertBottom :: Eq a => a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
        insertBottom a s = if W.member a s then s else
            W.modify (Just $ W.Stack a [] []) (\(W.Stack t l r) -> Just $ W.Stack t l (r ++ [a])) s

        -- Automatically switch layouts based on the number of windows present
        pickLayout :: [(Integer -> Integer -> Bool, Integer, [String])] -> X ()
        pickLayout checks = do
            ws <- gets windowset
            let howMany = windowCount ws in
                forM_ checks $ \(op, val, names) ->
                    when (howMany `op` val) (cycleThroughLayouts names)

        -- Count the number of windows in a window set
        windowCount :: WindowSet -> Integer
        windowCount = foldlZ_ (\count _win -> 1+count) 0 . W.stack . W.workspace . W.current
