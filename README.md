# HebaruSan's desktop environment

## Wallpaper

I downloaded 49 space-themed wallpapers and used [mk_bgxml](mk_bgxml) to generate an XML file that cycles through them, 30 minutes per file. GNOME can use such an XML file as wallpaper.

## Application launcher

I use [synapse](https://launchpad.net/synapse-project) to launch applications, configured to open in response to the Pause key (the one key I wasn't already using for something else).

## GNOME panel

- Applications menu
- Places menu
- My custom launchers
- Virtual desktop switcher (5x2)
- Window list (current desktop only)
- (Blank space)
- Notification area
- System monitor: CPU, RAM, network
- Output of [net_connections](net_connections) script
- Local temperature
- Date/time
- User menu

## GIMP

I set up GIMP to [dock all of its tool dialogs within a single window](https://askubuntu.com/questions/136992/how-to-make-gimp-run-as-a-single-window), so I can treat it like a normal program.

## Window manager

XMonad is a tiling window manager construction kit in Haskell.
I've used it since 2011-ish because it allows me to make just about everything work the way I want it to (and because programming in Haskell is fun). Specifically:

- Used with GNOME Panel
- Focus follows mouse and mouse follows focus
- No window decoration
- Mouse wheel bindings for workspace operations
- Supports "multimedia" keyboard keys
- "Maximized" Youtube or movie player windows go into a normal tile
- Automatically change layouts based on the number of windows
- Support "hinting" so terminals and GVim can use integer multiples of their font sizes, centered within their allotted spaces

I try to use XMonad solely for window management and input handling, with session management, utility panes, wallpaper, and so on handled by other tools.

I use the standard XMonad keys plus a few custom ones:

| Key                    | Effect |
| ---                    | ---    |
| Super-Up arrow         | Move horizontal splitter of active pane up |
| Super-Down arrow       | Move horizontal splitter of active pane down |
| Super-Minus            | Jump to next empty workspace |
| Super-Shift-Minus      | Move active window to next empty workspace and jump to it |
| Play, pause, etc.      | Control Rhythmbox |
| Super-Mousewheel       | Jump to non-empty workspaces |
| Super-Shift-Mousewheel | Move active window to empty workspace |
| Alt-Mute               | Mute/unmute speaker channels independently of headphones |

I tend to mix and match windows depending on what a given task requires, so I use the same layout for all workspaces rather than routing certain programs to certain special purpose workspaces as in many other xmonad configs.

I don't use a messaging application.

### Dependencies

Install the package containing `XMonad.Actions.Volume`:

    cabal install xmonad-extras

### Login session

1. Copy `xmonad-gnome.desktop` to `/usr/share/xsessions`
2. Copy `xmonad-compton.session` to `/usr/share/gnome-session/sessions`
3. Copy `compton.desktop` to `~/.local/share/applications`

### Known issues

- Vertical mouse offset in some full screen games with visible gnome-panel (have to aim below buttons to click them)
- Have to switch away/back to a workspace for tiled maximized Youtube windows to adopt the correct dimensions

### Future enhancements

- Preserve sizes by window instead of position (i.e., let my terminals stay small)
- Add new windows to side with fewest current windows
- Shrink all slaves in same pane when adding rather than only splitting next
- Focus previous when closing, not next (inverse of insertPosition)
- Include hint-based gaps as draggable handles
- Hotkey to set vertical splitter to 50%

## Other utilities

### rerun

Analyzes your `bash` history and presents a menu to allow you to re-run your most commonly run commands.

### pdfman

Opens a man page in PDF format.
