Shlurp
======

(the sound the windows make as they snap into place)

A basic, stacking X11 window manager, written in the One True Language.



Why would I use this?
---------------------

- you want to be a DWM[1]/xmonad[2] hipster but you just can't handle
  tiling-window managers no matter how hard you try

- you crave that simple, solid, one-colour window border that refuses to
  admit buttons, labels or decorations of any description

- you won't be satisfied unless you're configuring your wm by editing its
  source and recompiling it

- you want to switch between the two most recently focused windows with a
  single key: the mod key. (a feature i've always wanted and never
  encountered in the wild)

- you have a high tolerance for sub-par haskell code

[1] https://dwm.suckless.org/
[2] https://xmonad.org/



How to use?
-----------

(note: currently it needs xlib bindings with some additions which are not
in upstream yet; ie: you can't build it.)

If you're brave:

    stack install

then in your `.xinitrc`:

    exec xshlurp

and then explore the source to discover and edit the key bindings :)
