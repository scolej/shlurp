TODO
====



must haves
----------

sling windows to hot spots with shortcuts
  maximize toggle
  half screen
  vert max
  horz max
  move between screens

keybind for lower to bottom

toggleable borders

===> USABLE WM!!



bugs
----

switching focus with kbd can leave stale state:
clicking in unfocused win does nothing
clicks should focus

when i spawn a term, it doesn't get focus.
what about: an action which spawns a new prog which
creates a brief window during which the next mapped window
may take focus. but at all other times newly mapped windows
may not take focus. hmmm. maybe newly mapped windows should
just get focus.

still some weirdness with resizing emacs.
if i maximize, sometimes it doesn't get quite there on first go.
emacs fighting us with configures again???



technical
---------

todo revise author

generalized bindings, mouse / keys, eg: configurable from one place

logging with levels

think about event/command interface

error logger

think about how to write a log on keypress with state info,
which is currently not trivial



and then...
-----------

preserve stack order during switching

mouse cursor change when hold mod; change based on pos in window

scroll through windows under mouse

minimize

move window colour to conf

ensure adapt to screen resize

double click expand in dir

desktops? configuration registers?

window menu / list and selection.
eg: hold mod4,
get a visual menu with list of windows,
click on them to focus/raise,
right click to minimize

multi-screen snap polish:
shouldn't be able to snap to the outside of other screens bounds

how to resize one-handed?

can i bind mod4 l & r and still use sxhkd for mod4 binds?
