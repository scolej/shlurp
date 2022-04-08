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

close windows

launch progs

keybind for lower to bottom

===> USABLE WM!!



bugs
----

dead windows accumulating in focus history

switching focus with kbd can leave stale state:
clicking in unfocused win does nothing
clicks should focus



technical
---------

log as hex

reject configurerequests when drag active

dedupe moves

pull out modkey which is scattered everywhere

key binds

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

toggle border (might be a must for firefox context menus)

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
