#!/bin/bash

xrdb -merge .Xresources
xrdb -load .Xdefaults
xset r rate 200 30

trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 5 --transparent true --tint 0x191970 --height 12 &

xsetroot -cursor_name left_ptr -solid midnightblue

xscreensaver -no-splash &

offlineimap &

chromium &

xmonad
