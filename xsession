#!/bin/bash

xrdb -load .Xdefaults -merge .Xresources
xset r rate 200 30

trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 5 --transparent true --tint 0x191970 --height 12 &

xsetroot -cursor_name left_ptr -solid midnightblue

xscreensaver -no-splash &

if [ -x /usr/bin/nm-applet ] ; then
	nm-applet --sm-diable &
fi

if [ -x /usr/bin/gnome-power-manager ] ; then
	gnome-power-manager &
fi

chromium &

xmonad
