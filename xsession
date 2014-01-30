PATH=$PATH:~/.cabal/bin:~/.xmonad/bin

if [ -s ~/.Xmodmap ]; then
    xmodmap ~/.Xmodmap &
fi

xsetroot -cursor_name left_ptr &
xrdb -merge ~/.Xdefaults &
xrandr --output VGA1 --auto --pos 0x0 --output LVDS1 --auto --right-of VGA1 &

tray &
nm-applet &
google-musicmanager &
x-terminal-emulator &
chromium &
xscreensaver -no-splash &
pulseaudio --start &

exec ck-launch-session dbus-launch --sh-syntax --exit-with-session xmonad
