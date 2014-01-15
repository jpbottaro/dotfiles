PATH=$PATH:~/.cabal/bin:~/.xmonad/bin

if [ -s ~/.xmodmap ]; then
    xmodmap ~/.xmodmap
fi

xsetroot -cursor_name left_ptr &
xrdb -merge ~/.Xdefaults &

tray &
nm-applet &
kupfer --no-splash &
x-terminal-emulator &
chromium &
xscreensaver -no-splash &
pulseaudio --start &

exec ck-launch-session dbus-launch --sh-syntax --exit-with-session xmonad
