PATH=$PATH:~/.cabal/bin:~/.xmonad/bin

xsetroot -cursor_name left_ptr &
xrdb -merge ~/.Xdefaults &

tray &
x-terminal-emulator &
chromium &
xscreensaver -no-splash &
pulseaudio --start &

exec ck-launch-session dbus-launch --sh-syntax --exit-with-session xmonad
