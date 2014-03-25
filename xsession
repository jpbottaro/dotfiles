PATH=$PATH:~/.cabal/bin:~/.xmonad/bin

xsetroot -cursor_name left_ptr
xrdb -merge ~/.Xdefaults
xrandr --output HDMI1 --primary --auto --right-of eDP1 --output eDP1 --auto
setxkbmap -layout us -variant altgr-intl -option ctrl:nocaps
feh --bg-fill .wallpaper

if [ -s ~/.Xmodmap ]; then
    xmodmap ~/.Xmodmap &
fi

unclutter &
tray &
nm-applet &
x-terminal-emulator &
chromium &
xscreensaver -no-splash &
pulseaudio --start &

exec ck-launch-session dbus-launch --sh-syntax --exit-with-session xmonad
