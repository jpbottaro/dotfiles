PATH=$PATH:~/.cabal/bin:~/.xmonad/bin

xsetroot -cursor_name left_ptr
xrdb -merge ~/.Xresources
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
# BsAs
#gtk-redshift -l -34.64:-58.45 &
# Dublin
gtk-redshift -l 53.33:-6.24 &
xscreensaver -no-splash &

exec ck-launch-session dbus-launch --sh-syntax --exit-with-session xmonad
