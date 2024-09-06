killall waybar

echo $USER
if [[ $USER = "robert" ]]
then
    waybar -c ~/.dotfiles/waybar/config/config.jsonc & ~/.dotfiles/waybar/config/style.css
else
    waybar &
fi
