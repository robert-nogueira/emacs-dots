#!/bin/bash

USER_HOME=$(eval echo ~$SUDO_USER)

declare -A FILES=(
    ["zsh/.zshrc"]="$USER_HOME/.zshrc"
    ["zsh/.zshrc_aliases"]="$USER_HOME/.zshrc_aliases"
    [".gitconfig"]="$USER_HOME/.gitconfig"
    [".config/hypr"]="$USER_HOME/.config/hypr"
    [".config/neofetch"]="$USER_HOME/.config/neofetch"
)

echo "CREATING SYMBOLIC LINKS"
for key in "${!FILES[@]}";
    do
        value="${FILES[$key]}"
        echo -e "Creating symbolic link from \033[4m$key\033[0m to \033[4m$value\033[0m"
        if [ -f "$value" ] || [ -d "$value" ]; then
            echo "The file or directory already exists. Overwriting..."
            rm -rf "$value"
        fi
        echo ln -s $(readlink -f "$key") $value
        ln -s $(readlink -f "$key") $value
    done


if [ ! -d "$USER_HOME/.pyenv" ]; then
    echo "INSTALL PYENV"
    curl https://pyenv.run | bash
    export PYENV_ROOT="$USER_HOME/.pyenv"
    [[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
else
    echo "Pyenv already instaled, skiping..."
fi

echo "INSTALL PACMAN PACKAGES"
sudo pacman -S github-cli

echo "INSTALL PIPX..."
sudo pacman -S python-pipx
pipx ensurepath
pipx install poetry
