#!/bin/bash

# Verifica se o script está sendo executado com sudo
if [[ -z "$SUDO_USER" ]]; then
    echo "Este script precisa ser executado com sudo."
    exit 1
fi

USER_HOME=$(eval echo ~$SUDO_USER)
DOTFILES_DIR="$USER_HOME/.dotfiles"

declare -A FILES=(
    ["zsh/.zshrc"]="$USER_HOME/.zshrc"
    ["zsh/.zshrc_aliases"]="$USER_HOME/.zshrc_aliases"
    [".gitconfig"]="$USER_HOME/.gitconfig"
    [".config/hypr"]="$USER_HOME/.config/hypr"
    [".config/neofetch"]="$USER_HOME/.config/neofetch"
    [".config/emacs/init.el"]="$USER_HOME/.emacs.d/init.el"
)

echo "CRIANDO LINKS SIMBÓLICOS"
for key in "${!FILES[@]}"; do
    value="${FILES[$key]}"
    source_file="$DOTFILES_DIR/$key"
    echo -e "Criando link simbólico de \033[4m$source_file\033[0m para \033[4m$value\033[0m"
    if [ -f "$value" ] || [ -d "$value" ]; then
        echo "O arquivo ou diretório já existe. Substituindo..."
        rm -rf "$value"
    fi
    # Cria o diretório de destino, se necessário
    mkdir -p "$(dirname "$value")"
    ln -s "$source_file" "$value"
done

if [ ! -d "$USER_HOME/.pyenv" ]; then
    echo "INSTALANDO PYENV"
    if command -v curl &> /dev/null; then
        curl https://pyenv.run | bash
        export PYENV_ROOT="$USER_HOME/.pyenv"
        [[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
        eval "$(pyenv init -)"
        eval "$(pyenv virtualenv-init -)"
    else
        echo "curl não está instalado. Por favor, instale curl e execute novamente."
        exit 1
    fi
else
    echo "Pyenv já instalado, pulando..."
fi

echo "INSTALANDO PACOTES DO PACMAN"
sudo pacman -S github-cli

echo "INSTALANDO PIPX..."
sudo pacman -S python-pipx
export PATH="$HOME/.local/bin:$PATH"
pipx ensurepath
pipx install poetry
