#! /bin/bash

function unlink_symlinks {
	unlink ~/.vimrc
	unlink ~/.zshrc
	unlink ~/.tmux.conf
}

if [ "$1" == "first" ]; then
    echo "This is the first run"
    if [[ "$OSTYPE" == "linux-gnu"*  ]]; then
	sudo add-apt-repository ppa:neovim-ppa/unstable
        sudo apt install -yqq zsh curl git tmux neovim bat nvm
        git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
        ~/.fzf/install
	curl -LO https://github.com/BurntSushi/ripgrep/releases/download/11.0.2/ripgrep_11.0.2_amd64.deb
	sudo dpkg -i ripgrep_11.0.2_amd64.deb
        sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
         git clone https://github.com/pyenv/pyenv.git ~/.pyenv
    elif [[ "$OSTYPE" == "darwin19.0"  ]]; then
        brew install -yqq fzf tmux zsh curl git neovim bat nvm
        # To install useful key bindings and fuzzy completion:
        $(brew --prefix)/opt/fzf/install
        sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
    else
        echo "Not sure what type of OS we're on here"
        exit 1
    fi
else
    echo "This is not the first run, unlinking symlinks..."
    unlink_symlinks
fi

echo "Creating symlinks..."
function create_symlinks {
	ln -s $(pwd)/vim/vimrc ~/.vimrc
	ln -s $(pwd)/vim/basic.vim ~/basic.vim
	ln -s $(pwd)/vim/filetypes.vim ~/filetypes.vim
	ln -s $(pwd)/vim/extended.vim ~/extended.vim
	ln -s $(pwd)/vim/python.vim ~/python.vim
	ln -s $(pwd)/zsh/zshrc ~/.zshrc
	ln -s $(pwd)/tmux/conf ~/.tmux.conf
	    # . ~/.vimrc
	    . ~/.zshrc
	    # . ~/.tmux.conf
}

create_symlinks

echo 'export PYENV_ROOT="$HOME/.pyenv"' >> ~/.zshrc
echo 'export PATH="$PYENV_ROOT/bin:$PATH"' >> ~/.zshrc

echo "Success!"
echo "Please create/edit .zshrc.aliases to add aliases."
echo "Please create/edit .zshrc.loc to add any other local zsh config."
echo "Don't forget to download and set nvm version + pyenv version"
echo "https://github.com/deoplete-plugins/deoplete-jedi/wiki/Setting-up-Python-for-Neovim"
exit 0
