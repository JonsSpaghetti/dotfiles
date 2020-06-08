#! /bin/bash

function unlink_symlinks {
	unlink ~/.vimrc
	unlink ~/.zshrc
	unlink ~/.tmux.conf
}

if [ "$1" == "first" ]; then
    echo "This is the first run"
    if [[ "$OSTYPE" == "linux-gnu"*  ]]; then
        sudo apt install zsh curl git
        sudo apt install tmux
        sudo apt install neovim bat
        git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
        ~/.fzf/install
        sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
    elif [[ "$OSTYPE" == "darwin19.0"  ]]; then
        brew install zsh curl git
        brew install fzf tmux
        brew install neovim bat
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
	ln -s $(pwd)/zsh/zshrc ~/.zshrc
	ln -s $(pwd)/tmux/conf ~/.tmux.conf
	    # . ~/.vimrc
	    . ~/.zshrc
	    # . ~/.tmux.conf
}

create_symlinks

echo "Success!"
echo "Please create/edit .zshrc.aliases to add aliases."
echo "Please create/edit .zshrc.loc to add any other local zsh config."

exit 0
