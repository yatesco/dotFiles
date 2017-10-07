#!/bin/bash 
function install_homebrew {
	if [[ $(which brew) ]]; then
		echo "Homebrew already installed";
	else
		echo "Installing homebrew";
	/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)";
	fi	
	echo "Syncing apps"
	brew bundle
	brew bundle cleanup --force
}

function configure_email {
        echo "Creating mail directories if they don't already exist"
 	mkdir -p ~/Maildir/gmail
 	mkdir -p ~/Maildir/fastmail
 	mkdir -p ~/Maildir/work
}

function link_dot_files {
	echo "Installing ~/. files"
        pushd dotFiles > /dev/null
	for f in * 
	do
                eval "ln -sF $(pwd)/$f ~/.$f" 
	done	
        popd > /dev/null
}

function link_config_files {
	echo "Installing ~/.config files"
        pushd configFiles > /dev/null
	mkdir -p ~/.config/
	for f in * 
	do
                eval "ln -sF $(pwd)/$f ~/.config/$f" 
	done	
        popd > /dev/null
}

function checkout_emacs {
	if [ -d ~/.emacs.d ]; then
		echo ".emacs.d is already cloned"
        else
		echo "Cloning .emacs.d"
        	git clone https://github.com/yatesco/dotEmacs.git ~/.emacs.d 
	fi
}

function configure_fish {
	local LINE=`(which fish)`
	local FILE=/etc/shells
	# assume that if fish is already a valid shell then we have already installed this.
	local INSTALLED_P=`(grep "$LINE" $FILE)`
	if [[ $INSTALLED_P ]]; then
		echo "Fish already appears to be configured"
	else
		echo "Changing shell to fish"
		echo "$LINE" | sudo tee -a "$FILE"
		chsh -s `(which fish)` `(whoami)`
		echo "Installing oh-my-fish"
		curl -L https://get.oh-my.fish | fish
	fi
}

install_homebrew
configure_email
link_dot_files
link_config_files
checkout_emacs
configure_fish
