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

function configure_zsh {
	local LINE=`(which zsh)`
	local FILE=/etc/shells
	# assume that if zsh is already a valid shell then we have already installed this.
	local INSTALLED_P=`(grep "$LINE" $FILE)`
	if [[ $INSTALLED_P ]]; then
		echo "Zsh already appears to be configured"
	else
		echo "Changing shell to zsh"
		echo "$LINE" | sudo tee -a "$FILE"
		chsh -s `(which zsh)` `(whoami)`
		echo "Installing oh-my-zsh"
		sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
	fi
}

install_homebrew
configure_email
link_dot_files
link_config_files
configure_zsh
