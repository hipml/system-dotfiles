# Sets some defaults for OS X
#
# Shamelessly stolen from GitHub user @holman's dotfiles
#
# Run ./set-defaults.sh

# Show ~Library folder
chflags nohidden ~/Library

# Always open in Finder's list view
defaults write com.apple.Finder FXPreferredViewStyle Nlsv

# Set the Finder prefs for showing volumes on the Desktop
defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true
defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool true
