# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/home/artur/.oh-my-zsh"

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
# ZSH_THEME="awesomepanda"
ZSH_THEME="dracula"

DEFAULT_USER="artur"

# Set list of themes to load
# Setting this variable when ZSH_THEME=random
# cause zsh load theme from this variable instead of
# looking in ~/.oh-my-zsh/themes/
# An empty array have no effect
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=14

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder
#

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.

plugins=(
  git
  extract
  # trash
  z
  debian
  zsh-autosuggestions
  zsh-syntax-highlighting
)

# USER CONFIGURATION

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"


export MSBuildSDKsPath=/usr/share/dotnet/sdk/$(dotnet --version)/Sdks/
# export MSBuildSDKsPath=usr/share/dotnet/sdk/2.2.203/Sdks
export PATH=$MSBuildSDKsPath:$PATH

export PATH="$HOME/.cargo/bin:$PATH"
export PATH=$PATH:/opt/node/bin
export PATH=$PATH:/opt/elm
export PATH=$PATH:/opt/firefox
export PATH=$PATH:/opt/mssql-tools/bin/
export PATH=$PATH:~/.dotnet/tools
export PATH=$PATH:~/scripts
export DOTNET_CLI_TELEMETRY_OPTOUT=1

# turn the bell off
xset b off

# caps lock behaving as ctrl
setxkbmap -option ctrl:nocaps

# wal -R -e -q  # -n -v

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.

alias zshconfig="vim ~/.zshrc"
alias srcz="source ~/.zshrc"
alias ohmyzsh="vim ~/.oh-my-zsh"
# alias upd="sudo apt update && sudo apt upgrade" 

alias praca="firefox & skypeforlinux & teamviewer & spotify & code &"
alias perform="sudo cpufreq-set -r -g performance"

alias c="xclip -selection c"
alias v="xclip -selection c -o"

alias mrl='mogrify -rotate -90' 
alias mrr='mogrify -rotate 90' 
alias mrs='mogrify -resize 1000 -sharpen 0x1' 

alias fd=fdfind

alias vim=~/Downloads/appimages/nvim.appimage

# export EDITOR=/usr/bin/vim
export EDITOR=~/Downloads/appimages/nvim.appimage

export LC_ALL="en_GB.UTF-8"
source $ZSH/oh-my-zsh.sh


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
# export FZF_DEFAULT_COMMAND='fdfind --type f --hidden --exclude .git'
export FZF_DEFAULT_COMMAND='fdfind --type f'
export FZF_TMUX=1


_fzf_compgen_path() {
  fd --hidden --follow --exclude ".git" . "$1"
}

# Use fd to generate the list for directory completion
_fzf_compgen_dir() {
  fd --type d --hidden --follow --exclude ".git" . "$1"
}
 # Use fd and fzf to get the args to a command.
 # Examples:
 # f mv # To move files. You can write the destination after selecting the files.
 # f 'echo Selected:'
 # f 'echo Selected music:' --extention mp3
 # fm rm # To rm files in current directory
 f() {
  sels=( "${(@f)$(fd "${fd_default[@]}" "${@:2}"| fzf)}" )
  test -n "$sels" && print -z -- "$1 ${sels[@]:q:q}"
}

unalias z
z() {
  if [[ -z "$*" ]]; then
    cd "$(_z -l 2>&1 | fzf +s --tac | sed 's/^[0-9,.]* *//')"
  else
    _last_z_args="$@"
    _z "$@"
  fi
}

zz() {
  cd "$(_z -l 2>&1 | sed 's/^[0-9,.]* *//' | fzf -q "$_last_z_args")"
}

alias j=z
alias jj=zz

fortune -a
