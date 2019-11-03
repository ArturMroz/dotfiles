set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab
let mapleader=" "

imap jk <Esc>

vnoremap <leader>y "+y
nnoremap <leader>p "+p

nnoremap <leader><tab> mc80A <esc>080lDgelD`cP
nnoremap <leader>s :w<CR>
nnoremap <leader>a ggVG
" nnoremap <leader>R :source ~/.config/nvim/init.vim<CR>
nnoremap <leader>vs :so $MYVIMRC<CR>
nnoremap <leader>ve :e $MYVIMRC<CR>
nnoremap <CR> o<Esc>
nnoremap <leader>n :nohl<CR>
nnoremap <M-k> ddP
nnoremap <M-j> ddp
map <C-n> :NERDTreeToggle<CR>
map <C-p> :Files<CR>
map <leader>h :History<CR>

set number
set relativenumber

if exists('$TMUX')
    " Colors in tmux
    let &t_8f = "<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "<Esc>[48;2;%lu;%lu;%lum"
endif

set termguicolors
" set background=dark


colorscheme nord
let g:nord_italic = 1
let g:nord_italic_comments = 1
" let g:nord_comment_brightness = 1

hi Normal guibg=NONE ctermbg=NONE
hi LineNr guibg=NONE ctermbg=NONE
hi CursorLineNr guibg=dark ctermbg=NONE
hi SignColumn guibg=NONE

let g:lightline = {
      \ 'colorscheme': 'nord',
      \ }
let s:hidden_all = 0

function! ToggleHiddenAll()
    if s:hidden_all  == 0
        let s:hidden_all = 1
        set noshowmode
        set noruler
        set laststatus=0
        set noshowcmd
    else
        let s:hidden_all = 0
        set showmode
        set ruler
        set laststatus=2
        set showcmd
    endif
endfunction

" nnoremap <S-h> :call ToggleHiddenAll()<CR>

call plug#begin('~/.local/share/nvim/plugged')

Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'

Plug 'easymotion/vim-easymotion'
Plug 'justinmk/vim-sneak'
Plug 'unblevable/quick-scope'

Plug 'itchyny/lightline.vim'
Plug 'scrooloose/nerdtree'
Plug 'airblade/vim-gitgutter'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'takac/vim-hardtime'

" Plug 'vim-airline/vim-airline-themes'
" Plug 'vim-airline/vim-airline'
" Plug 'junegunn/fzf'
"
call plug#end()

let g:hardtime_default_on = 1
