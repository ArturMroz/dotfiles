call plug#begin('~/.local/share/nvim/plugged')

" Plug 'liuchengxu/vista.vim'
" Plug 'jreybert/vimagit'

Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'

Plug 'easymotion/vim-easymotion'
Plug 'justinmk/vim-sneak'
Plug 'unblevable/quick-scope'

Plug 'takac/vim-hardtime'
Plug 'wellle/targets.vim'
Plug 'cohama/lexima.vim'
Plug 'machakann/vim-highlightedyank'

Plug 'itchyny/lightline.vim'
Plug 'scrooloose/nerdtree'

Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'neoclide/coc.nvim', {'branch': 'release'}

Plug 'HerringtonDarkholme/yats.vim'
Plug 'prurigro/vim-polyglot-darkcloud'

Plug 'ryanoasis/vim-devicons'

Plug 'arcticicestudio/nord-vim'

call plug#end()

colorscheme nord

set number relativenumber
set ignorecase smartcase

set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab

set listchars=space:·,tab:>-,trail:~,extends:>,precedes:<
" set list

set autoindent
filetype plugin indent on

set termguicolors
" set background=dark

let mapleader=" "

imap jk <Esc>

vnoremap <leader>y "+y
nnoremap <leader>p "+p

nnoremap <leader>s :w<CR>
nnoremap <leader>a ggVG

nnoremap <CR> o<Esc>
nnoremap <leader>n :nohl<CR>
nnoremap <M-k> ddP
nnoremap <M-j> ddp

nnoremap <leader>vs :so $MYVIMRC<CR>
nnoremap <leader>ve :e $MYVIMRC<CR>
nnoremap <leader>vi :PlugInstall<CR>
nnoremap <leader>vc :PlugClean<CR>

nnoremap <C-n> :NERDTreeToggle<CR>
nnoremap <leader><C-n> :NERDTreeFind<CR>

nnoremap <C-p> :Files<CR>
nnoremap <leader>ff :Files<CR>
nnoremap <leader>fh :History<CR>
nnoremap <leader>fc :Commits<CR>
nnoremap <leader>fb :Buffers<CR>
nnoremap <leader>fx :Commands<CR>
nnoremap <leader>f/ :BLines<cr> 
nnoremap <leader>/ :Ag<cr> 

if exists('$TMUX')
    " Colors in tmux
    let &t_8f = "<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "<Esc>[48;2;%lu;%lu;%lum"
endif

let g:nord_italic = 1
let g:nord_italic_comments = 1
" let g:nord_comment_brightness = 1

hi Normal guibg=NONE ctermbg=NONE
hi LineNr guibg=NONE ctermbg=NONE
hi CursorLineNr guibg=dark ctermbg=NONE
hi SignColumn guibg=NONE

let g:hardtime_default_on = 1

" COC ------------------------------

" You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=300

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
  \ pumvisible() ? "\<C-n>" :
  \ <SID>check_back_space() ? "\<TAB>" :
  \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" Or use `complete_info` if your vim support it, like:
" inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Remap for rename current word
nmap <F2> <Plug>(coc-rename)

" Remap for format selected region
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

let g:lightline = {
\ 'component': {
\   'lineinfo': ' %3l:%-2v',
\ },
\ 'component_function': {
\   'readonly': 'LightlineReadonly',
\   'fugitive': 'LightlineFugitive',
\   'filetype': 'LightLineFiletype',
\   'fileformat': 'LightLineFileformat',
\ },
\ 'colorscheme': 'nord',
\ 'separator': { 'left': '', 'right': '' },
\ 'subseparator': { 'left': '', 'right': '' }
\ }

function! LightlineReadonly()
  return &readonly ? '' : ''
endfunction

function! LightlineFugitive()
  if exists('*fugitive#head')
    let branch = fugitive#head()
    return branch !=# '' ? ''.branch : ''
  endif
  return ''
endfunction

function! LightLineFiletype()
  "return winwidth(0) > 70 ? (strlen(&filetype) ? ' ' . WebDevIconsGetFileTypeSymbol() . ' ' . &filetype : '') : ''
  return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype . ' ' . WebDevIconsGetFileTypeSymbol() : 'no ft') : ''
endfunction

function! LightLineFileformat()
  return winwidth(0) > 70 ? (&fileformat . ' ' . WebDevIconsGetFileFormatSymbol()) : ''
endfunction

function! FloatingFZF()
  let buf = nvim_create_buf(v:false, v:true)
  call setbufvar(buf, '&signcolumn', 'no')

  let height = float2nr(20)
  let width = float2nr(90)
  let horizontal = float2nr((&columns - width) / 2)
  let vertical = 1

  let opts = {
        \ 'relative': 'editor',
        \ 'row': vertical,
        \ 'col': horizontal,
        \ 'width': width,
        \ 'height': height,
        \ 'style': 'minimal'
        \ }

  call nvim_open_win(buf, v:true, opts)
endfunction

let $FZF_DEFAULT_OPTS='--layout=reverse --margin=1,2'
let g:fzf_layout = { 'window': 'call FloatingFZF()' }
