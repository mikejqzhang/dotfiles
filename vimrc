"---------------------
" essentials
"---------------------

" not vi compatible
set nocompatible

" fight me
" imap jk <esc>
imap kj <esc>

" jump visual lines for line-wrapping
nmap j gj
nmap k gk

" system clipboard
set clipboard=unnamed


"---------------------
" VimPlug
"---------------------
call plug#begin('~/.vim/bundle')
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/nerdcommenter'
Plug 'tmux-plugins/vim-tmux'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'kien/ctrlp.vim'
" Plug 'ervandew/supertab'
Plug 'christoomey/vim-tmux-navigator'
Plug 'majutsushi/tagbar'
Plug 'junegunn/vim-easy-align'
Plug 'easymotion/vim-easymotion'
Plug 'haya14busa/incsearch-easymotion.vim'
Plug 'haya14busa/incsearch.vim'
Plug 'scrooloose/syntastic'
Plug 'flazz/vim-colorschemes'
Plug 'altercation/vim-colors-solarized'
Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'
call plug#end()


"---------------------
" Syntastic
"---------------------
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_mode_map = {
      \ 'mode': 'passive',
      \ 'active_filetypes': [],
      \ 'passive_filetypes': []
      \}

"---------------------
" EasyAlign
"---------------------
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

"---------------------
" NerdTree
"---------------------
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
autocmd BufEnter * lcd %:p:h

"---- "FileType Colors" ----"
function! NERDTreeHighlightFile(extension, fg, bg, guifg, guibg)
  exec 'autocmd filetype nerdtree highlight ' . a:extension .' ctermbg='.  a:bg .' ctermfg='. a:fg .' guibg='. a:guibg .' guifg='. a:guifg
  exec 'autocmd filetype nerdtree syn match ' . a:extension .' #^\s\+.*'.  a:extension .'$#'
endfunction

call NERDTreeHighlightFile('py', 'green', 'none', 'green', '#151515')
call NERDTreeHighlightFile('ipynb', 'cyan', 'none', 'cyan', '#151515')


"---------------------
" a e s t h e t i c
"---------------------

set t_co=256
set guifont=inconsolata\ for\ powerline:h15
let g:powerline_symbols='fancy'
set encoding=utf-8
set fillchars+=stl:\ ,stlnc:\
set term=screen-256color
set termencoding=utf-8

"---- "Solarized Theme" ----"
if !exists("g:syntax_on")
  syntax enable
endif

set background=dark
colorscheme solarized
let g:solarized_visibility = "high"
let g:solarized_contrast = "high"

"---- "AirLine Theme" ----"
let g:airline_theme='solarized'
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_symbols.space = "\ua0"


"---------------------
" Personal Shortcuts
"---------------------

let mapleader = ","

"---- "Editing" ----"
nnoremap <C-n> :set rnu!<cr>
nnoremap <leader>v :set paste!<cr>
nnoremap <leader>ww :%s/\s\+$//g<cr>
nnoremap <leader>ee gg=G<cr>

"---- "syntastic" ----"
nnoremap <leader>s :SyntasticCheck<cr>
nnoremap <leader>r :SyntasticReset<cr>
nnoremap <leader>i :SyntasticInfo<cr>
nnoremap <leader>m :SyntasticToggleMode<cr>

"---- "nerdtree" ----"
nnoremap <leader>n :NERDTreeToggle<cr>
nnoremap <leader>f :NERDTreeFind<cr>

"---- "Tagbar" ----"
nnoremap <leader>t :TagbarToggle<cr>


"---------------------
" basic editing config
"---------------------
set nu " line numbers
set rnu " relative line numbers
set cursorline " highlight selected line
set incsearch " incremental search
set hls " highlighted search
set listchars=tab:>>,nbsp:~
set lbr
set ruler
set scrolloff=5 " buffer 5 lines from top & bottom (when possible)
set showcmd
set noshowmode
set showmatch " show matching highlighted parens
set colorcolumn=101 " show line character limit

" 2-space soft tabs
filetype indent plugin on " enable filetype detection
set smarttab
set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2

" smart case-sensitive search
set ignorecase
set smartcase
set ttimeoutlen=50

" disable audible bell
set noerrorbells visualbell t_vb=

" Tab copletion for files/buffer
set wildmode=longest,list
set wildmenu

"---- "vim splits" ----"
" change splits to act like tmux
set splitbelow
set splitright

" split opening
nnoremap <C-w>h :split<cr>
nnoremap <C-w>u :vertical split<cr>

" split resizing
nnoremap <C-w><C-h> :vertical resize +10<cr>
nnoremap <C-w><C-j> :res -5<cr>
nnoremap <C-w><C-k> :res +5<cr>
nnoremap <C-w><C-l> :vertical resize -10<cr>

"---------------------
" Local customizations
"---------------------

" local customizations in ~/.vimrc_local
let $LOCALFILE=expand("~/.vimrc_local")
if filereadable($LOCALFILE)
  source $LOCALFILE
endif

