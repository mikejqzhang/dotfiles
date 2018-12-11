"---------------------
" essentials
"---------------------

" not vi compatible
set nocompatible

runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()

"---------------------
" Plugin configuration
"---------------------

" lightline
set laststatus=2 " required for lightline to show up
" TODO: Configure lightline
let g:lightline = {'colorscheme': 'solarized'}

" ctrl-p
" TODO: Add in buffer stuff
let g:ctrlp_working_path_mode = 'ra'

" syntastic
" TODO: understand these settings
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_mode_map = {
      \ 'mode': 'passive',
      \ 'active_filetypes': [],
      \ 'passive_filetypes': []
      \}
nnoremap <leader>s :SyntasticCheck<CR>
nnoremap <leader>r :SyntasticReset<CR>
nnoremap <leader>i :SyntasticInfo<CR>
nnoremap <leader>m :SyntasticToggleMode<CR>

" nerdtree
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
autocmd BufEnter * lcd %:p:h

function! NERDTreeHighlightFile(extension, fg, bg, guifg, guibg)
  exec 'autocmd filetype nerdtree highlight ' . a:extension .' ctermbg='.  a:bg .' ctermfg='. a:fg .' guibg='. a:guibg .' guifg='. a:guifg
  exec 'autocmd filetype nerdtree syn match ' . a:extension .' #^\s\+.*'.  a:extension .'$#'
endfunction

call NERDTreeHighlightFile('py', 'green', 'none', 'green', '#151515')
call NERDTreeHighlightFile('ipynb', 'cyan', 'none', 'cyan', '#151515')

nnoremap <leader>n :NERDTreeToggle<CR>
nnoremap <leader>f :NERDTreeFind<CR>

"---------------------
" a e s t h e t i c
"---------------------

set encoding=utf-8
set fillchars+=stl:\ ,stlnc:\

"---- "Solarized Theme" ----"
syntax enable
set background=dark
colorscheme solarized " This must be set after all the settings are done

"---------------------
" Personal Shortcuts
"---------------------

let mapleader = ","

"---- "Editing" ----"
nnoremap <C-n> :set rnu!<CR>
nnoremap <leader>v :set paste!<CR>
nnoremap <leader>ww :%s/\s\+$//g<CR>
nnoremap <leader>ee gg=G<CR>

"---------------------
" basic editing config
"---------------------
" fight me
imap jk <esc>
imap kj <esc>

set nu " line numbers
" set rnu " relative line numbers slowaf
" set cursorline " highlight selected line
" highlight current line, but only in active window
augroup CursorLineOnlyInActiveWindow
    autocmd!
    autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline
    autocmd WinLeave * setlocal nocursorline
augroup END

set incsearch " incremental search
set hls " highlighted search
set listchars=tab:>>,nbsp:~
set lbr
set ruler
set scrolloff=5 " buffer 5 lines from top & bottom (when possible)
" set showcmd
" set noshowmode
" set showmatch " show matching highlighted parens
set colorcolumn=101 " show line character limit

" jump visual lines for line-wrapping
nmap j gj
nmap k gk

" 2-space soft tabs
filetype indent plugin on " enable filetype detection
set smarttab
set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2

" 4 space tabs only in python
autocmd Filetype python setlocal ts=4 sw=4 sts=0 expandtab

" smart case-sensitive search
set ignorecase
set smartcase
set ttimeoutlen=50

" disable audible bell
set noerrorbells visualbell t_vb=

" Tab copletion for files/buffer
set wildmode=longest,list
set wildmenu

" Change vim splits to act like tmux
set splitbelow
set splitright

" split opening
nnoremap <C-w>h :split<CR>
nnoremap <C-w>u :vertical split<CR>

" split resizing
nnoremap <C-w><C-h> :vertical resize +10<CR>
nnoremap <C-w><C-j> :res -5<CR>
nnoremap <C-w><C-k> :res +5<CR>
nnoremap <C-w><C-l> :vertical resize -10<CR>

" set clipboard=unnamed " Sets to use system clipboard

"---------------------
" Local customizations
"---------------------

" local customizations in ~/.vimrc_local
let $LOCALFILE=expand("~/.vimrc_local")
if filereadable($LOCALFILE)
  source $LOCALFILE
endif

