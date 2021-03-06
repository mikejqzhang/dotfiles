"---------------------
" essentials
"---------------------
" not vi compatible
set nocompatible

runtime bundle/vim-pathogen/autoload/pathogen.vim
call pathogen#infect()
call pathogen#helptags()


"---------------------
" defaults
"---------------------
set nu " line numbers
set rnu " relative line numbers slowaf

set hls " highlighted search
set incsearch " incremental search

set listchars=tab:>>,nbsp:~ " set list to see tabs and non-breakable spaces
set linebreak " line break

set backspace=indent,eol,start " allow backspacing over everything

set scrolloff=5 " buffer 5 lines from top & bottom (when possible)
set showcmd " show current command
set noshowmode
set showmatch " show matching highlighted parens
set colorcolumn=81 " show line character limit

set lazyredraw " skip redrawing screen in some cases
set history=10000 " more history

set nojoinspaces " suppress inserting two spaces between sentences

set encoding=utf-8
set fillchars+=stl:\ ,stlnc:\

" 2-space soft tabs
filetype indent plugin on " enable filetype detection
set smarttab
set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2

" 4 space tabs only in python
autocmd Filetype python setlocal ts=4 sw=4 sts=0

" smart case-sensitive search
set ignorecase
set smartcase
set ttimeoutlen=50

" disable audible bell
set noerrorbells visualbell t_vb=

" Tab copletion for files/buffer
set wildmenu
set wildmode=longest,list

" Change vim splits to act like tmux
set splitbelow
set splitright

" highlight current line, but only in active window
augroup CursorLineOnlyInActiveWindow
    autocmd!
    autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline
    autocmd WinLeave * setlocal nocursorline
augroup END


"---------------------
" editing config
"---------------------
" fight me
imap jk <esc>
imap kj <esc>

" set leader key
let mapleader = ","

" jump visual lines for line-wrapping
nmap j gj
nmap k gk

" split opening
nnoremap <C-w>h :split<CR>
nnoremap <C-w>u :vertical split<CR>

" split resizing
nnoremap <C-w><C-h> :vertical resize +10<CR>
nnoremap <C-w><C-j> :res -5<CR>
nnoremap <C-w><C-k> :res +5<CR>
nnoremap <C-w><C-l> :vertical resize -10<CR>

nnoremap <C-n> :set rnu!<CR>
nnoremap <leader>v :set paste!<CR>
nnoremap <leader>ww :%s/\s\+$//g<CR>

"" set clipboard=unnamed " Sets to use system clipboard


"---------------------
" tabs config
"---------------------
noremap <C-w>c :tabnew<CR>
noremap <C-w>n :tabnext<CR>
noremap <C-w>p :tabprev<CR>
autocmd TabLeave * let g:lasttab = tabpagenr()
noremap <C-w>l :exe "tabn ".g:lasttab<CR>

noremap <C-w>1 1gt
noremap <C-w>2 2gt
noremap <C-w>3 3gt
noremap <C-w>4 4gt
noremap <C-w>5 5gt
noremap <C-w>6 6gt
noremap <C-w>7 7gt
noremap <C-w>8 8gt
noremap <C-w>9 9gt


"---------------------
" Plugin configuration
"---------------------
" ===== "buffergator" =====
" buffergator
let g:buffergator_suppress_keymaps = 1
nnoremap <Leader>b :BuffergatorToggle<CR>

" ===== "ctrl-p" =====
let g:ctrlp_working_path_mode = 'ra'
nnoremap <C-b> :CtrlPBuffer<CR>
set wildignore+=*/tmp/*,*.so,*.swp,*.zip
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_user_command = 'git ls-files . --cached --exclude-standard --others'


" ===== "lightline" =====
set laststatus=2 " required for lightline to show up
" TODO: Configure lightline
let g:lightline = {
      \ 'colorscheme': 'solarized',
      \ 'enable': {'statusline': 1, 'tabline': 1}
      \ }


" ===== "syntastic" =====
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

" ===== "nerdtree" =====
" TODO: understand these settings
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
autocmd BufEnter * lcd %:p:h

function! NERDTreeHighlightFile(extension, fg, bg, guifg, guibg)
  exec 'autocmd filetype nerdtree highlight ' . a:extension .' ctermbg='. 
        \ a:bg .' ctermfg='. a:fg .' guibg='. a:guibg .' guifg='. a:guifg
  exec 'autocmd filetype nerdtree syn match ' . a:extension .' #^\s\+.*'. 
        \ a:extension .'$#'
endfunction

call NERDTreeHighlightFile('py', 'green', 'none', 'green', '#151515')
call NERDTreeHighlightFile('ipynb', 'cyan', 'none', 'cyan', '#151515')

nnoremap <leader>n :NERDTreeToggle<CR>
nnoremap <leader>f :NERDTreeFind<CR>

" ====== "solarized theme" ======
" ===== "a e s t h e t i c" =====
syntax enable
set background=dark

let g:solarized_bold = 0
let g:solarized_underline = 0
let g:solarized_italic = 0
let g:solarized_visibility = 'high'
let g:solarized_contrast = 'high'
let g:solarized_termcolors = 16
colorscheme solarized " This must be set after all solarized things

"---------------------
" Local customizations
"---------------------
" local customizations in ~/.vimrc_local
let $LOCALFILE=expand("~/.vimrc_local")
if filereadable($LOCALFILE)
  source $LOCALFILE
endif

