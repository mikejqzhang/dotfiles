"---------------------
" defaults
"---------------------
set nocompatible " not vi compatible

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
set colorcolumn=101 " show line character limit

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
nnoremap <C-w><C-h> :vertical resize -10<CR>
nnoremap <C-w><C-j> :res +5<CR>
nnoremap <C-w><C-k> :res -5<CR>
nnoremap <C-w><C-l> :vertical resize +10<CR>

nnoremap <leader>v :set paste!<CR>

"" set clipboard=unnamed " Sets to use system clipboard

" movement relative to display lines
" nnoremap <silent> <Leader>d :call ToggleMovementByDisplayLines()<CR>
" function SetMovementByDisplayLines()
"     noremap <buffer> <silent> <expr> k v:count ? 'k' : 'gk'
"     noremap <buffer> <silent> <expr> j v:count ? 'j' : 'gj'
"     noremap <buffer> <silent> 0 g0
"     noremap <buffer> <silent> $ g$
" endfunction
" 
" function ToggleMovementByDisplayLines()
"     if !exists('b:movement_by_display_lines')
"         let b:movement_by_display_lines = 0
"     endif
"     if b:movement_by_display_lines
"         let b:movement_by_display_lines = 0
"         silent! nunmap <buffer> k
"         silent! nunmap <buffer> j
"         silent! nunmap <buffer> 0
"         silent! nunmap <buffer> $
"     else
"         let b:movement_by_display_lines = 1
"         call SetMovementByDisplayLines()
"     endif
" endfunction
" nnoremap <C-n> :set rnu!<CR>
" :call ToggleMovementByDisplayLines()
"
" syntax highlighting
syntax enable
autocmd BufNewFile,BufRead *.jsonl set syntax=json

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

" ===== "nerdtree" =====
" Close the tab if NERDTree is the only window remaining in it.
autocmd BufEnter * if winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() 
      \ | quit | endif

nnoremap <leader>n :NERDTreeToggle<CR>
" nnoremap <leader>f :NERDTreeFind<CR>

" ===== "Theme" =====
set background=dark
let g:solarized_bold = 0
let g:solarized_underline = 0
let g:solarized_italic = 0
let g:solarized_visibility = 'high'
let g:solarized_contrast = 'high'
let g:solarized_termcolors = 16
colorscheme solarized " This must be set after all solarized things

" ===== "ale" =====
:hi clear SignColumn " Removes bugged highlight in gutter. Must be done after loading colorscheme.
let g:ale_sign_error = '!!'
let g:ale_sign_warning = '--'
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%:%severity%] %s [%code%]'
let g:ale_linters={
      \ 'python': ['pylint', 'pylsp'],
      \}
let g:ale_completion_enabled = 1
nnoremap <leader>g :ALEGoToDefinition<CR>
nnoremap <leader>d :call ToggleALEHover()<CR>

function! ToggleALEHover() abort
    " loop through all the windows in the current tab page
    for win in range(1, winnr('$'))
      " let preview_window = getwinvar(win, 'ALEPreviewWindow') ? win : 0
      let preview_window = getwinvar(win, '&previewwindow') ? win : 0
    endfor
    if preview_window > 0
      pclose
    else
      ALEHover
    endif
endfunction


" ===== "Completion" =====
" let g:SuperTabCrMapping = 1
" let g:SuperTabMappingForward  = '<s-tab>'
" let g:SuperTabMappingBackward = '<tab>'
" packadd supertab

" if (v:version >= 801 && has("python3")) || has("nvim")
"   packadd YouCompleteMe
"   let g:ycm_key_list_stop_completion = ['<Enter>']
"   let g:ycm_autoclose_preview_window_after_completion = 1
"   nnoremap <leader>g :YcmCompleter GoTo<CR>
"   nnoremap <leader>c :pc<CR>
"   function! ToggleYcmGetDoc() abort
"       " loop through all the windows in the current tab page
"       for win in range(1, winnr('$'))
"         let preview_window = getwinvar(win, '&previewwindow') ? win : 0
"       endfor
"       if preview_window > 0
"         pclose
"       else
"         YcmCompleter GetDoc
"       endif
"   endfunction
"   nnoremap <leader>d :call ToggleYcmGetDoc()<CR>
" else
"   packadd supertab
" endif
"
" ===== "Completion" =====
" let g:SuperTabCrMapping = 1
" let g:SuperTabMappingForward  = '<s-tab>'
" let g:SuperTabMappingBackward = '<tab>'
" packadd supertab
"
" ===== "Copilot" =====
if has("nvim")
  packadd copilot.vim
else
  let g:SuperTabCrMapping = 1
  let g:SuperTabMappingForward  = '<s-tab>'
  let g:SuperTabMappingBackward = '<tab>'
  packadd supertab
endif



" ===== "argwrap" =====
nnoremap <silent> <leader>a :ArgWrap<CR>

" undo history
set undodir=~/.vim/undo-history
set undofile

" gundo
nnoremap <Leader>u :GundoToggle<CR>
let g:gundo_width = 60
let g:gundo_preview_height = 20
if has('python3')
    let g:gundo_prefer_python3 = 1
endif

" easymotion
map <Space> <Plug>(easymotion-prefix)

" incsearch
map / <Plug>(incsearch-forward)
map ? <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

" incsearch-easymotion
map <Space>/ <Plug>(incsearch-easymotion-/)
map <Space>? <Plug>(incsearch-easymotion-?)
map <Space>g/ <Plug>(incsearch-easymotion-stay)

" over
noremap <Space>: :OverCommandLine<CR>

let g:ackprg = 'ag --vimgrep'
command -nargs=+ Gag Gcd | Ack! <args>
nnoremap K :Gag "\b<C-R><C-W>\b"<CR>:cw<CR>
if executable('ag')
    let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'
    let g:ackprg = 'ag --vimgrep'
endif

cnoreabbrev Ack Ack!
nnoremap <leader>f :Ack!<Space>

nmap Q <Nop>
"---------------------
" Local customizations
"---------------------
" local customizations in ~/.vimrc_local
let $LOCALFILE=expand("~/.vimrc_local")
if filereadable($LOCALFILE)
  source $LOCALFILE
endif

