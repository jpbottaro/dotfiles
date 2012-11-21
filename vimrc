" dont emulate vi
set nocompatible
filetype on
filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required!
Bundle 'gmarik/vundle'

" original repos on github
Bundle 'msanders/snipmate.vim'
Bundle 'chrismetcalf/vim-yankring'
Bundle 'fholgado/minibufexpl.vim'
Bundle 'tpope/vim-fugitive'
Bundle 'kchmck/vim-coffee-script'
Bundle 'Lokaltog/vim-powerline'
Bundle 'kien/ctrlp.vim'

" enable filetype plugin
filetype plugin indent on

" autoread when a file is changed from the outside
set autoread

" set leader to ,
let mapleader = ","
let g:mapleader = ","

" * Misc
""""""""

" undo levels
set undolevels=100

" have fifty lines of command-line (etc) history:
set history=100

" * User Interface
""""""""""""""""""

" syntax highlighting
syntax on

" show no line numbers
set nu

" change buffer - without saving
set hid

" color scheme
colorscheme desert

" have command-line completion (for filenames, help topics, option names)
set wildmode=list:longest,full

" ignore case when searching
set ignorecase

" highlight as you type you search phrase
set incsearch

" show matching bracets when text indicator is over them
set showmatch
set mat=2

" no sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=500

" * Text Formatting
"""""""""""""""""""

" use indents of 4 spaces, and have them copied down lines:
set shiftwidth=4
set tabstop=4
set autoindent
set smartindent
set smarttab
set expandtab

" max text width
set textwidth=80

" * Backups
"""""""""""

" no backups
set nobackup
set nowb
set noswapfile

" persistent undo (vim 7.3)
try
	set undodir=~/.vim/vimundodir
	set undofile
catch
endtry

" * Bindings
""""""""""""""""

" bash like keys for the command line
cnoremap <C-A> <Home>
cnoremap <C-E> <End>
cnoremap <C-K> <C-U>
cnoremap <C-P> <Up>
cnoremap <C-N> <Down>

" smart way yo move between windows
map  <C-j> :wincmd w<cr>
imap <C-j> :wincmd w<cr>
map  <C-k> :wincmd W<cr>
imap <C-k> :wincmd W<cr>
map  <C-h> :wincmd <<cr>
imap <C-h> :wincmd <<cr>
map  <C-l> :wincmd ><cr>
imap <C-l> :wincmd ><cr>

" quit insert mode with jk
imap jk <Esc>

" * Buffers
"""""""""""

" buffer movement
map <C-Tab> :bn<cr>
map <C-S-Tab> :bp<cr>
map <C-W> :bd<cr>

" * Editing Mappings
""""""""""""""""""""

" move a line using alt+[jk]
nmap <M-j> mz:m+<cr>`z
nmap <M-k> mz:m-2<cr>`z
vmap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vmap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

" delete trailing white space, useful for python
func! DeleteTrailingWS()
	exe "normal mz"
	%s/\s\+$//ge
	exe "normal `z"
endfunc
autocmd BufWrite *.py :call DeleteTrailingWS()

set guitablabel=%t

" * Spell checking
""""""""""""""""""

" pressing ,ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>

" shortcuts using <leader>
map <leader>sn ]s
map <leader>sp [s
map <leader>sa zg
map <leader>s? z=

" * Yankring
""""""""""""

nnoremap <silent> <F4> :YRShow<cr>
let g:yankring_history_dir ='$HOME/.vim/'

" * MiniBufExpl
"""""""""""""""

let g:miniBufExplMaxSize = 1
let g:miniBufExplCheckDupeBufs = 0

" * Fugitive
""""""""""""

map <leader>gb :Gblame<cr>
map <leader>gc :Gcommit<cr>
map <leader>gd :Gdiff<cr>
map <leader>gl :Glog<cr>
map <leader>gs :Gstatus<cr>

" * Status Line
"""""""""""""""

set laststatus=2 "Always show the statusline

let g:statusline_fugitive = 1
let g:statusline_fullpath = 1
let g:statusline_enabled = 1
let g:statusline_order = [
	\ 'Filename',
	\ 'CheckUnix',
	\ 'Encoding',
	\ 'Modified',
	\ 'Fugitive',
	\ 'TabWarning',
	\ 'TrailingSpaceWarning',
	\ 'Paste',
	\ 'ReadOnly',
	\ 'RightSeperator',
	\ 'CursorColumn',
	\ 'LineAndTotal',
	\ 'FilePercent']

" * PowerLine
"""""""""""""

let g:Powerline_symbols = 'fancy'
