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
Bundle 'bling/vim-airline'
Bundle 'scrooloose/nerdtree'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-commentary'

" enable filetype plugin
filetype plugin indent on

" autoread when a file is changed from the outside
set autoread

" set leader to ,
let mapleader = ','
let g:mapleader = ','

" * Misc
""""""""

" undo levels
set undolevels=100

" have fifty lines of command-line (etc) history:
set history=100

" sane backspace
set backspace=indent,eol,start

" * User Interface
""""""""""""""""""

" syntax highlighting
syntax on

" don't break syntax highlighting, always rescan from start
syntax sync minlines=300

" show line numbers
set nu

" change buffer - without saving
set hid

" color scheme
colorscheme desert

" ignore case when searching
set ignorecase
set smartcase

" highlight as you type you search phrase
set incsearch

" show matching bracets when text indicator is over them
set showmatch
set mat=2

" no sound on errors
set noerrorbells
set visualbell
set t_vb=
set tm=500

" highlight as you type you search phrase
set scrolloff=5

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

" delete buffer without closing window
function! Bclose()
    let curbufnr = bufnr('%')
    let altbufnr = bufnr('#')

    if buflisted(altbufnr)
        buffer #
    else
        bnext
    endif

    if bufnr('%') == curbufnr
        new
    endif

    if buflisted(curbufnr)
        execute('bdelete ' . curbufnr)
    endif
endfunction

" buffer movement
map <C-Tab> :bn<cr>
map <C-S-Tab> :bp<cr>
map <C-W> :call Bclose()<cr>

" autocomplete
imap <C-N> <C-X><C-O><C-P>
set completeopt-=preview

" * Airline
"""""""""""""

set laststatus=2
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamecollapse = 0
let g:airline_powerline_fonts = 1
let g:airline_theme='dark'

" * NERDTree
"""""""""""""""

map <C-O> :NERDTreeToggle<cr>
let NERDTreeIgnore = ['\.pyc$']
