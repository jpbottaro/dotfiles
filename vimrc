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
Bundle 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/'}
Bundle 'msanders/snipmate.vim'
Bundle 'vim-scripts/YankRing.vim'
Bundle 'tpope/vim-fugitive'
Bundle 'kchmck/vim-coffee-script'
Bundle 'kien/ctrlp.vim'
Bundle 'derekwyatt/vim-scala'
Bundle 'majutsushi/tagbar'
Bundle 'klen/python-mode'
Bundle 'davidhalter/jedi-vim'
Bundle 'bling/vim-bufferline'
"Bundle 'fholgado/minibufexpl.vim'

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

" don't break syntax highlighting, always rescan from start
syntax sync minlines=300

" show no line numbers
set nu

" change buffer - without saving
set hid

" color scheme
colorscheme desert

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

" delete buffer without closing window
function! Bclose()
    let curbufnr = bufnr("%")
    let altbufnr = bufnr("#")

    if buflisted(altbufnr)
        buffer #
    else
        bnext
    endif

    if bufnr("%") == curbufnr
        new
    endif

    if buflisted(curbufnr)
        execute("bdelete " . curbufnr)
    endif
endfunction

" buffer movement
map <C-Tab> :bn<cr>
map <C-S-Tab> :bp<cr>
map <C-W> :call Bclose()<cr>

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

" * PowerLine
"""""""""""""

let g:Powerline_symbols = 'fancy'

" * Tagbar
""""""""""

autocmd FileType * nested :call tagbar#autoopen(0)

" * Python-mode
"""""""""""""""

let g:pymode_rope = 0

" Documentation
let g:pymode_doc = 1
let g:pymode_doc_key = 'K'

" Linting
let g:pymode_lint = 1
let g:pymode_lint_checker = "pyflakes,pep8"
let g:pymode_lint_ignore = "E221,E272"
let g:pymode_lint_write = 1

" Support virtualenv
let g:pymode_virtualenv = 1

" Enable breakpoints plugin
let g:pymode_breakpoint = 1
let g:pymode_breakpoint_key = '<leader>b'

" syntax highlighting
let g:pymode_syntax = 1
let g:pymode_syntax_all = 1
let g:pymode_syntax_indent_errors = g:pymode_syntax_all
let g:pymode_syntax_space_errors = g:pymode_syntax_all

" Don't autofold code
let g:pymode_folding = 0
