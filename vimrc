" dont emulate vi
set nocompatible

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required!
Bundle 'gmarik/vundle'

" original repos on github
Bundle 'msanders/snipmate.vim'
"Bundle 'dickeytk/status.vim'
"Bundle 'Lokaltog/vim-powerline'
Bundle 'chrismetcalf/vim-yankring'
Bundle 'fholgado/minibufexpl.vim'
Bundle 'bcnice20/go-vim'
"Bundle 'fsouza/go.vim'
Bundle 'scrooloose/nerdtree'
Bundle 'tpope/vim-fugitive'
"Bundle 'kevinw/pyflakes-vim'
"Bundle 'nvie/vim-pep8'

" enable filetype plugin
filetype plugin on
filetype indent on

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

" show no line numbers
set nu

" change buffer - without saving
set hid

" syntax highlighting
syntax on

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
map <C-j> :wincmd w<cr>
imap <C-j> :wincmd w<cr>
map <C-k> :wincmd W<cr>
imap <C-k> :wincmd W<cr>
map <C-h> :wincmd <<cr>
imap <C-h> :wincmd <<cr>
map <C-l> :wincmd ><cr>
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

" * Tags
""""""""""""""""""

"set tags=./tags;/
" only add tags to the master tag file of the proyect (the script looks for
" it parent by parent)
"au BufWrite *.py,*.rb,*.cpp,*.c,*.h silent !ctagsadd %:p:h %:p &
"map <C-]> g<C-]>

" * Yankring
""""""""""""

nnoremap <silent> <F4> :YRShow<cr>
let g:yankring_history_dir ='$HOME/.vim/'

" * MiniBufExpl
"""""""""""""""

let g:miniBufExplMaxSize = 1

" * Fugitive
""""""""""""

map <leader>gb :Gblame<cr>
map <leader>gc :Gcommit<cr>
map <leader>gd :Gdiff<cr>
map <leader>gl :Glog<cr>
map <leader>gs :Gstatus<cr>

" * NERD_tree
"""""""""""""

let g:NERDTreeShowBookmarks = 1
let NERDTreeIgnore = ['\.pyc']
imap <C-o> <ESC>:NERDTreeToggle<cr>
map <C-o> :NERDTreeToggle<cr>

" * OmniComplete
""""""""""""""""

set ofu=syntaxcomplete#Complete
let Omnifunc_GlobalScopeSearch   = 1
let Omnifunc_DisplayMode         = 1
let Omnifunc_ShowPrototypeInAbbr = 1 "show prototype in pop-up
let Omnifunc_ShowAccess          = 1 "show access in pop-up
let Omnifunc_SelectFirstItem     = 1 "select first item in pop-up
set completeopt=menuone,menu,longest
inoremap <C-Space> <C-x><C-o>

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
