" dont emulate vi
set nocompatible

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

" show line and column numbers
    set ruler

" change buffer - without saving
    set hid

" syntax highlighting
    syntax on

" color scheme
    colorscheme desert
    "colorscheme molokai

" have command-line completion <Tab> (for filenames, help topics, option names)
" first list the available options and complete the longest common part, then
" have further <Tab>s cycle through the possibilities:
    set wildmode=list:longest,full

" display the current mode and partially-typed commands in the status line:
    set showmode
    set showcmd

" ignore case when searching
    set ignorecase

" highlight as you type you search phrase
    set incsearch

" shoe matching bracets when text indicator is over them
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
    set expandtab
    set autoindent
    set smartindent
    set smarttab
    set wrap

" max text width
    set textwidth=80

" * Backups
"""""""""""
" no backups
    set nobackup
    set nowb
    set noswapfile

" persistent undo
    try
        set undodir=~/.vim/vimundodir
        set undofile
    catch
    endtry

" * Command Mode
""""""""""""""""
" bash like keys for the command line
    cnoremap <C-A> <Home>
    cnoremap <C-E> <End>
    cnoremap <C-K> <C-U>
    cnoremap <C-P> <Up>
    cnoremap <C-N> <Down>

" smart way yo move between windows
    map <C-j> :wincmd j<cr>
    map <C-k> :wincmd k<cr>
    map <C-h> :wincmd h<cr>
    map <C-l> :wincmd l<cr>

" use the arrows to do something useful
    map <right> :bn<cr>
    map <left> :bp<cr>

" * Tabs
"""""""""""

" switch to the directory of current buffer
	map <leader>cd :cd %:p:h<cr>

" tab configuration
	map <leader>tn :tabnew<cr>
	map <leader>te :tabedit
	map <leader>tc :tabclose<cr>
	map <leader>tm :tabmove

	map <C-T> :tabnew<cr>
	map <C-W> :tabclose<cr>
	map <C-Tab> :tabn<cr>
	map <C-S-Tab> :tabp<cr>

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


" * Yankring
""""""""""""
" open YankRing Buffer 
nnoremap <silent> <F4> :YRShow<cr>

" do not put the yankring_history_v2 file in Home, but in .vim directory
let g:yankring_history_dir='$HOME/.vim/'


" * Spell checking
""""""""""""""""""
" pressing ,ss will toggle and untoggle spell checking
	map <leader>ss :setlocal spell!<cr>

" shortcuts using <leader>
	map <leader>sn ]s
	map <leader>sp [s
	map <leader>sa zg
	map <leader>s? z=

" * TabList
"""""""""""

    nnoremap <silent> <F3> :Tlist<cr>

" * .h
""""""

    nnoremap <silent> <F2> :A<cr>

" * Clojure
"""""""""""

    let vimclojure#HighlightBuiltins = 1

" * Omni Completion
"""""""""""""""""""

    set ofu=syntaxcomplete#Complete
