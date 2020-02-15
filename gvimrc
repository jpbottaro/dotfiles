" remove bars
set guioptions=

" set font to inconsolata (remember to install it first)
set guifont=Inconsolata\ 12

" nvim-qt fixes
try
    GuiTabline 0
    GuiFont Inconsolata:h12
    set mouse=a
    vmap <LeftRelease> "*ygv
catch
endtry

" set visual bell (this removes the audio bell on osx)
set novb
