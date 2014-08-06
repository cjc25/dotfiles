set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'altercation/vim-colors-solarized'
Plugin 'tpope/vim-sensible'

" Improved C++ syntax highlighting
Plugin 'vim-jp/cpp-vim'
Plugin 'octol/vim-cpp-enhanced-highlight'

" <leader>cc comments lines/regions. <leader>cu undoes comments.
Plugin 'scrooloose/nerdcommenter'

" Highlights syntax breakages
Plugin 'scrooloose/syntastic'

" <leader>be shows all loaded buffers in a selector window.
Plugin 'bufexplorer.zip'

" Paired actions with [ and ]. In general, caps are first/last, lowercase are
" previous/next. Sometimes it's on/off, before/after, above/below. Lots of docs
" are at :help unimpaired.
Plugin 'tpope/vim-unimpaired'

" youcompleteme for golang.
Plugin 'Blackrush/vim-gocode'

" Improved statusline
Plugin 'bling/vim-airline'
call vundle#end()

" Solarized dark is pretty, but better without italics
let g:solarized_italic = 0
colorscheme solarized

if filereadable(expand("~/google.vimrc"))
  source ~/google.vimrc
endif

" GENERAL SETTINGS
" Embrace the darkness.
set background=dark
" Always set windows to the same size when they're split.
set equalalways
" Spaces are better than tabs.
set expandtab
" Don't unload buffers when the window holding them is closed, so they can be
" found with bufexplorer.
set hidden
" Highlight search results.
set hlsearch
" Show line numbers.
set number
" Highlight matching parens, etc.
set showmatch
" Don't show INSERT/REPLACE/VISUAL since we have powerline.
set showmode!
" Change the window manager title.
set title
" Show a column at the 81st character. Note that DarkRed is actually quite
" understated in solarized dark (same as the grey in guibg).
set colorcolumn=81
hi ColorColumn ctermbg=DarkRed guibg=#592929
" Change the directory for :o, :Ex, etc. to the current file's directory.
au BufEnter * silent! lcd %:p:h
" Highlight the window statusline in Cyan, which is actually quite understated
" in solarized dark (light grey).
hi StatusLine ctermfg=Cyan
" Remap escape to jk
inoremap jk <Esc>

" Go support.
autocmd FileType go autocmd BufWritePre <buffer> Fmt
" Real tabs are the style in Go, but only use 2 characters for them for
" prettiness.
autocmd BufEnter *.go silent! set shiftwidth=2 tabstop=2 softtabstop=2 noexpandtab

" vim-airline setup
" Use the smart tab line
let g:airline#extensions#tabline#enabled = 1

" powerline symbols
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

" If we are using a GUI, use a good font.
set guifont=DejaVu_Sans_Mono_for_Powerline:h9:cANSI

filetype plugin indent on
