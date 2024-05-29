let s:at_google = filereadable(expand("~/google.vimrc"))

set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'tpope/vim-sensible'

" Improved C++ syntax highlighting
Plugin 'bfrg/vim-cpp-modern'

" <leader>cc comments lines/regions. <leader>cu undoes comments.
Plugin 'scrooloose/nerdcommenter'

" Highlights syntax breakages
Plugin 'scrooloose/syntastic'

" <leader>be shows all loaded buffers in a selector window.
Plugin 'bufexplorer.zip'

" Awesome golang support
Plugin 'fatih/vim-go'

" Improved statusline
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'

" Snippets
Plugin 'SirVer/ultisnips'

" TLA+ highlighting
Plugin 'hwayne/tla.vim'

" Use Valloric/YouCompleteMe for speedy autocompletions.
if !s:at_google
  Plugin 'Valloric/YouCompleteMe'
endif
call vundle#end()

if s:at_google
  source ~/google.vimrc
endif

" This slows down vim in diff mode, but can be useful on demand.
let g:ycm_auto_hover = ""
nmap <leader>D <plug>(YCMHover)

" GENERAL SETTINGS
" If we have the selenized colorscheme, use it.
try
  set termguicolors
  colorscheme selenized
catch
  set termguicolors&
endtry
" Embrace the darkness.
set background=dark
" Always set windows to the same size when they're split.
set equalalways
" Spaces are better than tabs.
set expandtab
" But let's not tab too far.
set shiftwidth=2
set tabstop=2
set softtabstop=2
" Don't unload buffers when the window holding them is closed, so they can be
" found with bufexplorer.
set hidden
" Highlight search results.
set hlsearch
" Show line numbers.
set number
" Don't show INSERT/REPLACE/VISUAL since we have powerline.
set showmode!
" Change the window manager title.
set title
" Show a column at the 81st character.
set colorcolumn=81
hi ColorColumn ctermbg=DarkRed guibg=#592929
" Allow paragraph formatting to use the 80th column
set textwidth=80
" When joining lines, don't use two spaces after a .
set nojoinspaces

" Change the directory for :o, :Ex, etc. to the current file's directory.
au BufEnter * silent! lcd %:p:h
" Use - as netrw parent dir
nnoremap - :Ex<Return>
" Remap escape to jk
inoremap jk <Esc>

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
