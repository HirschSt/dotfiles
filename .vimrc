"Install
"mkdir ~/.vim/tmp/backup
"mkdir ~/.vim/tmp/swap
"git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
"
"For vim with lua see http://winter233.com/Using-Vim-with-Neocomplete/
"
set nocompatible
filetype off   

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'L9'
Plugin 'git://git.wincent.com/command-t.git'
Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-bundler'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-endwise'
Plugin 'Yggdroot/indentLine'
Plugin 'sjl/gundo.vim'
Plugin 'mattn/webapi-vim'
Plugin 'mattn/gist-vim'
Plugin 'jiangmiao/auto-pairs'
Plugin 'scrooloose/nerdTree'
Plugin 'scrooloose/syntastic'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'yegappan/mru'
Plugin 'thoughtbot/vim-rspec'
Plugin 'vim-ruby/vim-ruby'
Plugin 'flazz/vim-colorschemes'
Plugin 'jistr/vim-nerdtree-tabs'
Plugin 'majutsushi/tagbar'
Plugin 'xolox/vim-misc'
Plugin 'xolox/vim-colorscheme-switcher'
Plugin 'altercation/vim-colors-solarized'
Plugin 'bling/vim-airline'
Plugin 'ervandew/supertab'
Plugin 'mileszs/ack.vim'
Plugin 'Raimondi/delimitMate'
Plugin 'leafgarland/typescript-vim'
Plugin 'othree/html5.vim'
Plugin 'pangloss/vim-javascript'

call vundle#end()            " required
filetype plugin indent on    " required
set tabstop=2
set shiftwidth=2
set expandtab
let mapleader=","
syntax on
set encoding=utf8
set t_Co=256
let g:solarized_termcolors=256
"set background=dark
"colorscheme solarized 
:hi CursorLine   cterm=NONE ctermbg=238
:hi CursorColumn cterm=NONE ctermbg=238
set backupdir=~/.vim/tmp/backup 
set directory=~/.vim/tmp/swap
set mouse=a
set equalalways
set splitright splitbelow
set autoread
set wildmenu
set wildmode=full
set clipboard+=unnamed,unnamedplus,autoselect
set history=1000
set tags=.tags;/ 
set ttyfast
set tabstop=2
set ttimeoutlen=50 
set showmode
set wrap
set number
set showbreak=↪ 
set showmatch
set backspace=2
nnoremap <C-n> :NERDTreeToggle<CR>
nnoremap <C-t> :Tagbar<CR>
nnoremap <F4> :MRU<CR>
nnoremap <F5> :Ack 
set splitbelow
set splitright
let g:NERDTreeMinimalUI=1
let g:NERDTreeDirArrows=1
let g:SuperTabContextTextOmniPrecedence = ['&completefunc', '&omnifunc']
let g:NERTreeHighlightCursorLine=1
let g:nerdtree_tabs_open_on_console_startup=1
let NERDTreeWinSize = 24
let g:rubycomplete_rails=1
let g:rubycomplete_classes_in_global=1
let g:rubycomplete_buffer_loading=1
let g:rubycomplete_include_object=1
let g:rubycomplete_include_objectspace=1
let MRU_Auto_Close = 0 
let MRU_Window_Height = 6
let g:indentLine_color_term = 236
augroup FTRuby
  au!
  autocmd FileType eruby,ruby        setlocal ai et sta sw=2 sts=2
  autocmd BufNewFile,BufRead *.html.erb   set filetype=eruby.html
  autocmd FileType ruby,eruby             let g:rubycomplete_rails = 1
  autocmd FileType ruby,eruby             let g:rubycomplete_classes_in_global=1
  autocmd FileType ruby,eruby             let g:rubycomplete_buffer_loading = 1
augroup END
augroup CursorLine
  au!
  au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  au WinLeave * setlocal nocursorline
augroup END
if $TERM =~ '^screen-256color'
  map [7~ <Home>
  map [8~ <End>
  imap [7~ <Home>
  imap [8~ <End>
endif
