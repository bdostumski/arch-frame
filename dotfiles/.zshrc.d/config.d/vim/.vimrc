" BASIC SETTINGS ----------------------------------------------------------
colorscheme wildcharm
set nocompatible
set mouse=a
filetype plugin indent on
syntax on
set number
set relativenumber
set shiftwidth=4
set tabstop=4
set expandtab
set nobackup
set noswapfile
set scrolloff=10
set nowrap
set incsearch
set ignorecase
set smartcase
set showcmd
set showmode
set showmatch
set hlsearch
set history=1000
set wildmenu
set wildmode=list:longest
set wildignore=*.docx,*.jpg,*.png,*.gif,*.pdf,*.pyc,*.exe,*.flv,*.img,*.xlsx
set cursorline
set clipboard=unnamedplus

" STATUS LINE ------------------------------------------------------------
set statusline=
set statusline+=\ %F\ %M\ %Y\ %R
set statusline+=%=
set statusline+=\ ascii:\ %b\ hex:\ 0x%B\ row:\ %l\ col:\ %c\ percent:\ %p%%
set laststatus=2

" MAPPINGS ---------------------------------------------------------------

" General keybindings
let mapleader = " "
nnoremap <leader>t :terminal<CR>
nnoremap <leader>n :Ntree<CR>

" Window navigation
nnoremap <leader>s <c-w>j
nnoremap <leader>w <c-w>k
nnoremap <leader>a <c-w>h
nnoremap <leader>d <c-w>l

" Resize splits
noremap <c-up> <c-w>+
noremap <c-down> <c-w>-
noremap <c-left> <c-w>>
noremap <c-right> <c-w><

" TABS -------------------------------------------------------------------
nnoremap <leader>z :tabnew<CR>
nnoremap <leader>x :tabclose<CR>
nnoremap <leader>v :tabmove
nnoremap <leader>c :tabonly<CR>
nnoremap <leader>e :tabnext<CR>
nnoremap <leader>q :tabprevious<CR>
nnoremap <leader>1 1gt
nnoremap <leader>2 2gt
nnoremap <leader>3 3gt
nnoremap <leader>4 4gt
nnoremap <leader>5 5gt
nnoremap <leader>6 6gt
nnoremap <leader>7 7gt
nnoremap <leader>8 8gt
nnoremap <leader>9 9gt
nnoremap <leader>0 :tablast<CR>

