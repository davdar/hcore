if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
map! <F1> <F1>
noremap Y y$
nmap gx <Plug>NetrwBrowseX
onoremap <silent> io :normal vio
vmap <silent> io <Plug>InnerOffside
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#NetrwBrowseX(expand("<cWORD>"),0)
omap <F1> <F1>
vmap <F1> <F1>
nnoremap <F1> :help 
let &cpo=s:cpo_save
unlet s:cpo_save
set autoindent
set backspace=indent,eol,start
set backup
set backupdir=~/.vim/backup
set commentstring=--%s
set directory=~/.vim/tmp
set errorformat=%*[^\"]\"%f\"%*\\D%l:\ %m,\"%f\"%*\\D%l:\ %m,%-G%f:%l:\ (Each\ undeclared\ identifier\ is\ reported\ only\ once,%-G%f:%l:\ for\ each\ function\ it\ appears\ in.),%-GIn\ file\ included\ from\ %f:%l:%c,%-GIn\ file\ included\ from\ %f:%l,%-Gfrom\ %f:%l:%c,%-Gfrom\ %f:%l,%f:%l:%c:%m,%f(%l):%m,%f:%l:%m,\"%f\"\\,\ line\ %l%*\\D%c%*[^\ ]\ %m,%D%*\\a[%*\\d]:\ Entering\ directory\ `%f',%X%*\\a[%*\\d]:\ Leaving\ directory\ `%f',%D%*\\a:\ Entering\ directory\ `%f',%X%*\\a:\ Leaving\ directory\ `%f',%DMaking\ %*\\a\ in\ %f,%f|%l|\ %m,%A%f:%l:\ %m,%A%f:%l:,%C%\\s%m,%Z
set expandtab
set fileencodings=ucs-bom,utf-8,default,latin1
set helplang=en
set hidden
set history=50
set hlsearch
set ignorecase
set incsearch
set nojoinspaces
set lazyredraw
set modelines=0
set path=.,/usr/include,,,**
set ruler
set runtimepath=~/.vim,~/.vim/bundle/dustjs,~/.vim/bundle/vim2hs,/usr/share/vim/vimfiles,/usr/share/vim/vim73,/usr/share/vim/vimfiles/after,~/.vim/bundle/vim2hs/after,~/.vim/after
set scrolloff=3
set shiftwidth=2
set showcmd
set showmatch
set smartcase
set smartindent
set smarttab
set tabstop=2
set tags=./tags,tags,java.tags
set title
set wildmenu
set wildmode=list:longest,full
set window=0
" vim: set ft=vim :
