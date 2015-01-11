"
" Plugin config
"

" Pathogen must be initialized immediately in order to work properly
filetype off
execute pathogen#infect()
execute pathogen#helptags()

" Pathogen only re-enables filetype detection; it won't re-enable plugin/indent
filetype plugin indent on

" Enable plugins that need to be manually run
source $HOME/.vim/bundle/closetag/closetag.vim
source $HOME/.vim/bundle/matchit/plugin/matchit.vim

" For the CtrlP plugin
let g:ctrlp_map = '<C-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg)$',
  \ 'file': '\v\.(so|swp|o)$',
  \ }

" For the Tagbar plugin
nmap <leader>t :TagbarToggle<CR>

"
" Theming
"

colorscheme slate

" For some reason, the syntax highlighting overwrites the definition for
" certain highlight groups, so we specify them after we've turned on
" syntax highlighting.
syntax on
highlight Comment ctermfg=14
highlight Identifier ctermfg=14
highlight Statement ctermfg=11
highlight PreProc ctermfg=81
highlight Type ctermfg=121
highlight Function ctermfg=33
highlight Include ctermfg=166
highlight Operator ctermfg=64
highlight Structure ctermfg=10
highlight Constant ctermfg=37
highlight clear Special
highlight link Special String
highlight Error ctermfg=red ctermbg=0
highlight StatusLine term=reverse ctermfg=36 ctermbg=16 gui=bold
highlight ColorColumn term=reverse ctermbg=36
highlight clear CursorLine
highlight CursorLine term=reverse ctermbg=0

set number
set cmdheight=1
set nohlsearch
set laststatus=2

" Make cursorline only show up on the currently focused window
set cursorline
autocmd WinEnter * setlocal cursorline
autocmd WinLeave * setlocal nocursorline

" Inspired by http://stackoverflow.com/a/10416234 and Powerline
set statusline=
set statusline+=[%n]                                   " Buffer number
set statusline+=\ %{&paste?'--PASTE--':''}\ >\         " Paste mode indicator
set statusline+=\ %<%F                                 " File+path
set statusline+=\ %m%r%w\                              " Modified? Readonly?
set statusline+=%=                                     " Separator
set statusline+=\ %y\ <                                " FileType
set statusline+=\ %{''.(&fenc!=''?&fenc:&enc).''}      " Encoding
set statusline+=\ %{(&bomb?\",BOM\":\"\")}\            " Encoding2
set statusline+=\ [%{&ff}]\ <                          " FileFormat (dos/unix..)
set statusline+=\ %p%\%\ :                             " Percentage
set statusline+=\ line\ %l/%L\ :                       " Row/total
set statusline+=\ col\ %c\                             " Column

" I want splits to open up the way I read: left-to-right, top-to-bottom
set splitright
set splitbelow

" Some special magic to get full 256 colors working in terminals
set t_ut=
set t_Co=256

"
" Keys/Controls
"

" Shortcuts for cycling buffers
nnoremap <leader>[ :bp<CR>
nnoremap <leader>] :bn<CR>

" Cycle between last used buffers
nnoremap <leader>` :b#<CR>

" Allows Ctrl-C to be always used in place of Esc (normally, Visual Mode
" doesn't allow you to exit with Ctrl-C, for example).
inoremap <C-c> <Esc>

" Open tag in a vertical split (Ctrl-W ] opens in horizontal split)
map <C-w>\ :vsp<CR>:exec("tag ".expand("<cword>"))<CR>

" Vertical split version of Ctrl-W F (because I never use vim tabs anyway)
map <C-w>gf :vertical wincmd f<CR>

" Map Y do be analog of D
map Y y$

" Versions of cc and o that don't exit normal mode
nmap <leader>cc cc<ESC>
nmap <leader>o o<ESC>
nmap <leader>O O<ESC>

" Toggle autocomment mode (normally I don't like it, but it's really helpful
" when writing multiline C block comments, for example)
nmap <leader>s <ESC>:set formatoptions+=ro formatoptions?<CR>
nmap <leader>d <ESC>:set formatoptions-=ro formatoptions?<CR>

" Toggle paste mode
map zp :set invpaste paste?<CR>

" Toggle search pattern hilighting and display the value
map <leader>h :set hlsearch! hlsearch?<CR>

" Emulate some IDE-style editing behavior with backspace and shift-tab
set backspace=indent,eol,start
imap <S-Tab> <Esc><<i
imap <C-BS> <C-w>

" Fix PgUp/PgDn
map <silent> <PageUp> 1000<C-U>
map <silent> <PageDown> 1000<C-D>
imap <silent> <PageUp> <C-O>1000<C-U>
imap <silent> <PageDown> <C-O>1000<C-D>

" Often I hold shift too long when issuing these commands
" (adapted from http://www2.mathematik.hu-berlin.de/~altmeyrx/BZQ/vimrc)
command! Q q
command! Qall qall
command! W w
command! Wall wall
command! WQ wq
command! E e
command! Bd bd

" Never use Ex mode -- I never *mean* to press it
nnoremap Q <ESC>

set mouse=a

"
" Formatting
"

" The C file plugin resets whatever formatoptions we specify here, so we need
" to set this to trigger on buffer load events instead.
autocmd BufNewFile,BufRead * setlocal formatoptions=cql
au BufRead,BufNewFile *.h set filetype=c

set textwidth=80
"if exists('+colorcolumn')
"    set colorcolumn=+1
"endif

" Tab settings
set shiftwidth=4
set tabstop=4
set expandtab
set smarttab  " Use shiftwidth for tabs instead of tab stops

" Indentation
set noautoindent
set nosmartindent
set cindent

" Indents lines inside parens to the column where the starting paren is
set cinoptions=(0,w0,W4

" Indent statements in cases relative to the case labels (this is confusing;
" see the vim documentation for the lN setting for an example).
set cinoptions+=l1

set cinoptions+=jN,JN  " Fixes for Java/JavaScript indentation

" Settings for coding styles that prefer certain constructs be unindented
set cinoptions+=:0  " Indent case labels
set cinoptions+=t0  " Don't indent function return type declaration

" Normally I don't want line wrapping, so disable it for everything but plain
" text and files with no file type (which are probably also plain text).
set wrap
autocmd FileType * if !empty(&filetype) && &filetype != "text" | set nowrap

set linebreak
set nostartofline
set display+=lastline

"
" Miscellaneous settings
"

set nocompatible
set history=100
set printoptions=syntax:y,wrap:y

" Don't keep around the backup file on success
set writebackup
set nobackup

" Enable 'wild menus' for tags
set wildmenu
set showfulltag

" Ignore case in searches for all-lowercase searches, but respect case if
" there is an uppercase character.
set ignorecase
set smartcase

set hidden  " Keep around modified buffers without having to save them
set confirm  " Ask instead of autofailing when doing a destructive action
