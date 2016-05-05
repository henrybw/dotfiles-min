"
" Plugin config
"

call plug#begin('~/.config/nvim/plugged')

Plug 'vim-scripts/closetag.vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tmhedberg/matchit'
Plug 'hynek/vim-python-pep8-indent'
Plug 'majutsushi/tagbar'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-scripts/bufkill.vim'
Plug 'tpope/vim-fugitive'
Plug 'justinmk/vim-syntax-extra'
Plug 'bronson/vim-trailing-whitespace'
Plug 'henrybw/vim-colors-aurora'
Plug 'guns/xterm-color-table.vim'
Plug '$HOME/.config/nvim/custom/cscope-maps'

call plug#end()

" For the CtrlP plugin
let g:ctrlp_map = '<C-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg)$',
  \ 'file': '\v\.(pyc|so|swp|o)$',
  \ }
let g:ctrlp_user_command = [
  \ '.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard'
  \ ]

" Load custom mappings for bufkill
let g:BufKillCreateMappings = 1

"
" Theming
"

syntax on
silent! colorscheme aurora

set number
set cmdheight=1
set nohlsearch
set noincsearch
set laststatus=2

" Make cursorline only show up on the currently focused window
set cursorline
augroup cursorline
    autocmd!
    autocmd WinEnter,BufEnter * setlocal cursorline
    autocmd WinLeave,BufLeave * setlocal nocursorline
augroup END

" Airline-specific config and tweaks
let g:airline_theme = 'powerlineish'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'
let g:airline#extensions#tabline#show_buffers = 0
let g:airline_detect_modified = 0  " This can get reaaaaaally slow
let g:airline_extensions = ['ctrlp', 'netrw', 'tabline', 'tagbar', 'whitespace']

" For the trailing whitespace plugin
hi ExtraWhitespace ctermbg=red guibg=red

" I usually use vertical splits to follow tags / call chains, so I want them to
" progress left-to-right. However, I tend to use horizontal splits to examine
" definitions, for which I'd like to have them open upward (so the opened buffer
" with the definition appears above the current buffer I'm in).
set splitright
set nosplitbelow

" Make diff windows open as vertical splits by default
set diffopt=filler,vertical

" This seems to have a net effect of batching screen updates, resulting in very
" large redraw events occuring when, for example, switching tmux windows. Over a
" network connection, this can make rendering large vim windows very laggy. With
" this off, however, rendering speed seems to stay relatively sane in most cases.
set nolazyredraw

"
" Keys/Controls
"

" Spacebar >>> \ as a leader key
let mapleader="\<Space>"

" Shortcuts for cycling buffers
nnoremap gB :bp<CR>
nnoremap gb :bn<CR>

" Cycle between last used buffers
nnoremap <leader>` :b#<CR>

" Allows Ctrl-C to be always used in place of Esc (normally, Visual Mode
" doesn't allow you to exit with Ctrl-C, for example).
inoremap <C-c> <Esc>

" Open tag in a vertical split (Ctrl-W ] opens in horizontal split).
" NOTE: The visual mode version of this clobbers the 'u' register, chosen
" because using 'u' in conjunction with y and " seems inconvenient.
nnoremap <C-w>\ :exec("vert stag " . expand("<cword>"))<CR>
vnoremap <C-w>\ "uy :exec("vert stag " . getreg('u'))<CR>

" Vertical split version of Ctrl-W F (because I never use vim tabs anyway)
map <C-w>gf :vertical wincmd f<CR>

fun! g:ScrollToPercent(percent)
    let movelines=winheight(0)*(50-a:percent)/100
    echo movelines
    if movelines<0
        let motion='k'
        let rmotion='j'
        let movelines=-movelines
    elseif movelines>0
        let motion='j'
        let rmotion='k'
    else
        return 0
    endif
    if has('float') && type(movelines)==type(0.0)
        let movelines=float2nr(movelines)
    endif
    execute 'normal! zz'.movelines.motion.'zz'.movelines.rmotion
endfun

" I find just having zt and zb limiting; oftentimes, I'll want to put the
" current piece of code I'm looking at in either top or bottom 'quadrant' of the
" screen. Hence, the scrolling to 25% and 75% commands.
map zr :call g:ScrollToPercent(25)<CR>
map zv :call g:ScrollToPercent(75)<CR>

" Map Y do be analog of D
map Y y$

" I never use K to lookup 'keyword' help, so have it perform the reverse of J,
" i.e. split lines.
nnoremap K i<CR><ESC>

" Versions of cc and o that don't exit normal mode
nmap <leader>cc cc<ESC>
nmap <leader>o o<ESC>
nmap <leader>O O<ESC>

" Toggle autocomment mode, for those times when I don't want enter to continue a
" comment.
nmap <leader>s <ESC>:set formatoptions+=r formatoptions?<CR>
nmap <leader>d <ESC>:set formatoptions-=r formatoptions?<CR>

" For quicker quickfixing
nmap <leader>n <ESC>:cn<CR>
nmap <leader>p <ESC>:cp<CR>

" For when you match a tag more than once
nmap <leader>] <ESC>:tn<CR>
nmap <leader>[ <ESC>:tp<CR>

" Toggle paste mode
map zp :set invpaste paste?<CR>

" Toggle search pattern hilighting and display the value
map <leader>h :set hlsearch! hlsearch?<CR>

" Toggle wrapping
map <leader>w :set wrap! wrap?<CR>

" Toggle column guide
fun! g:ToggleColorColumn()
    if empty(&colorcolumn)
        set colorcolumn=+1
    else
        set colorcolumn=
    endif
endfun
nmap <leader>\ :call g:ToggleColorColumn()<CR>

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

" Disable SQL omnicompletion because it makes Esc super slow
let g:omni_sql_no_default_maps = 1
let g:ftplugin_sql_omni_key = '<Plug>DisableSqlOmni'

" For the Tagbar plugin
nmap <leader>t :TagbarToggle<CR>

" Sudo save shortcut.
cmap w!! w !sudo tee > /dev/null %

nmap <leader><CR> :FixWhitespace<CR>

"
" Formatting
"

" The C file plugin resets whatever formatoptions we specify here, so we need
" to set this to trigger on buffer load events instead.
augroup cformatopt
    autocmd!
    if v:version < 703
        autocmd BufNewFile,BufRead * setlocal formatoptions=cql
    else
        autocmd BufNewFile,BufRead * setlocal formatoptions=crjql
    endif
    autocmd BufRead,BufNewFile *.h set filetype=c
augroup END

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
set cinoptions+=g2  " C++ scope labels indented by half shiftwidth (Google inspired)

set linebreak
set nostartofline
set display+=lastline

" Better wrapping for bulleted lists
if exists('+breakindent')
    set breakindent
    set breakindentopt=shift:2
endif

" Normally I don't want line wrapping, so disable it for everything but plain
" text and files with no file type (which are probably also plain text). Of
" course, since vimscript is a pile of shit, we have to do this with a custom
" function instead of embedding a conditional in the autocmd...
fun! g:SetFiletypeConditionalConfig()
    if empty(&filetype) || &filetype == "text"
        set wrap
        set textwidth=0
    else
        set nowrap
        set textwidth=80
    endif
endfun

augroup filetype
    autocmd!
    autocmd FileType * call g:SetFiletypeConditionalConfig()
augroup END

"
" Miscellaneous settings
"

set history=100
set printoptions=syntax:y,wrap:y

" Don't keep around backups or swap files
set writebackup
set nobackup
set noswapfile

" Enable 'wild menus' for tags
set wildmenu
set showfulltag

" Ignore case in searches for all-lowercase searches, but respect case if
" there is an uppercase character.
set ignorecase
set smartcase

set hidden  " Keep around modified buffers without having to save them
set confirm  " Ask instead of autofailing when doing a destructive action
