"
" Plugin config
"

call plug#begin('~/.config/nvim/plugged')

Plug 'vim-scripts/closetag.vim'
Plug 'brookhong/cscope.vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tmhedberg/matchit'
Plug 'hynek/vim-python-pep8-indent'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-scripts/bufkill.vim'
Plug 'tpope/vim-fugitive'
Plug 'henrybw/vim-colors-aurora'
Plug '$HOME/.config/nvim/custom/highlight-ctypes'

call plug#end()

if has("cscope")
    set csto=0
    set cst
    set nocsverb
    set cscopequickfix=
    " add any database in current directory
    if filereadable("cscope.out")
        cs add cscope.out
    " else add database pointed to by environment
    elseif $CSCOPE_DB != ""
        cs add $CSCOPE_DB
    endif
    set csverb
endif

" To do the first type of search, hit 'CTRL-\', followed by one of the
" cscope search types above (s,g,c,t,e,f,i,d).  The result of your cscope
" search will be displayed in the current window.  You can use CTRL-T to
" go back to where you were before the search.
nmap <C-\>s :cs find s <C-R>=expand("<cword>")<CR><CR>
nmap <C-\>g :cs find g <C-R>=expand("<cword>")<CR><CR>
nmap <C-\>c :cs find c <C-R>=expand("<cword>")<CR><CR>
nmap <C-\>t :cs find t <C-R>=expand("<cword>")<CR><CR>
nmap <C-\>e :cs find e <C-R>=expand("<cword>")<CR><CR>
nmap <C-\>f :cs find f <C-R>=expand("<cfile>")<CR><CR>
nmap <C-\>i :cs find i <C-R>=expand("<cfile>")<CR><CR>
nmap <C-\>d :cs find d <C-R>=expand("<cword>")<CR><CR>

" Using 'CTRL-spacebar' (intepreted as CTRL-@ by vim) then a search type
" makes the vim window split horizontally, with search result displayed in
" the new window.
"
" (Note: earlier versions of vim may not have the :scs command, but it
" can be simulated roughly via:
"    nmap <C-@>s <C-W><C-S> :cs find s <C-R>=expand("<cword>")<CR><CR>
nmap <C-@>s :scs find s <C-R>=expand("<cword>")<CR><CR>
nmap <C-@>g :scs find g <C-R>=expand("<cword>")<CR><CR>
nmap <C-@>c :scs find c <C-R>=expand("<cword>")<CR><CR>
nmap <C-@>t :scs find t <C-R>=expand("<cword>")<CR><CR>
nmap <C-@>e :scs find e <C-R>=expand("<cword>")<CR><CR>
nmap <C-@>f :scs find f <C-R>=expand("<cfile>")<CR><CR>
nmap <C-@>i :scs find i <C-R>=expand("<cfile>")<CR><CR>
nmap <C-@>d :scs find d <C-R>=expand("<cword>")<CR><CR>


" Hitting CTRL-space *twice* before the search type does a vertical
" split instead of a horizontal one (vim 6 and up only)
nmap <C-@><C-@>s :vert scs find s <C-R>=expand("<cword>")<CR><CR>
nmap <C-@><C-@>g :vert scs find g <C-R>=expand("<cword>")<CR><CR>
nmap <C-@><C-@>c :vert scs find c <C-R>=expand("<cword>")<CR><CR>
nmap <C-@><C-@>t :vert scs find t <C-R>=expand("<cword>")<CR><CR>
nmap <C-@><C-@>e :vert scs find e <C-R>=expand("<cword>")<CR><CR>
nmap <C-@><C-@>f :vert scs find f <C-R>=expand("<cfile>")<CR><CR>
nmap <C-@><C-@>i :vert scs find i <C-R>=expand("<cfile>")<CR><CR>
nmap <C-@><C-@>d :vert scs find d <C-R>=expand("<cword>")<CR><CR>

" For the CtrlP plugin
let g:ctrlp_map = '<C-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg)$',
  \ 'file': '\v\.(pyc|so|swp|o)$',
  \ }

" For the Tagbar plugin
nmap <leader>t :TagbarToggle<CR>

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
autocmd WinEnter * setlocal cursorline
autocmd BufEnter * setlocal cursorline
autocmd WinLeave * setlocal nocursorline
autocmd BufLeave * setlocal nocursorline

" Enable airline
let g:airline_theme = 'powerlineish'
let g:airline_powerline_fonts = 1
" But we don't want any extensions
let g:airline_extensions = []

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

" Open tag in a vertical split (Ctrl-W ] opens in horizontal split)
map <C-w>\ :vsp<CR>:exec("tag ".expand("<cword>"))<CR>

" Vertical split version of Ctrl-W F (because I never use vim tabs anyway)
map <C-w>gf :vertical wincmd f<CR>

fun! ScrollToPercent(percent)
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
map zr :call ScrollToPercent(25)<CR>
map zv :call ScrollToPercent(75)<CR>

" Map Y do be analog of D
map Y y$

" I never use K to lookup 'keyword' help, so have it perform the reverse of J,
" i.e. split lines.
nnoremap K i<CR><ESC>

" Versions of cc and o that don't exit normal mode
nmap <leader>cc cc<ESC>
nmap <leader>o o<ESC>
nmap <leader>O O<ESC>

" Toggle autocomment mode (normally I don't like it, but it's really helpful
" when writing multiline C block comments, for example)
nmap <leader>s <ESC>:set formatoptions+=ro formatoptions?<CR>
nmap <leader>d <ESC>:set formatoptions-=ro formatoptions?<CR>

" For quicker quickfixing
nmap <leader>n <ESC>:cn<CR>
nmap <leader>p <ESC>:cp<CR>

" Toggle paste mode
map zp :set invpaste paste?<CR>

" Toggle search pattern hilighting and display the value
map <leader>h :set hlsearch! hlsearch?<CR>

" Toggle wrapping
map <leader>w :set wrap! wrap?<CR>

" Toggle column guide
fun! ToggleColorColumn()
    if empty(&colorcolumn)
        set colorcolumn=+1
    else
        set colorcolumn=
    endif
endfun
nmap <leader>\ :call ToggleColorColumn()<CR>

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

" Sudo save shortcut.
cmap w!! w !sudo tee > /dev/null %

"
" Formatting
"

" The C file plugin resets whatever formatoptions we specify here, so we need
" to set this to trigger on buffer load events instead.
if v:version < 703
    autocmd BufNewFile,BufRead * setlocal formatoptions=cql
else
    autocmd BufNewFile,BufRead * setlocal formatoptions=cjql
endif
au BufRead,BufNewFile *.h set filetype=c

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
fun! SetFiletypeConditionalConfig()
    if empty(&filetype) || &filetype == "text"
        set wrap
        set textwidth=0
    else
        set nowrap
        set textwidth=80
    endif
endfun
autocmd FileType * call SetFiletypeConditionalConfig()

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
