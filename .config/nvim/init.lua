-- vim: ts=2 sw=2

vim.cmd([[
"
" Plugin config
"

call plug#begin()

Plug 'vim-scripts/closetag.vim'
Plug 'tmhedberg/matchit'
Plug 'hynek/vim-python-pep8-indent'
Plug 'vim-scripts/bufkill.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'justinmk/vim-syntax-extra'
Plug 'bronson/vim-trailing-whitespace'
Plug 'henrybw/vim-colors-aurora'

Plug 'neovim/nvim-lspconfig'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make' }

Plug 'sindrets/diffview.nvim'
Plug 'NeogitOrg/neogit'

Plug 'FabijanZulj/blame.nvim'

call plug#end()

" Load custom mappings for bufkill
let g:BufKillCreateMappings = 1

"
" Theming
"

syntax on
silent! colorscheme aurora

set nonumber
set cmdheight=1
set nohlsearch
set noincsearch
set laststatus=2
set ruler

" Make cursorline only show up on the currently focused window
set cursorline
augroup cursorline
    autocmd!
    autocmd WinEnter,BufEnter * setlocal cursorline
    autocmd WinLeave,BufLeave * setlocal nocursorline
augroup END

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

" Some special magic to get full 256 colors working in terminals
set t_ut=
set t_Co=256

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

" Spacemacs ruined me
nnoremap <leader>fs :w<CR>
nnoremap <leader>fed :e $MYVIMRC<CR>

" Shortcuts for cycling buffers
nnoremap gB :bp<CR>
nnoremap gb :bn<CR>

" Cycle between last used buffers
nnoremap <leader>` :b#<CR>

" Allows Ctrl-C to be always used in place of Esc (normally, Visual Mode
" doesn't allow you to exit with Ctrl-C, for example).
inoremap <C-c> <Esc>

" Open tag in a vertical split (Ctrl-W ] opens in horizontal split).
nnoremap <C-w>\ <C-w>v<C-]>

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

" Versions of cc and o that don't exit normal mode
nmap <leader>cc cc<ESC>
nmap <leader>o o<ESC>
nmap <leader>O O<ESC>

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

" Fix weird 223-char terminal limit to be unlimited
if has('mouse_sgr')
    set ttymouse=sgr
endif

" Disable SQL omnicompletion because it makes Esc super slow
let g:omni_sql_no_default_maps = 1
let g:ftplugin_sql_omni_key = '<Plug>DisableSqlOmni'

" Sudo save shortcut.
cmap w!! w !sudo tee > /dev/null %

nmap <leader><CR> :FixWhitespace<CR>

" Telescope
nnoremap <C-p> :Telescope find_files<CR>
nnoremap <Leader>/ :Telescope live_grep<CR>
nnoremap <Leader>s :Telescope current_buffer_fuzzy_find<CR>
nnoremap <Leader>b :Telescope buffers<CR>
nnoremap <Leader>o :Telescope vim_options<CR>

" Rough port of cscope-maps to Telescope's LSP pickers
nnoremap <C-\>s :Telescope lsp_references<CR>
nnoremap <C-\>c :Telescope lsp_incoming_calls<CR>
nnoremap <C-\>d :Telescope lsp_outgoing_calls<CR>
nnoremap <C-\>g :Telescope lsp_definitions<CR>
" ...plus some features that cscope doesn't have
nnoremap <C-\>i :Telescope lsp_implementations<CR>
nnoremap <C-\>t :Telescope lsp_type_definitions<CR>

" Neogit
nnoremap <Leader>gs :Neogit kind=auto<CR>
nnoremap <Leader>gl :Neogit log kind=auto<CR>
nnoremap <Leader>gz :Neogit stash kind=auto<CR>
nnoremap <Leader>gb :Neogit branch kind=auto<CR>

" blame.nvim
nnoremap <Leader>gb :BlameToggle<CR>

"
" Formatting
"

" The C file plugin resets whatever formatoptions we specify here, so we need
" to set this to trigger on buffer load events instead.
augroup cformatopt
    autocmd!
    autocmd BufNewFile,BufRead * setlocal formatoptions=crjql
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
set cinoptions+=g0  " C++ scope labels not indented
set cinoptions+=N-s " Don't indent namespace blocks

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
" Paste settings
"

" Use + register whenever possible
if has('unnamedplus')
    set clipboard=unnamedplus
else
    set clipboard=unnamed
endif

"
" Miscellaneous settings
"

set nocompatible
set history=100

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
]])

require('lspconfig').gopls.setup {
  cmd = { 'gopls' },
}

-- https://github.com/golang/tools/blob/master/gopls/doc/vim.md#neovim-imports
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = "*.go",
  callback = function()
    local params = vim.lsp.util.make_range_params()
    params.context = {only = {"source.organizeImports"}}
    -- buf_request_sync defaults to a 1000ms timeout. Depending on your
    -- machine and codebase, you may want longer. Add an additional
    -- argument after params if you find that you have to write the file
    -- twice for changes to be saved.
    -- E.g., vim.lsp.buf_request_sync(0, "textDocument/codeAction", params, 3000)
    local result = vim.lsp.buf_request_sync(0, "textDocument/codeAction", params)
    for cid, res in pairs(result or {}) do
      for _, r in pairs(res.result or {}) do
        if r.edit then
          local enc = (vim.lsp.get_client_by_id(cid) or {}).offset_encoding or "utf-8"
          vim.lsp.util.apply_workspace_edit(r.edit, enc)
        end
      end
    end
    vim.lsp.buf.format({async = false})
  end
})

require('nvim-treesitter.configs').setup {
  ensure_installed = { "go" },
  sync_install = false,
  auto_install = false,
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
}

require('neogit').setup {}
require('blame').setup {}
