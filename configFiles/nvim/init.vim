let g:python2_host_prog = '/usr/bin/python'
let g:python3_host_prog = '/usr/local/bin/python3'

set nocompatible            " Disable compatibility to old-time vi
set showmatch               " Show matching brackets.
set ignorecase              " Do case insensitive matching
set mouse=v                 " middle-click paste with mouse
set hlsearch                " highlight search results
set tabstop=2               " number of columns occupied by a tab character
set softtabstop=2           " see multiple spaces as tabstops so <BS> does the right thing
set expandtab               " converts tabs to white space
set shiftwidth=2            " width for autoindents
set autoindent              " indent a new line the same amount as the line just typed
set number                  " add line numbers
set wildmode=longest,list   " get bash-like tab completions
set cc=80                   " set an 80 column border for good coding style
set cursorline              " and why not highlight the current line as well
set encoding=utf-8
set inccommand=nosplit      " interactive :s//
set hidden                  " allow non-saved buffers to be hidden
set incsearch               " highlight search before enter
set autoread                " actively reload changed buffers
set wildignore+=**/temp/**,.git,.node_modules/**,**/target/**,**/out/**,**/lib/**,**/compiled/**
set wildmenu
set path=.,**               " allow :find to recurse

call plug#begin('~/.local/share/nvim/plugged')
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
Plug 'majutsushi/tagbar'
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }

" Language server
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
Plug 'reasonml-editor/vim-reason-plus'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'dracula/vim', { 'as': 'dracula' }

call plug#end()

set t_Co=256
colorscheme zenburn
let g:airline_powerline_fonts = 1
let g:airline_skip_empty_sections = 2

" Markdown
let g:tagbar_type_markdown = {
    \ 'ctagstype' : 'markdown',
    \ 'kinds' : [
        \ 'h:Heading_L1',
        \ 'i:Heading_L2',
        \ 'k:Heading_L3'
    \ ]
\ }

let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
    \ 'javascript': ['javascript-typescript-stdio'],
    \ 'javascript.jsx': ['javascript-typescript-stdio'],
    \ 'reason': ['ocaml-language-server', '--stdio'],
    \ 'ocaml': ['ocaml-language-server', '--stdio'],
    \ }

"""""
" Thanks
" https://stackoverflow.com/questions/1676632/whats-a-quick-way-to-comment-uncomment-lines-in-vim
" Commenting blocks of code.
let s:comment_map = { 
    \   "c": '\/\/',
    \   "cpp": '\/\/',
    \   "go": '\/\/',
    \   "java": '\/\/',
    \   "javascript": '\/\/',
    \   "lua": '--',
    \   "scala": '\/\/',
    \   "php": '\/\/',
    \   "python": '#',
    \   "ruby": '#',
    \   "rust": '\/\/',
    \   "sh": '#',
    \   "desktop": '#',
    \   "fstab": '#',
    \   "conf": '#',
    \   "profile": '#',
    \   "bashrc": '#',
    \   "bash_profile": '#',
    \   "mail": '>',
    \   "eml": '>',
    \   "bat": 'REM',
    \   "ahk": ';',
    \   "vim": '"',
    \   "tex": '%',
    \ }

function! ToggleComment()
    if has_key(s:comment_map, &filetype)
        let comment_leader = s:comment_map[&filetype]
        if getline('.') =~ "^\\s*" . comment_leader . " " 
            " Uncomment the line
            execute "silent s/^\\(\\s*\\)" . comment_leader . " /\\1/"
        else 
            if getline('.') =~ "^\\s*" . comment_leader
                " Uncomment the line
                execute "silent s/^\\(\\s*\\)" . comment_leader . "/\\1/"
            else
                " Comment the line
                execute "silent s/^\\(\\s*\\)/\\1" . comment_leader . " /"
            end
        end
    else
        echo "No comment leader found for filetype"
    end
endfunction

"""""
" ALE
let g:ale_completion_enabled = 1
let g:airline#extensions#ale#enabled = 1

"""""
" Use the awesomeness of rg
if executable('rg')
  set grepprg=rg\ --no-heading\ --vimgrep\ --smart-case
  set grepformat=%f:%l:%c:%m
endif
" also don't show me the intermediate grep results, open the cwindow
" immediately
" CAN'T GET THIS TO WORK - always pauses and "Press Enter"
" augroup quickfix
    " autocmd!
    " autocmd QuickFixCmdPost *grep* <cr>
    " autocmd QuickFixCmdPost *grep* cwindow
" augroup END
command! -nargs=+ MyGrep execute 'silent grep! <args>' | copen 30

"""""
" keybindings
:imap jj <Esc> " get out of here!
:imap jk <Esc> " get out of here!
:imap fd <Esc> " get out of here!

:noremap [b :bprev<cr>
:noremap ]b :bnext<cr>
" Leader mappings
let mapleader = ' '
" top level SPC mappings
nnoremap <leader><leader> :   " save the pain of SHIFT ;
nnoremap <leader><CR> :nohl<cr> " remove highlighting
vnoremap <leader><CR> :nohl<cr>
nnoremap <leader>/ :MyGrep      " don't nag for "Press Enter"
nnoremap <leader>; :call ToggleComment()<cr> " commenting FTW
vnoremap <leader>; :call ToggleComment()<cr> " commenting FTW

:nnoremap <leader>E :e $MYVIMRC<cr>
:nnoremap <leader>R :source %<cr>:PlugInstall<cr>:PlugClean<cr>

:nnoremap <leader>o :BTags<cr>
