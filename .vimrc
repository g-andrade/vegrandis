"let g:syntastic_cpp_compiler = "g++"
let g:syntastic_cpp_compiler_options = "-std=c++11 -Wall -Wextra -Wpedantic"
let g:syntastic_cpp_check_header = 0
let g:syntastic_cpp_include_dirs = [ $ERL_INCLUDE ]
auto FileType cpp setlocal expandtab softtabstop=4 shiftwidth=4
auto FileType erlang setlocal expandtab softtabstop=4 shiftwidth=4
