#!/bin/bash
# This file was generated from the README.md.

lfec -o ebin src/*

inotifywait --monitor --event close_write --format '%w%f' ebin src | while read file; do
    case $file in
        *.lfe)
            erlang_code="{lfe_watcher_lfe_compiler, list_to_atom(\"lfe_watcher@\" ++ net_adm:localhost())} ! \"$file\""
            erl -noshell -sname "lfe_watcher_sh_lfe_$RANDOM" -eval "$erlang_code" -s init stop &
            ;;
        
        *.beam)
            module_name=$(basename "$file" .beam)
            erlang_code="{lfe_watcher_beam_reloader, list_to_atom(\"lfe_watcher@\" ++ net_adm:localhost())} ! \"$module_name\""
        
            erl -noshell -sname "lfe_watcher_sh_beam_$RANDOM" -eval "$erlang_code" -s init stop &
            ;;
    esac
done &

lfe -lfe_watcher_on -sname lfe_watcher -pa ebin