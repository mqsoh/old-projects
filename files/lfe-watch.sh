#!/bin/bash
# This file was generated from the README.md.

lfec -o ebin src/*

inotifywait --monitor --event close_write --format '%w%f' src | while read file; do
    case $file in
        *.lfe)
            lfec -o ebin $file
            ;;
    esac
done