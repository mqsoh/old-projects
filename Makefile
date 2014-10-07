# This file was generated from Makefile.nw using something like the following
# command.
#
#     notangle -t4 Makefile.nw > Makefile
SHELL := /bin/bash
help:
	@echo tangle: Compiles all noweb files to source code.
	@echo weave: Compiles all .nw files into docs/.
	@echo tangle-watch: Watches for changes to .nw files and tangles them.
	@echo weave-watch: Watches for changes to .nw files and weaves them.
	@echo watch: Watches for changes to .nw files and both tangles and weaves them.
tangle:
	@for file in $$(find . -name '*.nw'); do \
		echo "$$(date +%H:%M:%S) tangle $$file $${file%.nw}"; \
		notangle -t4 $$file > $${file%.nw}; \
	done
weave:
	@for file in $$(find . -name '*.nw'); do \
		mkdir -p docs/$$(dirname $$file); \
		echo "$$(date +%H:%M:%S) weave $$file docs/$$file.html"; \
		cat doc_top.html | sed -e "s|{file}|$$file|g" > docs/$$file.html; \
		noweave -html -n -filter l2h -index $$file >> docs/$$file.html; \
		cat doc_bottom.html >> docs/$$file.html; \
	done
tangle-watch: tangle
	@inotifywait -mr --format '%w%f' -e close_write ./ | while read file; do \
		case $$file in \
			*.nw) \
	echo "$$(date +%H:%M:%S) tangle $$file $${file%.nw}"; \
	notangle -t4 $$file > $${file%.nw}; \
			;; \
		esac \
	done
watch-weave: weave
	@inotifywait -mr --format '%w%f' -e close_write ./ | while read file; do \
		case $$file in \
			*.nw) \
	mkdir -p docs/$$(dirname $$file); \
	echo "$$(date +%H:%M:%S) weave $$file docs/$$file.html"; \
	cat doc_top.html | sed -e "s|{file}|$$file|g" > docs/$$file.html; \
	noweave -html -n -filter l2h -index $$file >> docs/$$file.html; \
	cat doc_bottom.html >> docs/$$file.html; \
			;; \
		esac \
	done
watch: tangle weave
	@inotifywait -mr --format '%w%f' -e close_write ./ | while read file; do \
		case $$file in \
			*.nw) \
	echo "$$(date +%H:%M:%S) tangle $$file $${file%.nw}"; \
	notangle -t4 $$file > $${file%.nw}; \
	mkdir -p docs/$$(dirname $$file); \
	echo "$$(date +%H:%M:%S) weave $$file docs/$$file.html"; \
	cat doc_top.html | sed -e "s|{file}|$$file|g" > docs/$$file.html; \
	noweave -html -n -filter l2h -index $$file >> docs/$$file.html; \
	cat doc_bottom.html >> docs/$$file.html; \
			;; \
		esac \
	done
