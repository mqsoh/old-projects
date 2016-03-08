# LFE Watcher

To watch your LFE files (must be in `src/` and will output to `ebin/`):

    docker run --interactive --tty --rm --volume $(pwd):/workdir mqsoh/lfe-watcher

To load a shell that will automatically reload compiled modules:

    docker run --interactive --tty --rm --volume $(pwd):/workdir mqsoh/lfe-watcher shell

To manage the container with [Docker Compose][]:

    lfe:
      image: mqsoh/lfe-watcher
      volumes:
        - .:/workdir

And get to the shell with:

    docker-compose run lfe shell



# Potential Problems

- You can only run one shell. (Caused by the `-sname` flag.)
- There's no way to augment the code path using command line arguments (`-pa
  ebin` is given in the image). However, I think you can use the [ERL_LIBS][]
  environment variable.



# Rationale

I want to have an LFE container watching `src/` and compiling to `ebin/`. I
would also like a shell that automatically reloads recompiled modules.

This is not based on [the official LFE image][] because I tried to install
`inotify-tools` in the [the official LFE image][], but there was this error:

    root@3991ece0bad8:/workdir# apt-get install inotify-tools
    Reading package lists... Done
    Building dependency treeₓₓₓₓₓₓₓ
    Reading state information... Done
    You might want to run 'apt-get -f install' to correct these:
    The following packages have unmet dependencies:
     esl-erlang : Depends: libwxbase3.0-0 but it is not going to be installed or
                           libwxbase2.8-0 but it is not installable
                  Depends: libwxgtk3.0-0 but it is not going to be installed or
                           libwxgtk2.8-0 but it is not installable
                  Recommends: erlang-mode but it is not going to be installed
     inotify-tools : Depends: libinotifytools0 (>= 3.11) but it is not going to be installed
    E: Unmet dependencies. Try 'apt-get -f install' with no packages (or specify a solution).

To fix it (by running `apt-get -f install`) I would need to install a bunch of
packages related to the Erlang installation. It turns out that the official LFE
image isn't using [the official Erlang image][]. (I think the Erlang image is
new.) Both are based on Debian Jessie, so it will be easy to build my own LFE
image for the time being.

###### file:Dockerfile

```{name="file:Dockerfile"}
# This file was generated from the README.md in the GitHub repository.
FROM erlang:18

<<Install LFE.>>
<<Install watcher script.>>
<<Reload code.>>
```



# Install LFE.

The Erlang image already has git installed. I'm installing LFE to
`/usr/local/lib/erlang/lib` to avoid any path issues. Initially I tried to
compile it in `/lfe` but the `lfec` escript couldn't find the `lfe_comp`
module.

###### Install LFE.

```{.Dockerfile name="Install LFE."}
RUN cd /usr/local/lib/erlang/lib \
    && git clone https://github.com/rvirding/lfe.git \
    && cd /usr/local/lib/erlang/lib/lfe \
    && git checkout v0.10.1 \
    && make compile install
```



# Watching and Compiling Source Code

I need to

- install inotify-tools,
- copy the script into the image, and
- set up the workdir and entrypoint.

###### Install watcher script.

```{.Dockerfile name="Install watcher script."}
RUN apt-get update \
    && apt-get install -y inotify-tools \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

COPY files/lfe-watch.sh /usr/local/bin/lfe-watch.sh
RUN chmod +x /usr/local/bin/lfe-watch.sh

WORKDIR /workdir
CMD ["lfe-watch.sh"]
```

The `WORKDIR /workdir` is a convention I've been using in my Dockerfiles. I
always map the project directory there, so with `docker run` I use `--volume
$(pwd):/workdir` and in a Docker Compose file:

    mycontainer:
      volumes:
        - .:/workdir

I need to make that script. How about this?

###### file:files/lfe-watch.sh

```{.bash name="file:files/lfe-watch.sh"}
#!/bin/bash
# This file was generated from the README.md.

lfec -o ebin src/*

inotifywait --monitor --event close_write --format '%w%f' src | while read file; do
    case $file in
        *.lfe)
            lfec -o ebin $file &
            ;;
    esac
done
```

If the command isn't backgrounded, it won't process more than one event. If
multiple files are written at once, only the first in the group is processed. I
didn't do any intense debugging, so I can't say why.



# Reloading Code in the Shell

[Erlang supports shell configuration in a `.erlang` file.][] Here is [an
example of sending a message between processes on the same node][]. Here's an
abbreviated example of that.

    erl -sname one
    > register(my_process, spawn(fun () -> receive Any -> io:format("Got: ~w~n", [Any]) end end)).
    true

And from the bash prompt:

    $ erl -noshell -sname two -eval '{my_process, list_to_atom("one@" ++ net_adm:localhost())} ! from_the_shell' -s init stop

In the shell, you'll see:

    Got: from_the_shell

The `-sname <name>` puts Erlang in distributed mode and lets me send a message.
So, I think all I need to do is pick names for the shell and the process that
will reload files. Then I can use `inotifywait` to send updates to that
process.

I can put the Erlang code to do this in a `.erlang` and it'll be automatically
run any time a shell is started.

###### file:files/dot_erlang

```{.erlang name="file:files/dot_erlang"}
% This file was generated from the README.md.
case init:get_argument(start_lfe_watcher_reloader) of
    {ok, _} ->
        io:format("~nStarting beam reloader.~n"),
        register(lfe_watcher_reloader, spawn(fun F() ->
            receive
                Module_name ->
                    io:format("Reloading: ~s~n", [Module_name]),
                    Module = list_to_atom(Module_name),
                    code:purge(Module),
                    code:load_file(Module)
            end,
            F()
        end));
    _ -> ok
end.
```

When I start the shell I can start a background process to watch the files and
send messages to `lfe_watcher_reloader`.

The `erl` runtime can take arbitrary flags from the command line. I can check
them with `init:get_argument/1`. By wrapping the beam reloader in an explicit
flag I can prevent myself from unnecessarily clutter random `erl` commands.

###### file:files/shell

```{.bash name="file:files/shell"}
#!/bin/bash
# This file was generated from the README.md.
inotifywait --monitor --event close_write --format '%w%f' ebin | while read file; do
    case $file in
        *.beam)
            module_name=$(basename "$file" .beam)
            erlang_code="{lfe_watcher_reloader, list_to_atom(\"lfe_watcher_shell@\" ++ net_adm:localhost())} ! \"$module_name\""

            erl -noshell -sname lfe_watcher_inotify_$RANDOM -eval "$erlang_code" -s init stop &
            ;;
    esac
done &
lfe -start_lfe_watcher_reloader -sname lfe_watcher_shell -pa ebin
```

Again, the command needs to be backgrounded because it seems to block the
processing of other files.

Finally, I just need to put it in the image.

###### Reload code.

```{.Dockerfile name="Reload code."}
COPY files/dot_erlang /root/.erlang
COPY files/shell /usr/local/bin/shell
RUN chmod +x /usr/local/bin/shell
```



[the official LFE image]: https://hub.docker.com/r/lfex/lfe/
[the official Erlang image]: https://hub.docker.com/_/erlang/
[Erlang supports shell configuration in a `.erlang` file.]: http://erlang.org/doc/man/erl.html#id179026
[an example of sending a message between processes on the same node]: http://stackoverflow.com/a/16913797/8710
[Docker Compose]: https://docs.docker.com/compose/overview/
[ERL_LIBS]: http://erlang.org/doc/man/code.html
