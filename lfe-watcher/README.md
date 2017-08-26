# LFE Watcher

There is only one supported workflow and that is to load an LFE shell that will
compile `src/*.lfe` files when they are changed and output them to `ebin`. The
`ebin/*.beam` files generated will automatically be reloaded in the shell.

    docker run --interactive --tty --rm --volume $(pwd):/workdir mqsoh/lfe-watcher

Previously I'd written it so that you could run a container that would *only*
compile `src/*.lfe` files and then optionally load up a shell. However, it's
never the case that I develop without a shell open.

You can augment the code path using the [ERL_LIBS][] environment variable, such
as the following in the `docker run` command.

    --env "ERL_LIBS=$(find deps -type d -name ebin)"



# Rationale

I want to make developing with LFE as easy as possible.

This image is not based on [the official LFE image][] because I tried to
install `inotify-tools` in the [the official LFE image][], but there was this
error:

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
<<Install inotify-tools.>>
<<The Bash side.>>
<<The Erlang side.>>
```



# Install LFE.

The Erlang image already has git installed. I'm installing LFE to
`/usr/local/lib/erlang/lib` to avoid any path issues. Initially I tried to
compile it in `/lfe` but the `lfec` escript couldn't find the `lfe_comp`
module. The bug has been fixed, but I might as well put it with core Erlang.

```{.Dockerfile name="Install LFE."}
RUN cd /usr/local/lib/erlang/lib \
    && git clone https://github.com/rvirding/lfe.git \
    && cd /usr/local/lib/erlang/lib/lfe \
    && git checkout v1.0 \
    && make compile install
```



# Install inotify-tools.  
The official Erlang image is based on Debian Jessie; `inotify-tools` is in the
package manager.

```{.Dockerfile name="Install inotify-tools."}
RUN apt-get update \
    && apt-get install -y inotify-tools \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
```

The `apt-get clean` and `rm ...` cleans up any unnecessary `apt` stuff. I saw
this suggested in an article, but I didn't save the link.



# The Bash side.

I'm going to write a Bash script and this is how I'll add it to the Docker
image. The `WORKDIR /workdir` is a convention I like to use in my images; I
always have a `--volume $(pwd):/workdir`.

```{.Dockerfile name="The Bash side."}
COPY files/lfe-watcher.sh /usr/local/bin/lfe-watcher.sh
RUN chmod +x /usr/local/bin/lfe-watcher.sh
WORKDIR /workdir
CMD ["lfe-watcher.sh"]
```

I can use `inotifywait` to be notified of changes in `src` and `ebin`.

```{.bash name="file:files/lfe-watcher.sh"}
#!/bin/bash
# This file was generated from the README.md.

lfec -o ebin src/*

inotifywait --monitor --event close_write --format '%w%f' ebin src | while read file; do
    case $file in
        <<Handle changes.>>
    esac
done &

<<Start the shell.>>
```

I compile the files (`lfec ...`) before I start listening to avoid needing any
bootstrapping of a new project. Also note that the `inotifywait` process is
backgrounded.

### Sending Messages to the Shell

When files are changed I need to send a message to the running shell. I'll
enumerate those processes below, but first I have to be able to send a message
to them! Here is [an example of sending a message between processes on the same
node][].  And here's my abbreviated example with a bash one-liner.

    erl -sname one
    > register(my_process, spawn(fun () -> receive Any -> io:format("Got: ~w~n", [Any]) end end)).
    true

And from the bash prompt:

    $ erl -noshell -sname two -eval '{my_process, list_to_atom("one@" ++ net_adm:localhost())} ! from_the_shell' -s init stop

In the Erlang shell, you'll see:

    Got: from_the_shell

The `-sname <name>` puts Erlang in distributed mode and lets me send a message.
So, all I need to do is pick names for the process that will compile and reload
files. I'll call them `lfe_watcher_lfe_compiler` and
`lfe_watcher_beam_realoader`. The names are long because I want to avoid
colliding with anyone or anything.

Remember, these segments are inside the `case` statement in `lfe-watcher.sh`.

###### Handle changes.

```{name="Handle changes."}
*.lfe)
    erlang_code="{lfe_watcher_lfe_compiler, list_to_atom(\"lfe_watcher@\" ++ net_adm:localhost())} ! \"$file\""
    erl -noshell -sname "lfe_watcher_sh_lfe_$RANDOM" -eval "$erlang_code" -s init stop &
    ;;

*.beam)
    module_name=$(basename "$file" .beam)
    erlang_code="{lfe_watcher_beam_reloader, list_to_atom(\"lfe_watcher@\" ++ net_adm:localhost())} ! \"$module_name\""

    erl -noshell -sname "lfe_watcher_sh_beam_$RANDOM" -eval "$erlang_code" -s init stop &
    ;;
```

The `erl ...` calls need to be backgrounded because, in my experience, it will
block the processing of other events if you don't.

I called the shell `lfe_watcher` above, so I need to call it that when it
starts. Also, I'm going to start these processes in the `.erlang`, but I want
the image to be useful in other ways so I need to use a command line flag to
turn them on.

###### Start the shell.

```{name="Start the shell."}
lfe -lfe_watcher_on -sname lfe_watcher -pa ebin
```



# The Erlang side.

[Erlang supports shell configuration in a `.erlang` file.][] The commands in
this file are run in the shell as if a user had typed them (in the same
context). You can define helper functions, for example.

I need to add this to the Docker image.

```{name="The Erlang side."}
COPY files/dot_erlang /root/.erlang
```

The files itself needs to start the compiler and reloader processes if they're
turned on from the command line.

```{name="file:files/dot_erlang"}
% This file was generated from the README.md.
case init:get_argument(lfe_watcher_on) of
    {ok, _} ->
        io:format("~nStarting the LFE compiler and BEAM reloader.~n"),
        <<Start the LFE compiler.>>
        ,
        <<Start the BEAM reloader.>>
        ;
    _ -> ok
end.
```

The dangling `,` and `;` mean that I can define those code sections in the same
way, without terminating the term.

### Start the LFE compiler.

```{name="Start the LFE compiler."}
register(lfe_watcher_lfe_compiler, spawn(fun F() ->
    receive
        File_name ->
            io:format("Compiling: ~s~n", [File_name]),
            lfe_comp:file(File_name, [report, {outdir, "ebin"}])
    end,
    F()
end))
```

### Start the BEAM reloader.

```{name="Start the BEAM reloader."}
register(lfe_watcher_beam_reloader, spawn(fun F() ->
    receive
        Module_name ->
            io:format("Reloading: ~s~n", [Module_name]),
            Module = list_to_atom(Module_name),
            code:purge(Module),
            code:load_file(Module)
    end,
    F()
end))
```



[the official LFE image]: https://hub.docker.com/r/lfex/lfe/
[the official Erlang image]: https://hub.docker.com/_/erlang/
[Erlang supports shell configuration in a `.erlang` file.]: http://erlang.org/doc/man/erl.html#id179026
[an example of sending a message between processes on the same node]: http://stackoverflow.com/a/16913797/8710
[ERL_LIBS]: http://erlang.org/doc/man/code.html
