# Old Projects

These are projects that I'm no longer interested in.

## 2010

[animan](./animan) was a JavaScript function for animating stuff back in 2010.
I was still mostly a frontend developer at the time. jQuery had a function to
animate CSS properties but its easing function would output subpixels and make
stuff blurry. Also, it seemed like the different CSS properties were animated
independently so that they would start and end at different times. I believe
this was before
[requestAnimationFrame](https://developer.mozilla.org/en-US/docs/Web/API/window/requestAnimationFrame).

## 2011

The [vendetta-plugins](./vendetta-plugins) are a couple plugins I wrote for
[Vendetta Online](https://www.vendetta-online.com/) which was (is?) a fairly
fun space simulator. I stopped playing because people would role-play in it and
since the back story is a race war, it got ugly most of the time. I found
myself silencing all channels and playing single player and then got bored. The
Plod plugin would do some automated trading between stations. The Humaniform
plugin would send human-like actions to sector chat, e.g. burping and farting.

## 2012

[preg_router](./preg_router) is a PHP router. I'd been working at a company
where the person developing their proprietary platform implemented a router
that was basically a custom regex parser. It was insane to maintain. I had to
improve it once. I wrote this to console myself.

[dbdo](./dbdo) was a MySQL migration tool. I was mainly a PHP developer at the
time. I'd been using so many crap frameworks I was wondering what it would take
to run database migrations.

[nyc-kepler](./nyc-kepler) is the project for the first [NASA Space Apps
Challenge](https://spaceappschallenge.org). It was incredibly rewarding working
with that group of people. This was my first Node.js app and also the first
time I realized how awful callbacks were. During my time in frontend work I was
able to mitigate callback hell but I found working with Node.js to be a pain.
Recently I've had to come back to Node.js and using Promises are better but --
meh.

[exocodex](./exocodex) was a site I was going to build to learn Clojure using
Kepler data I got familiar with at the Space Apps Challenge. There were too
many pain points for me since, at this point, I was mostly used to Python
development. I sort considering anything on the JVM at this point. When asked I
like to say of Clojure that the worst part for me is the one most lauded by
other people: Java interop.

## 2013

[apiauth](./apiauth) is a few Python functions that implement a draft for
OAuth2. I found myself reading it one day and I thought it'd be fun to
implement something against a spec. (A process very different from web
development. 😏)

[tenper](./tenper) is a tmux manager. I was mostly full time on Python and I
was switching virtualenvs and project related environment variables all day. I
used this for a while and it helped. Ultimately I abandoned it for a smaller,
simpler Bash wrapper.

[trussws](./trussws) was meant to be like Python's SimpleHTTPServer with the
addition of uploads. It's a transient server that would shutdown after five
minutes of not being used. I'd switched to ChromeOS full time (at home and at
work) so all my development was in the cloud. Every once in a while someone
would email me something and I'd need to get it up to the remote server.  In
ChromeOS's developer mode it's very easy because you get a normal shell, but I
didn't want to turn on developer mode. I liked secure boot and when it's
disabled the normal super speedy boot up process is blocked by a frowny face
warning me that secure boot is disabled. It's annoying; lasts like a full
minute.

[django-s3-etag-collectstatic](./django-s3-etag-collectstatic) is an
improvement to a Django addon that would push static site assets to S3 (called,
I think, django-s3-collectstatic). The original plugin would use the modified
time of the assets on the filesystem to determine if it needed to upload
updates. We had a build server at my job so the files were always newly checked
out. I patched it to use S3's ETag and the md5sum of the file to determine if
it was updated.

## 2014

[json.erl](./json.erl) is a toy JSON parser in Erlang. I wanted a little
project to do in Erlang and JSON parsers are pretty simple. It's not idiomatic
Erlang, though, as I used strings and not the more efficient bitstrings. This
is also around the time I started getting interested in literate programming
and I converted it to a literate program using
[noweb](http://www.cs.tufts.edu/~nr/noweb/).

## 2015

[blue-dragon](./blue-dragon) is an abortive attempt to implement a game from my
youth: [Legend of the Red
Dragon](https://en.wikipedia.org/wiki/Legend_of_the_Red_Dragon). I was trying
to learn LISP Flavored Erlang and the idea was to implement it from the ground
up -- a custom HTTP server (which was working last I looked at it) and then the
game logic on top of that. I lost interest because I started to feel like what
I like about LFE was its Erlang-ness. Also, this was around the time my partner
was pregnant.

[lfe-watcher](./lfe-watcher) is a Docker image that will compile LFE code and
reload it into a running shell. I developed it as I was working on blue-dragon.

## 2016

[quash](./quash) is a joke I made at work. I don't remember the context, but I
still think it's funny.
