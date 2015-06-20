# What is this?

I'm working on a game that is a REST API version of a BBS door game called
[Legend of the Red Dragon][]. I'm creating it using [Lisp Flavored Erlang][]
(LFE) and I'm doing so without any frameworks. It's a demo application to get
me familiar with LFE and more familiar with Erlang.

It's also a literate program that uses [Knot][], a tool I wrote. All the code
is in Markdown documents.



# What I've done.

[My first pass at a web server][] was just a transliteration of the example in
[the `gen_tcp` man page][]. In [my second pass][] I added an acceptor pool and
a request parser. Finally, I applied OTP design principles and created
[myserver][], which is a behavior module that provides a generic socket server
and, since it required a surprising amount of exposition, an [HTTP request
parsing][] module.

I started developing the [game][].




[Legend of the Red Dragon]: https://en.wikipedia.org/wiki/Legend_of_the_Red_Dragon
[Lisp Flavored Erlang]: http://lfe.io/
[Knot]: https://github.com/mqsoh/knot
[My first pass at a web server]: research/simple.md
[the `gen_tcp` man page]: http://www.erlang.org/doc/man/gen_tcp.html
[my second pass]: research/less_simple.md
[myserver]: myserver.md
[HTTP request parsing]: parsing_http_requests.md
[game]: game.md
