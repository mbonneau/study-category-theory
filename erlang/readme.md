# Erlang
[Erlang](https://en.wikipedia.org/wiki/Erlang_(programming_language)) is a general-purpose, concurrent, functional programming language. It is also a garbage-collected runtime system. The sequential subset of Erlang supports eager evaluation, single assignment, and dynamic typing. Erlang is known for its designs that are well suited for systems with the following characteristics:

- Distributed
- Fault-tolerant
- Soft real-time,
- Highly available, non-stop applications
- Hot swapping, where code can be changed without stopping a system.

## Documentation
- [Erlang Documentation](http://www.erlang.org/docs)

## Erlang tutorials
- [University of Kent - Erlang Massive Open Online Course (MOOC)](https://www.youtube.com/watch?v=yZ-e6ZT4G6U&list=PLlML6SMLMRgAooeL26mW502jCgWikqx_n)
- [University of Kent - Erlang Master Classes](https://www.youtube.com/watch?v=YZjAHRu4oF8&list=PLlML6SMLMRgCaVx42utIleC2aerD504qj)
- [Erlang Workshop 2009](https://vimeo.com/album/129567)

## Installing Erlang
Erlang can be installed with the brew formula:

```bash
$ brew install erlang
$ erl
Erlang/OTP 18 [erts-7.3] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V7.3  (abort with ^G)
1> Fun1 = fun (X) -> X+1 end.
#Fun<erl_eval.6.50752066>
2> Fun1(2).
3
```