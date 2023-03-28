-module(fib).

-export([seq/1]).

seq(0) -> 1;
seq(1) -> 1;
seq(Number) when Number > 1 -> seq(Number - 1) + seq(Number - 2).
