# Hippo

> A web-based heap dump viewer in full-stack Scala

Status (as of **October 2021**): 

I am actively trying to abandon this project as I've learnt what I wanted from
it. But it's hard. There's something so cool about it (mostly scodec part).

## About

This project serves purely as an exercise for learning [scodec](https://github.com/scodec/scodec) and [Waypoint](https://github.com/raquo/waypoint). As well as Scala 3 and its new syntax.

The main purpose of it is to provide a self-contained app that you can launch by
pointing at a heap dump, and have it analysed in a nice web-based interface.

It should be as simple as running this:

> backend/run --hprof heapdump-1631032668990.hprof --port 1111

And then opening http://localhost:1111 to see this interface:

// TODO: add interface GIF

