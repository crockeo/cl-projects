# cl-projects

I had a bunch of half-finished Common Lisp projects. Instead of littering my
GitHub with them, I decided to collect them here. Each directory has its own
README.md that probably thinks it's its own project still.

## Projects

### [bokbok](/bokbok)

Draft of a 2D game engine written on top of SDL. Hosts a Swank server & finagles
the default cl-sdl2 event loop to support live editing of nearly everything.

### [cl-monthly-playlist](/cl-monthly-playlist)

A Spotify client wrapper and maybe one day monthly favorited songs
archiver. We'll see if we ever get there.

### [practical-common-lisp-exercises](/practical-common-lisp-exercises)

A collection of exercises written while reading [Practical Common
Lisp](http://www.gigamonkeys.com/book/). It's a great book, by the way. Read it
if you haven't.

## Planned Projects

This section contains projects that I haven't actually begun, but I want to do
40% of before getting bored and abandoning them.

### Vulkan

* [learn cffi](https://common-lisp.net/project/cffi/manual/cffi-manual.html#Tutorial)
* make basic Vulkan bindings (cffi exercise!)
* [follow tutorial](https://vulkan-tutorial.com/)

### Client-Definition Library

Take learnings from `cl-monthly-playlist` and apply it to other places. Make a
library to easily spec an API.

Apply this to certain places:

* Finish Spotify wrapper
* Make a better GH wrapper
* Other things too!

## Dependencies

Each of these projects were written using SBCL 2.0.7. No promises about
cross-implementation or cross-version compatibility. Bonne chance!

## License

MIT Open Source. Refer to `LICENSE` for more details.
