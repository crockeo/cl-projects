# cl-monthly-playlist

Interacting with the spotify API to manage monthly playlists all in Common Lisp.

## Work Items

A collection of things that I'd like to do:

- [ ] Save refresh token to disk, so a user doesn't have to re-auth every time
- [ ] Refresh token & retry request when token is expired (401 w/ appropriate
      error)
- [ ] Fill out the rest of the Spotify API
- [x] Populate URL / body parameters when provided to spotify API function

## Running

Run `sbcl --load build.lisp` to get things going. Not sure to do after that
because it's not made yet.

## License

MIT Open Source licensed. Refer to `LICENSE` file for more details.
