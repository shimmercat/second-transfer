
- 0.10.0.0 :
    * The AwareWorker interface was modified to include prompt resource 
      termination. That is, AwareWorkers can register finalizers for critical 
      resources now. But this breaks backward compatibility.
    * The internals pipe architecture was overhauled to have even less 
      unbounded chans and more MVar(s) acting as channels. This should result 
      in better behaviour regarding memory consumption. 

- 0.9.0.0 :
    * New tidal session manager to keep the number of connections 
      that clients make in check. 
    * Added support for loading certs from memory. 
    * HTTP/1.1 supports chunked transfer-encodings now 
    * Mac build works

- 0.8.0.0 :
    * All threads exceptions are reported 
    * Reverse proxy functionality from HTTP/2 to HTTP/1.1

- 0.7.1.0 :
    * A few important memory leaks got fixed
    * Changed I/O interface to live in IOCallbacks
    * Working now around congestion window.

- 0.6.1.0 :
    * Improved documentation.
    * Improved ALPN negotiation handling.
    * Messages with a body are now handled by the HTTP/1.1 speaking part of this
      library.

- 0.6.0.0 :
    * Changed the interface to provide more information to and from workers
    * Implemented HTTP/2 push
    * Upgraded dependencies to 1.0. for http2 package
    * Extended channel interface in such a way that latency is reduced, as a result,
      the test suite is completely broken. Please wait for 0.6.0.1 release.

- 0.5.5.1 :
    * Restricted HTTP2 package version.
    * Fixes #2

- 0.5.5.0 :
    * Made HeaderEditor a Monoid instance
    * Trying to WINDOW_UPDATE an unexistent stream aborts the session, as it should

- 0.5.4.0 :
    * Added the type HTTP500PrecursorException and corresponding test
    * Improved test suite so that requests can be simulated a little bit more easily
    * Re-export type Headers from module HTTPHeaders

- 0.5.3.2 :
    * Added this changelog.
    * Fixed the lower limit for the time package.
    * Enforce first frame being a settings frame when receiving.
    * Added a couple of tests.
