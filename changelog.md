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
