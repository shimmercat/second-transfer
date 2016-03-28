
module SecondTransfer.Sessions.ApplicationPortProbe (
                ProbeProtocol
       ) where


import          SecondTransfer.IOCallbacks.Types


-- | A simple callback type to identify protocol at an application port.
--   Port-based applications (which is all of them when it comes to ShimmerCat)
--   open a port. This callback is used to probe that port and establish if the
--   port is listening at the given protocol.
type ProbeProtocol = IOCallbacks -> Bool
