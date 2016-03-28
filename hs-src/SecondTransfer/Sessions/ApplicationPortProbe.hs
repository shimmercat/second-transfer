
module SecondTransfer.Sessions.ApplicationPortProbe (
                ProbeProtocol
              , ProbeResult
       ) where


import          SecondTransfer.IOCallbacks.Types


-- | Was this protocol? Left gives an error message, right
--  means success
type ProbeResult = Either String ()

-- | A simple callback type to identify protocol at an application port.
--   Port-based applications (which is all of them when it comes to ShimmerCat)
--   open a port. This callback is used to probe that port and establish if the
--   port is listening at the given protocol.
type ProbeProtocol = IOCallbacks -> IO ProbeResult
