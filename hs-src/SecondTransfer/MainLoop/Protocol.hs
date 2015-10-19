module SecondTransfer.MainLoop.Protocol (
    HttpProtocolVersion(..)
                                        ) where


-- | The protocol version used. Here we distinguish only between
--   HTTP/1.1 and  HTTP/2
data HttpProtocolVersion =
     Http11_HPV
    |Http2_HPV
   deriving (Show, Eq, Enum, Ord)
