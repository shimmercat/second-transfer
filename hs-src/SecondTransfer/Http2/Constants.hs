module SecondTransfer.Http2.Constants where


streamInitialFlowControlCredit :: Int
streamInitialFlowControlCredit = 65536


connectionInitialFlowControlCredit :: Int
connectionInitialFlowControlCredit = 65536


maxAllowedStreams :: Int
maxAllowedStreams = 200


-- | If this is the lowest data availabiliy in the connection,
--   we just don't send more data. Used from module
--   PriorityChannel
sessionFlowControlHighTide :: Int
sessionFlowControlHighTide = 8448
