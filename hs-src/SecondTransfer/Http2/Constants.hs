module SecondTransfer.Http2.Constants where


streamInitialFlowControlCredit :: Int
streamInitialFlowControlCredit = 65536000


connectionInitialFlowControlCredit :: Int
connectionInitialFlowControlCredit = 65536000


maxAllowedStreams :: Int
maxAllowedStreams = 200


-- | If this is the lowest data availabiliy in the connection,
--   we just don't send more data. Used from module
--   PriorityChannel
sessionFlowControlHighTide :: Int
sessionFlowControlHighTide = 8448



default_DYNAMIC_TABLE_FOR_ENCODING :: Int
default_DYNAMIC_TABLE_FOR_ENCODING = 65536


default_DYNAMIC_TABLE_FOR_DECODING :: Int
default_DYNAMIC_TABLE_FOR_DECODING = 65536
