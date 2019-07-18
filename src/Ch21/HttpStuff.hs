module Ch21.HttpStuff where

import Data.ByteString.Lazy
import Network.Wreq

urls :: [String]
urls = [ "http://httpbin.com/ip"
       , "http://httpbin.org/bytes/5"
       ]

mappingGet :: [IO (Response ByteString)]
mappingGet = fmap get urls

traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls
