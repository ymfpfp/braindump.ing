-- This is two parts:
-- 1. Use SES to send emails from the command line.
-- 2. HTTP server to take emails from

-- import Network.Wai
-- import Network.Wai.Handler.Warp
-- import Network.HTTP.Types

-- app :: Application
-- app req response = do
--   response $ responseLBS
--     status200
--     [("Content-Type", "text-

main :: IO () 
main = putStrLn "Hello, world!"
