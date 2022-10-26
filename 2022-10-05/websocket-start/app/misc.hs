
-- https://stackoverflow.com/questions/46059483/how-to-access-newtype-named-tuples-fields-in-haskell

-- make instance parsed from Json or Text or Url so on..

-- https://stackoverflow.com/questions/20907974/haskell-record-accessors-shared-by-different-constructors-of-the-same-data-type

-- data RegArgsValidated = Errors { username :: [UserNameValidationError], 
--                                 email :: [EmailValidationError]
--                               } |
--                        Ok { username :: Username, 
--                                 email :: Email
--                               }        

-- data RegArgsValidated = RegDataValidated {
--   username :: Either [UserNameValidationError] Maybe UserName,
--   email :: Either [EmailValidationError] Email
-- }   

-- data FieldRequired = FieldRequired deriving (Generic,Show)
-- instance ToJSON FieldRequired where    
--     --toEncoding = genericToEncoding defaultOptions
--     toJSON _ = Data.Aeson.String "FieldRequired"


    -- toJSON es =  Data.Aeson.object ["username" .= username_error es, "email" .= er]
    --   where
    --     er = if isLeft em then Data.Aeson.String "FieldRequired" else toJSON $ fromRight undefined em
    --     em = email_error es

-- data RegParse = RegParse {
--   username_p :: String -> Validation [UserNameValidationError] UserName,
--   email_p :: String -> Validation [EmailValidationError] Email
-- }


-- router_ :: (ToJSON de,ToJSON dr) => LB.ByteString -> (a -> IO (ErrorOrResult de dr)) -> IO LB.ByteString
-- router_ b df = case (parseCmdfromJson b) :: ((JsonArgs ae ar, ToJSON ae) => Either ae ar) of 
--                                      (Left es) -> return $ encode es
--                                      (Right args) -> encode <$> (df args)

-- router :: LB.ByteString -> IO LB.ByteString
-- router b = case act of "reg" -> case (parseCmdfromJson b :: Either RegArgsErrors RegArgs) of 
--                                      (Left es) -> return $ encode es
--                                      (Right args) -> encode <$> (reg args)
--                        _ -> return $ "uncknown act " `mappend` LBU8.fromString act
--            where (act :: String) = fromJust $ exF b "action"

-- parseRowRegData :: T.Text -> Either (Maybe UserNameValidationError,
--                                      Maybe EmailValidationError) 
--                                     (UserName,
--                                      Email)


-- https://github.com/jaspervdj/websockets
-- main :: IO ()
-- main = putStrLn "Hello, Haskell!"

