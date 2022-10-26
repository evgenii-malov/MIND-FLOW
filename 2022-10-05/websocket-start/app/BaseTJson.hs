{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module BaseTJson where
import BaseT
import           Data.Aeson.Types
import Core 
import Data.Aeson
import qualified Data.ByteString.Lazy as LB

class JsonArgs e r | r -> e where
  parseCmdfromJson :: LB.ByteString -> Either e r
  -- collect_errors :: a -> Maybe T.Text -- json errors
  -- is_success :: a -> Bool

router_ :: (ToJSON de,ToJSON dr, ToJSON ae,JsonArgs ae ar) => (Either ae ar) -> (ar -> IO (ErrorOrResult de dr)) -> IO LB.ByteString
router_ pd df = case pd of 
                     (Left es) -> return $ encode es
                     (Right args) -> encode <$> (df args)

instance ToJSON ResultOk_ where    
    toJSON (_) = Data.Aeson.object ["result" .= ("ok" :: String)]

instance (ToJSON e, ToJSON r) => ToJSON (ErrorOrResult e r) where        
    toJSON (DError e) = Data.Aeson.object ["result" .= ("fail" :: String),"errors" .= toJSON e]
    toJSON (DResult r) = toJSON r

instance ToJSON a => ToJSON (FieldRequired a) where    
  toJSON FieldRequired = Data.Aeson.String "FieldRequired"
  toJSON (FieldErrors a) = toJSON a    


instance ToJSON Email where    
    toEncoding = genericToEncoding defaultOptions   


instance ToJSON EmailValidationError where    
    toEncoding = genericToEncoding defaultOptions    

instance ToJSON UserName where    
    toEncoding = genericToEncoding defaultOptions


instance ToJSON UserNameValidationError where    
    toEncoding = genericToEncoding defaultOptions    


