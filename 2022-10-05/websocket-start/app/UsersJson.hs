{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module UsersJson where
import Users
import Data.Aeson 
import BaseTJson
import Core
import BaseT

instance ToJSON RegArgsErrors where    
    toEncoding = genericToEncoding defaultOptions


instance ToJSON RegError where    
    toEncoding = genericToEncoding defaultOptions




instance JsonArgs RegArgsErrors RegArgs where
  parseCmdfromJson t = if is_errors then Left $ RegArgsErrors 
                                                    (errs_ username_)
                                                    (errs email_)
                                    else Right $ RegArgs 
                                                    (res_ username_) 
                                                    (res email_)
                       where
                          is_errors = e username_ || e_ email_
                          username_ = pf t "username" makeUsername
                          email_ = pf t "email" makeEmail     