module Core where
import           Data.Aeson 
import qualified Data.Aeson.Key as LB
import           Data.Validation
import qualified Data.ByteString.Lazy as LB
import           Data.Maybe
import           Data.Aeson.Types


-- Field required err or actual fields errors
data FieldRequired a = FieldRequired | FieldErrors a deriving Show



exF :: FromJSON b => LB.ByteString -> Key -> Maybe b
exF t n = do result <- decode t -- "{\"name\":\"Dave\",\"age\":2}"
             flip parseMaybe result $ \obj -> obj .: n

-- pReq :: LB.ByteString -> Key -> (String -> Validation a b) -> Either a b
-- pReq d k f = toEither $ f $ fromJust $ exF d k

-- pNull :: LB.ByteString -> Key -> (String -> Validation a b) -> Maybe (Either a b)
-- pNull d k f = toEither.f <$> exF d k

-- extract field from json and validate it with smartconstructor
pf :: LB.ByteString -> Key -> (String -> Validation a b) -> Maybe (Either a b)
pf d k f = toEither.f <$> exF d k

-- check that required field has error or not
e :: Maybe (Either a b) -> Bool
e Nothing = True
e (Just (Left _)) = True
e _ = False

-- check that nullable field has error or not
e_ :: Maybe (Either a b) -> Bool
e_ Nothing = False
e_ (Just (Left _)) = True
e_ _ = False


-- extract error of required field
errs :: Monoid a => Maybe (Either a b) -> FieldRequired a
errs (Just (Left a)) = FieldErrors a
errs Nothing = FieldRequired
errs _ = FieldErrors mempty
--errs _ = error "use only to extract errors (not succes results)"

-- extract error of an null field
errs_ :: Monoid a => Maybe (Either a b) -> a
errs_ (Just (Left a)) = a
errs_ _ = mempty
--errs_ _ = error "use only to extract errors (not succes results)"

--extract result of req field
res :: Maybe (Either a b) -> b
res (Just (Right b)) = b
res _ = error "use only to extract succes results"

--extract result of null field
res_ :: Maybe (Either a b) -> Maybe b
res_ (Just (Right b)) = Just b
res_ Nothing = Nothing
res_ _ = error "use only to extract succes results"  