--https://www.stackage.org/package/wai
-- https://www.yesodweb.com/book/web-application-interface
-- https://codeahoy.com/learn/appliedfp/ch9/#templating
-- https://seanhess.github.io/2015/08/19/practical-haskell-json-api.html
-- https://stackoverflow.com/questions/63346279/how-is-it-possible-to-collect-all-error-messages-in-the-either-monad
-- https://haskell-at-work.com/episodes/2018-02-26-validation-with-smart-constructors.html
-- https://gist.github.com/sjsyrek/518735739239b5b0bcd6620162270f7a
-- https://stackoverflow.com/questions/63346279/how-is-it-possible-to-collect-all-error-messages-in-the-either-monad
-- https://hackage.haskell.org/package/monad-validate
-- https://github.com/system-f/validation/blob/master/examples/src/Email.hs


-- https://github.com/thma/PolysemyCleanArchitecture
-- https://blog.ploeh.dk/2016/03/18/functional-architecture-is-ports-and-adapters/
-- https://stackoverflow.com/questions/66781933/clean-architecture-repository-is-a-gateway-if-is-right-a-usecase-can-call-a
-- https://medium.com/@jonathanloscalzo/perhaps-gateway-and-repository-do-not-mean-the-same-according-to-fowler-7a4367a6d631
-- https://stackoverflow.com/questions/5801586/idiomatic-haskell-for-database-abstraction
-- http://blog.ezyang.com/2010/06/databases-are-categories/

-- routing
-- https://hackage.haskell.org/package/simple-0.3.0/docs/Web-Simple-Router.html

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE GADTs #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Char8 as Bs
import Text.Email.Validate
import qualified Data.Text as T

type UserName = String
type Password = String
type Email = String
type JWTToken = String

data EmailValidationError =
  MustNotBeEmpty | 
  MustContainAt |
  MustContainPeriod | 
  OtherError String deriving Show

data UserNameValidationError = 
  ToBigUserLength Int Int | 
  ToSmallUserLength Int Int | 
  InvalidUserChars String String deriving Show

data PasswordValidationError = 
  ToBigPassLength Int Int | 
  ToSmallPassLength Int Int | 
  InvalidPassChars String String deriving Show

data RegError =
  UserExists UserName |
  OtherRegError String deriving Show

data LoginError =
  NoSuchEmail Email |
  InvalidPassword deriving Show  


data ResultOk p where
  ROK :: p -> ResultOk p

data ResultOk_ = ROK_

makeFF :: T.Text -> (T.Text -> (Either [String] a)) -> Either [(String,String)] a
makeFF fieldname f = undefined

reg_ :: T.Text -> T.Text -> IO (Either [(String,String)] ResultOk_)
reg_ username email = undefined
    -- do 
    --     --https://github.com/system-f/validation/blob/master/examples/src/Email.hs
    --     let uname = makeFF "username" username MakeUser
    --     let email = makeFF "email" email MakeEmail
    --     r = liftA2 reg uname email
    --     if isLeft r then Left ("result", show $ fromLeft r) else return $ ROK_
    --     --if isLeft r
    --     -- if isRight uname && isRight email 

reg :: UserName -> Email -> IO (Either RegError ResultOk_)
reg u e = 
    do 
        print "Do reg action"
        return $ Right ROK_

login :: Email -> Password -> IO (Either LoginError (ResultOk JWTToken))
login = undefined


                       


app :: Application
app req respond = do
    putStrLn "I've done some IO here"
    print req
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 app



-- Чтобы зависимость была направлена в сторону обратную потоку данных, применяется принцип инверсии зависимостей (буква D из аббревиатуры SOLID). То есть, вместо того чтобы UseCase напрямую зависел от Presenter’a (что нарушало бы Dependency Rule), он зависит от интерфейса в своём слое, а Presenter должен этот интерфейс реализовать.
-- Точно та же схема работает и в других местах, например, при обращении UseCase к Gateway/Repository. Чтобы не зависеть от репозитория, выделяется интерфейс и кладется в слой UseCases.
-- Что же касается данных, которые пересекают границы, то это должны быть простые структуры. Они могут передаваться как DTO или быть завернуты в HashMap, или просто быть аргументами при вызове метода. Но они обязательно должны быть в форме более удобной для внутреннего слоя (лежать во внутреннем слое).



-- от вебсокета можно забрать все в Text или в ByteString см https://hackage.haskell.org/package/websockets-0.12.7.3/docs/Network-WebSockets.html#v:receiveData
-- забрать в Text -> трансформировать в json (отловить ошибку)
-- на основе json определить роутер
-- на основе json сформировать типы хаскеля (или собрать ошибки аппликативом и монойдом)
-- https://github.com/system-f/validation/blob/master/examples/src/Email.hs
-- проставить в домайн функцию типы хаскеля - получить результат (ошибка или домайн объект)
-- результат завернуть в json
-- json закодиоровать в utf-8 byterstring
-- отправить в вебсокет


-- опишем ендпоинты api в примитивных типах (а надо ли? если значения в полях сложнее?? NULL ?)

-- data Person = Person {
--       name :: Text
--     , age  :: Int
--     } deriving Show

-- instance FromJSON Person where
--     parseJSON = withObject "Person" $ \v -> Person
--         <$> v .: "name"
--         <*> v .: "age"

-- >>> decode "{\"name\":\"Joe\",\"age\":12}" :: Maybe Person
-- Just (Person {name = "Joe", age = 12})
-- https://williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html