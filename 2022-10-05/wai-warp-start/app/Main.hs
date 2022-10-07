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
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Char8 as Bs
import Text.Email.Validate
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