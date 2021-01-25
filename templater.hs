module Main (main) where
import System.Environment
import System.Directory
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Except
import System.FilePath
import Data.List
import Data.Char
import Data.Maybe
import Text.Regex.Posix hiding (multiline)
import qualified Data.Map.Strict as Map

data Context = Context {templateFile :: String
                        , templateDir :: String
                        , multiline :: Maybe (String, [String])
                        , vars :: Map.Map String String
                        , newFile :: Maybe String
                        , result :: [String]}

-- загружаем переменные в структуру Context
prepare :: String -> String -> IO Context
prepare templateFile sourceFile =
    let
        templateDir = takeDirectory templateFile ++ "/"
        vars = Map.empty
        initContext = Context templateFile templateDir Nothing vars Nothing []
    in
        loadVars initContext sourceFile

loadVars :: Context -> String -> IO Context
loadVars context sourceFile =
    readFile sourceFile >>= foldM (processLine processDirective) context . lines

processLine :: (Context -> String -> Context) -> Context -> String -> IO Context
processLine pureProcess context line =
    case newContext of
        Context { templateDir = dir, newFile = Just file }
            -> loadVars newContext { newFile = Nothing } (dir ++ file)
        otherwise -> return newContext
    where newContext = pureProcess context line

processDirective :: Context -> String -> Context
processDirective context@(Context {multiline = Just (sym, val), vars = vars}) line =
    if "==" `isPrefixOf` line
    then context {multiline = Nothing
                 , vars = Map.insertWith seq sym (unlines $ reverse val) vars}
    else context {multiline = Just (sym, dropWhileEnd isSpace line:val)}
processDirective context ('@':newFile) =
    context {templateFile = templateDir context ++ newFile}
processDirective context line =
    case includeFile of
        Just _ -> context { newFile = includeFile }
        Nothing ->  case regexpMatch "(.*[^=])=(.*)" line of
                        [var, val] -> varAdder context var val
                        [] -> context
    where includeFile = "включить " `stripPrefix` line

varAdder context var ('=':val) = context {multiline = Just (trim var, [])}
varAdder context var val =
    context {vars = Map.insertWith seq (trim var) (trim val) (vars context)}

trim = dropWhile isSpace . dropWhileEnd isSpace

regexpMatch :: String -> String -> [String]
regexpMatch regexp str = (\ (_, _, _, res) -> res)
                         (str =~ regexp :: (String, String, String, [String]))

regexpReplaceAll :: String -> (String -> String) -> String -> String
regexpReplaceAll regexp f str =
    case str =~ regexp of
        (_, "", _) -> str
        (before, at, after) -> before ++ f at ++ regexpReplaceAll regexp f after

-- применяем переменные из Context к шаблону
process :: Context -> String -> String -> IO ()
process context templateFile outFile =
    readFile templateFile
        >>= foldM (processLine processTemplate) context . lines
        >>= writeFile outFile . unlines . reverse . result

processTemplate :: Context -> String -> Context
processTemplate context@Context {result = outstr} line
    | "%[[" `isPrefixOf` line = processDirective context $ unbracket line
    | otherwise = context { result = replaceFromTemplate context line:outstr }

replaceFromTemplate :: Context -> String -> String
replaceFromTemplate context line =
    let new = regexpReplaceAll "\\[\\[([^\\[]*)\\]\\]" (replaceFunc $ vars context) line
    in if new == line then line else replaceFromTemplate context new

replaceFunc vars x =
    fromMaybe (error $ "Не задан параметр " ++ var) $ Map.lookup var vars
    where var = trim . dropWhile (== '[') . dropWhileEnd (== ']') $ x

unbracket =
    fromMaybe (error "Не закрытый %[[") . stripSuffux "]]"
        . fromJust . stripPrefix "%[["
    where stripSuffux x = fmap reverse . stripPrefix x . reverse

check test errStr = if test then return () else throwError errStr

getResult :: IO (Either String ())
getResult = runExceptT $ do
    args <- lift getArgs
    check (length args == 3)
          "Вызывать с аргументами templateFile, sourceFile, outFile"
    let [templateFile, sourceFile, outFile] = args
    sourceExists <- lift $ doesFileExist sourceFile
    check sourceExists $ "Не найден файл данных " ++ sourceFile
    context@(Context { templateFile = templateFile1
                       , vars = vars }) <- lift $ prepare templateFile sourceFile
    templateExists <- lift $ doesFileExist templateFile1
    check templateExists $ "Не найден файл шаблона " ++ templateFile1
    lift $ process context templateFile1 outFile

doResult :: Either String () -> IO ()
doResult (Left e) = putStrLn $ "Ошибка: " ++ e
doResult (Right r) = return r

main :: IO ()
main = getResult >>= doResult
