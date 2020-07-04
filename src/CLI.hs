module CLI (cli,parseFile,run) where

import Options.Applicative
import Data.Semigroup ((<>))
import Text.Megaparsec
import qualified Data.Text.IO as TIO
import Control.Monad.Except
import Runtime
import AST (Stmt)
import Parser (pMain)
import Type
import Interpreter (runProgram)


data CLIConfig = CLIConfig
    { fileName            :: String
    , clearExceptions     :: Bool }

cliConfig :: Parser CLIConfig
cliConfig = CLIConfig
    <$> argument str
        (  metavar "TARGET"
        <> help "path to .uwu file" )
    <*> switch
        ( long "human-errors"
        <> help "Un-UwU error messages for easier debuging")


cli :: IO (CLIConfig)
cli = execParser opts
    where
        opts = info (cliConfig <**> helper)
            ( fullDesc
            <> progDesc "This is the offical interpreter for UwUpp!"
            <> header "An intepreter for UwUpp")

parseFile :: CLIConfig -> IO( (Maybe [Stmt], Bool))
parseFile (CLIConfig file clearText) = do
    contents <- TIO.readFile file
    let ast = runParser pMain file contents
    case ast of
        Right stmts -> return $ (Just stmts,clearText)
        _           -> return $ (Nothing,clearText)

run :: (Maybe [Stmt],Bool) -> IO()
run (Nothing,False)   = putStrLn "OwO? fwile fwaild to parse"
run (Nothing,True)    = putStrLn "Failed to parse file, please double check spelling"
run ((Just stmts),b)  = runExceptT (runProgram stmts) >>= reportResult b

reportResult :: Bool -> Either RuntimeError () -> IO()
reportResult _     (Right _ ) = return ()
reportResult False   (Left e )  = print ("UwU? is dis a wuntwime ewwow?: " ++ uwuShow e)
reportResult True (Left e )  = print ("The program failed to execute: "++ show e)

