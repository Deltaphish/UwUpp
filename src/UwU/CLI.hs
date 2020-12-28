module UwU.CLI (cli,parseFile,run) where

import Options.Applicative
import Data.Semigroup ((<>))
import Text.Megaparsec
import qualified Data.Text.IO as TIO
import Control.Monad.Except

import UwU.Frontend.AST (Stmt)

import UwU.Frontend.Parser (pMain)
import UwU.Backend.Interpreter.TypeHelpers
import UwU.Backend.Interpreter.Run (runProgram)
import UwU.Backend.Interpreter.Runtime
import UwU.Backend.LLVM.Emit
import UwU.Backend.LLVM.IRGen

import LLVM.Module



data CLIConfig = CLIConfig
    { fileName            :: String
    , clearExceptions     :: Bool
    , compile             :: Bool}

cliConfig :: Parser CLIConfig
cliConfig = CLIConfig
    <$> argument str
        (  metavar "TARGET"
        <> help "path to .uwu file" )
    <*> switch
        ( long "human-errors"
        <> help "Un-UwU error messages for easier debuging")
    <*> switch
        ( long "compile"
        <> help "Compile uwu++ to LLVM IR")


cli :: IO (CLIConfig)
cli = execParser opts
    where
        opts = info (cliConfig <**> helper)
            ( fullDesc
            <> progDesc "This is the offical interpreter for UwUpp!"
            <> header "An intepreter for UwUpp")

parseFile :: CLIConfig -> IO()
parseFile (CLIConfig file clearText cmpl) = do
    contents <- TIO.readFile file
    print contents
    let ast = case runParser pMain file contents of
                Right stmts -> Just stmts
                _           -> Nothing

    print ast
    if cmpl then compileToIR ast else run (ast,clearText)


compileToIR :: Maybe [Stmt] -> IO()
compileToIR (Just stmts) = codegen (emptyModule (stosbs "discus")) stmts >> return ()

run :: (Maybe [Stmt],Bool) -> IO()
run (Nothing,False)   = putStrLn "OwO? fwile fwaild to parse"
run (Nothing,True)    = putStrLn "Failed to parse file, please double check spelling"
run ((Just stmts),b)  = runExceptT (runProgram stmts) >>= reportResult b

reportResult :: Bool -> Either RuntimeError () -> IO()
reportResult _     (Right _ ) = return ()
reportResult False   (Left e )  = print ("UwU? is dis a wuntwime ewwow?: " ++ uwuShow e)
reportResult True (Left e )  = print ("The program failed to execute: "++ show e)

