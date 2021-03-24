module ArgParser
  ( cmdParser
  , mainArgParser
  , splitToCmdArgs
  ) where

import           Data.List.NonEmpty    (NonEmpty (..), toList)
import           Options.Applicative   (Parser (..), ParserInfo (..), command,
                                        execParser, fullDesc, header, help,
                                        helper, hsubparser, info, metavar,
                                        progDesc, strArgument, value, (<**>))

import           Structure.ControlType (Cmd (..), RootPath (..))

-- | Parser for the initial path
argParser :: Parser RootPath
argParser = RootPath <$> strArgument
  (  metavar "RootDir"
  <> help "Initial root folder of file manager"
  )

-- | Parse command line argument as 'RootPath'
mainArgParser :: IO RootPath
mainArgParser = execParser opts
  where
    opts = info (argParser <**> helper)
      (  fullDesc
      <> progDesc "Provide root directory. There is no default argument here, \
        \think twice, the application will download everything into RAM. (Old hw version)"
      <> header "File manager argument"
      )

-- | Takes a command as a line and splits it into words, by spaces.
-- Inside double quotes, spaces are considered part of the word.
splitToCmdArgs :: String -> Maybe [String]
splitToCmdArgs input = if null input then Nothing else toList <$> splitBlocks input
  where
    splitBlocks :: String -> Maybe (NonEmpty String)
    splitBlocks [] = Just ("" :| [])
    splitBlocks (' ' : cs) = splitBlocks cs >>= \res@(b :| bs) -> return $
      if null b then res else ("" :| (b : bs))
    splitBlocks ('\\' : '"' : cs) = splitBlocks cs >>= \bs -> addToHead '\"' bs
    splitBlocks ('"' : cs) = parseBlock cs
    splitBlocks (ch : cs) = splitBlocks cs >>= \bs -> addToHead ch bs

    parseBlock :: String -> Maybe (NonEmpty String)
    parseBlock []                = Nothing
    parseBlock ('\\' : '"' : cs) = parseBlock cs >>= \bs -> addToHead '\"' bs
    parseBlock ('"' : cs)        = splitBlocks cs
    parseBlock (ch : cs)         = parseBlock cs >>= \bs -> addToHead ch bs

    addToHead :: Char -> NonEmpty String -> Maybe (NonEmpty String)
    addToHead c (s :| ss) = return ((c : s) :| ss)

-- | Cd command parser.
cdParser :: Parser Cmd
cdParser = Cd <$> strArgument (metavar "<dir_path>" <> help "Path to directory")

-- | Ls command parser.
lsParser :: Parser Cmd
lsParser = Ls <$> strArgument
  (  metavar "<dir_path>"
  <> help "Path to directory"
  <> value "."
  )

-- | Dir command parser.
dirParser :: Parser Cmd
dirParser = pure DirCmd

-- | Cat command parser.
catParser :: Parser Cmd
catParser = Cat <$> strArgument
  (  metavar "<file_path>"
  <> help "Show file content by path"
  )

-- | Create-folder command parser.
createFolderParser :: Parser Cmd
createFolderParser = CreateFolder <$> strArgument
  (  metavar "folder_name"
  <> help "Create folder by name"
  )

-- | Create-file command parser.
createFileParser :: Parser Cmd
createFileParser = CreateFile <$> strArgument
  (  metavar "file_name"
  <> help "Create empty file by name"
  )

-- | Find-file command parser.
findFileParser :: Parser Cmd
findFileParser = FindFile <$> strArgument
  (  metavar "file_name"
  <> help "Find file by name"
  )

-- | Information command parser.
informationParser :: Parser Cmd
informationParser = Information <$> strArgument
  (  metavar "<folder|file path>"
  <> help "Show entity info"
  )

-- | Quit command parser.
quitParser :: Parser Cmd
quitParser = pure Quit

-- | Remove command parser.
removeParser :: Parser Cmd
removeParser = Remove <$> strArgument
  (  metavar "<folder|file path>"
  <> help "Remove entity by path"
  )

treeParser :: Parser Cmd
treeParser = Tree <$> strArgument
  (  metavar "<folder_path>"
  <> help "Shows the filesystem tree starting from a passed directory."
  <> value "."
  )

-- | Write-file command parser.
writeFileParser :: Parser Cmd
writeFileParser = WriteFile <$>
  strArgument (metavar "<file path>" <> help "Path to file to write") <*>
  strArgument (metavar "text" <> help "Text to write")

-- | Parser selects a command from the proposed.
cmdOptions :: Parser Cmd
cmdOptions = hsubparser
  (  command "cat" (info catParser (progDesc "Show file content by path"))
  <> command "cd" (info cdParser (progDesc "Change directory"))
  <> command "create-file" (info createFileParser (progDesc "Create file by name"))
  <> command "create-folder" (info createFolderParser (progDesc "Create folder by name"))
  <> command "dir" (info dirParser (progDesc "Show current directory content"))
  <> command "find-file" (info findFileParser (progDesc "Find file in (sub)directory by name"))
  <> command "information" (info informationParser (progDesc "Show dir or file information"))
  <> command "ls" (info lsParser (progDesc "Show content of the directory"))
  <> command "quit" (info quitParser (progDesc "Write data to real file system and quit"))
  <> command "remove" (info removeParser (progDesc "Remove directory or file by path"))
  <> command "tree" (info treeParser (progDesc "Shows the filesystem tree by path"))
  <> command "write-file" (info writeFileParser (progDesc "Write text to file by path"))
  )

-- | The main parser that translates a line into a 'Cmd'.
cmdParser :: ParserInfo Cmd
cmdParser = info
  (helper <*> cmdOptions)
  (  fullDesc
  <> progDesc "Please use --help if you have some troubles or read documentation."
  <> header "File manager.\n\
    \Downloads the specified directory and whatever it finds recursively.\n\
    \Works with it. Then it loads back."
  )
