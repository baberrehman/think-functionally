
cwords :: FilePath -> FilePath -> IO()
cwords infile outfile = do { text <- readFile infile;
                               writeFile outfile text;
                               putStrLn "cwords done!"
                             }

main = do {putStrLn "Take text from where:";
           infile <- getLine;
           putStrLn "How many words:";
           n <- getLine;
           putStrLn "Put results where:";
           outfile <- getLine;
           text <- readFile infile;
           writeFile outfile text;
           putStrLn "cwords done!"
          }

