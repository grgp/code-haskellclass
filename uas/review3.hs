tigaAksi :: (IO a, IO b, IO c) -> IO (a, b, c)
tigaAksi (a, b, c) = do ra <- a
                        rb <- b
                        rc <- c
                        return (show (ra, rb, rc))

test = do hasil <- tigaAksi (getLine, getLine, getLine)
          putStrLn hasil