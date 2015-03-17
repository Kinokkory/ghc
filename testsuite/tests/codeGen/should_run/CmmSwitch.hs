{-# LANGUAGE MagicHash #-}
import Control.Monad (unless)
import GHC.Exts
{-# NOINLINE aa #-}
aa :: Int# -> Int#
aa 1# = 42#
aa 2# = 43#
aa 3# = 43#
aa 4# = 44#
aa 5# = 44#
aa 6# = 45#
aa 7# = 45#
aa 8# = 46#
aa 9# = 46#
aa 10# = 47#
aa _ = 1337#

{-# NOINLINE ab #-}
ab :: Int# -> Int#
ab 0# = 42#
ab 1# = 42#
ab 2# = 43#
ab 3# = 43#
ab 4# = 44#
ab 5# = 44#
ab 6# = 45#
ab 7# = 45#
ab 8# = 46#
ab 9# = 46#
ab 10# = 47#
ab _ = 1337#

{-# NOINLINE ac #-}
ac :: Int# -> Int#
ac -1# = 41#
ac 0# = 42#
ac 1# = 42#
ac 2# = 43#
ac 3# = 43#
ac 4# = 44#
ac 5# = 44#
ac 6# = 45#
ac 7# = 45#
ac 8# = 46#
ac 9# = 46#
ac 10# = 47#
ac _ = 1337#

{-# NOINLINE ad #-}
ad :: Int# -> Int#
ad 1# = 42#
ad 2# = 43#
ad 3# = 43#
ad _ = 1337#

{-# NOINLINE ae #-}
ae :: Int# -> Int#
ae 1# = 42#
ae 2# = 43#
ae 3# = 43#
ae 4# = 44#
ae _ = 1337#

{-# NOINLINE af #-}
af :: Int# -> Int#
af 1# = 42#
af 2# = 43#
af 3# = 43#
af 4# = 44#
af 5# = 44#
af _ = 1337#

{-# NOINLINE ag #-}
ag :: Int# -> Int#
ag -9223372036854775808# = -4611686018427387862#
ag 0# = 42#
ag 9223372036854775807# = 4611686018427387945#
ag _ = 1337#

{-# NOINLINE ah #-}
ah :: Int# -> Int#
ah -9223372036854775808# = -4611686018427387862#
ah -9223372036854775807# = -4611686018427387862#
ah -9223372036854775806# = -4611686018427387861#
ah -9223372036854775805# = -4611686018427387861#
ah -9223372036854775804# = -4611686018427387860#
ah -9223372036854775803# = -4611686018427387860#
ah -9223372036854775802# = -4611686018427387859#
ah -9223372036854775801# = -4611686018427387859#
ah -9223372036854775800# = -4611686018427387858#
ah -9223372036854775799# = -4611686018427387858#
ah -9223372036854775798# = -4611686018427387857#
ah 9223372036854775797# = 4611686018427387940#
ah 9223372036854775798# = 4611686018427387941#
ah 9223372036854775799# = 4611686018427387941#
ah 9223372036854775800# = 4611686018427387942#
ah 9223372036854775801# = 4611686018427387942#
ah 9223372036854775802# = 4611686018427387943#
ah 9223372036854775803# = 4611686018427387943#
ah 9223372036854775804# = 4611686018427387944#
ah 9223372036854775805# = 4611686018427387944#
ah 9223372036854775806# = 4611686018427387945#
ah 9223372036854775807# = 4611686018427387945#
ah _ = 1337#

aa_check :: IO ()
aa_check = do 
  let r = I# (aa 1#) in unless (r == 42) $ putStrLn $ "ERR: aa (1#) is " ++ show r ++ " and not 42"
  let r = I# (aa 2#) in unless (r == 43) $ putStrLn $ "ERR: aa (2#) is " ++ show r ++ " and not 43"
  let r = I# (aa 3#) in unless (r == 43) $ putStrLn $ "ERR: aa (3#) is " ++ show r ++ " and not 43"
  let r = I# (aa 4#) in unless (r == 44) $ putStrLn $ "ERR: aa (4#) is " ++ show r ++ " and not 44"
  let r = I# (aa 5#) in unless (r == 44) $ putStrLn $ "ERR: aa (5#) is " ++ show r ++ " and not 44"
  let r = I# (aa 6#) in unless (r == 45) $ putStrLn $ "ERR: aa (6#) is " ++ show r ++ " and not 45"
  let r = I# (aa 7#) in unless (r == 45) $ putStrLn $ "ERR: aa (7#) is " ++ show r ++ " and not 45"
  let r = I# (aa 8#) in unless (r == 46) $ putStrLn $ "ERR: aa (8#) is " ++ show r ++ " and not 46"
  let r = I# (aa 9#) in unless (r == 46) $ putStrLn $ "ERR: aa (9#) is " ++ show r ++ " and not 46"
  let r = I# (aa 10#) in unless (r == 47) $ putStrLn $ "ERR: aa (10#) is " ++ show r ++ " and not 47"

ab_check :: IO ()
ab_check = do 
  let r = I# (ab 0#) in unless (r == 42) $ putStrLn $ "ERR: ab (0#) is " ++ show r ++ " and not 42"
  let r = I# (ab 1#) in unless (r == 42) $ putStrLn $ "ERR: ab (1#) is " ++ show r ++ " and not 42"
  let r = I# (ab 2#) in unless (r == 43) $ putStrLn $ "ERR: ab (2#) is " ++ show r ++ " and not 43"
  let r = I# (ab 3#) in unless (r == 43) $ putStrLn $ "ERR: ab (3#) is " ++ show r ++ " and not 43"
  let r = I# (ab 4#) in unless (r == 44) $ putStrLn $ "ERR: ab (4#) is " ++ show r ++ " and not 44"
  let r = I# (ab 5#) in unless (r == 44) $ putStrLn $ "ERR: ab (5#) is " ++ show r ++ " and not 44"
  let r = I# (ab 6#) in unless (r == 45) $ putStrLn $ "ERR: ab (6#) is " ++ show r ++ " and not 45"
  let r = I# (ab 7#) in unless (r == 45) $ putStrLn $ "ERR: ab (7#) is " ++ show r ++ " and not 45"
  let r = I# (ab 8#) in unless (r == 46) $ putStrLn $ "ERR: ab (8#) is " ++ show r ++ " and not 46"
  let r = I# (ab 9#) in unless (r == 46) $ putStrLn $ "ERR: ab (9#) is " ++ show r ++ " and not 46"
  let r = I# (ab 10#) in unless (r == 47) $ putStrLn $ "ERR: ab (10#) is " ++ show r ++ " and not 47"

ac_check :: IO ()
ac_check = do 
  let r = I# (ac -1#) in unless (r == 41) $ putStrLn $ "ERR: ac (-1#) is " ++ show r ++ " and not 41"
  let r = I# (ac 0#) in unless (r == 42) $ putStrLn $ "ERR: ac (0#) is " ++ show r ++ " and not 42"
  let r = I# (ac 1#) in unless (r == 42) $ putStrLn $ "ERR: ac (1#) is " ++ show r ++ " and not 42"
  let r = I# (ac 2#) in unless (r == 43) $ putStrLn $ "ERR: ac (2#) is " ++ show r ++ " and not 43"
  let r = I# (ac 3#) in unless (r == 43) $ putStrLn $ "ERR: ac (3#) is " ++ show r ++ " and not 43"
  let r = I# (ac 4#) in unless (r == 44) $ putStrLn $ "ERR: ac (4#) is " ++ show r ++ " and not 44"
  let r = I# (ac 5#) in unless (r == 44) $ putStrLn $ "ERR: ac (5#) is " ++ show r ++ " and not 44"
  let r = I# (ac 6#) in unless (r == 45) $ putStrLn $ "ERR: ac (6#) is " ++ show r ++ " and not 45"
  let r = I# (ac 7#) in unless (r == 45) $ putStrLn $ "ERR: ac (7#) is " ++ show r ++ " and not 45"
  let r = I# (ac 8#) in unless (r == 46) $ putStrLn $ "ERR: ac (8#) is " ++ show r ++ " and not 46"
  let r = I# (ac 9#) in unless (r == 46) $ putStrLn $ "ERR: ac (9#) is " ++ show r ++ " and not 46"
  let r = I# (ac 10#) in unless (r == 47) $ putStrLn $ "ERR: ac (10#) is " ++ show r ++ " and not 47"

ad_check :: IO ()
ad_check = do 
  let r = I# (ad 1#) in unless (r == 42) $ putStrLn $ "ERR: ad (1#) is " ++ show r ++ " and not 42"
  let r = I# (ad 2#) in unless (r == 43) $ putStrLn $ "ERR: ad (2#) is " ++ show r ++ " and not 43"
  let r = I# (ad 3#) in unless (r == 43) $ putStrLn $ "ERR: ad (3#) is " ++ show r ++ " and not 43"

ae_check :: IO ()
ae_check = do 
  let r = I# (ae 1#) in unless (r == 42) $ putStrLn $ "ERR: ae (1#) is " ++ show r ++ " and not 42"
  let r = I# (ae 2#) in unless (r == 43) $ putStrLn $ "ERR: ae (2#) is " ++ show r ++ " and not 43"
  let r = I# (ae 3#) in unless (r == 43) $ putStrLn $ "ERR: ae (3#) is " ++ show r ++ " and not 43"
  let r = I# (ae 4#) in unless (r == 44) $ putStrLn $ "ERR: ae (4#) is " ++ show r ++ " and not 44"

af_check :: IO ()
af_check = do 
  let r = I# (af 1#) in unless (r == 42) $ putStrLn $ "ERR: af (1#) is " ++ show r ++ " and not 42"
  let r = I# (af 2#) in unless (r == 43) $ putStrLn $ "ERR: af (2#) is " ++ show r ++ " and not 43"
  let r = I# (af 3#) in unless (r == 43) $ putStrLn $ "ERR: af (3#) is " ++ show r ++ " and not 43"
  let r = I# (af 4#) in unless (r == 44) $ putStrLn $ "ERR: af (4#) is " ++ show r ++ " and not 44"
  let r = I# (af 5#) in unless (r == 44) $ putStrLn $ "ERR: af (5#) is " ++ show r ++ " and not 44"

ag_check :: IO ()
ag_check = do 
  let r = I# (ag -9223372036854775808#) in unless (r == -4611686018427387862) $ putStrLn $ "ERR: ag (-9223372036854775808#) is " ++ show r ++ " and not -4611686018427387862"
  let r = I# (ag 0#) in unless (r == 42) $ putStrLn $ "ERR: ag (0#) is " ++ show r ++ " and not 42"
  let r = I# (ag 9223372036854775807#) in unless (r == 4611686018427387945) $ putStrLn $ "ERR: ag (9223372036854775807#) is " ++ show r ++ " and not 4611686018427387945"

ah_check :: IO ()
ah_check = do 
  let r = I# (ah -9223372036854775808#) in unless (r == -4611686018427387862) $ putStrLn $ "ERR: ah (-9223372036854775808#) is " ++ show r ++ " and not -4611686018427387862"
  let r = I# (ah -9223372036854775807#) in unless (r == -4611686018427387862) $ putStrLn $ "ERR: ah (-9223372036854775807#) is " ++ show r ++ " and not -4611686018427387862"
  let r = I# (ah -9223372036854775806#) in unless (r == -4611686018427387861) $ putStrLn $ "ERR: ah (-9223372036854775806#) is " ++ show r ++ " and not -4611686018427387861"
  let r = I# (ah -9223372036854775805#) in unless (r == -4611686018427387861) $ putStrLn $ "ERR: ah (-9223372036854775805#) is " ++ show r ++ " and not -4611686018427387861"
  let r = I# (ah -9223372036854775804#) in unless (r == -4611686018427387860) $ putStrLn $ "ERR: ah (-9223372036854775804#) is " ++ show r ++ " and not -4611686018427387860"
  let r = I# (ah -9223372036854775803#) in unless (r == -4611686018427387860) $ putStrLn $ "ERR: ah (-9223372036854775803#) is " ++ show r ++ " and not -4611686018427387860"
  let r = I# (ah -9223372036854775802#) in unless (r == -4611686018427387859) $ putStrLn $ "ERR: ah (-9223372036854775802#) is " ++ show r ++ " and not -4611686018427387859"
  let r = I# (ah -9223372036854775801#) in unless (r == -4611686018427387859) $ putStrLn $ "ERR: ah (-9223372036854775801#) is " ++ show r ++ " and not -4611686018427387859"
  let r = I# (ah -9223372036854775800#) in unless (r == -4611686018427387858) $ putStrLn $ "ERR: ah (-9223372036854775800#) is " ++ show r ++ " and not -4611686018427387858"
  let r = I# (ah -9223372036854775799#) in unless (r == -4611686018427387858) $ putStrLn $ "ERR: ah (-9223372036854775799#) is " ++ show r ++ " and not -4611686018427387858"
  let r = I# (ah -9223372036854775798#) in unless (r == -4611686018427387857) $ putStrLn $ "ERR: ah (-9223372036854775798#) is " ++ show r ++ " and not -4611686018427387857"
  let r = I# (ah 9223372036854775797#) in unless (r == 4611686018427387940) $ putStrLn $ "ERR: ah (9223372036854775797#) is " ++ show r ++ " and not 4611686018427387940"
  let r = I# (ah 9223372036854775798#) in unless (r == 4611686018427387941) $ putStrLn $ "ERR: ah (9223372036854775798#) is " ++ show r ++ " and not 4611686018427387941"
  let r = I# (ah 9223372036854775799#) in unless (r == 4611686018427387941) $ putStrLn $ "ERR: ah (9223372036854775799#) is " ++ show r ++ " and not 4611686018427387941"
  let r = I# (ah 9223372036854775800#) in unless (r == 4611686018427387942) $ putStrLn $ "ERR: ah (9223372036854775800#) is " ++ show r ++ " and not 4611686018427387942"
  let r = I# (ah 9223372036854775801#) in unless (r == 4611686018427387942) $ putStrLn $ "ERR: ah (9223372036854775801#) is " ++ show r ++ " and not 4611686018427387942"
  let r = I# (ah 9223372036854775802#) in unless (r == 4611686018427387943) $ putStrLn $ "ERR: ah (9223372036854775802#) is " ++ show r ++ " and not 4611686018427387943"
  let r = I# (ah 9223372036854775803#) in unless (r == 4611686018427387943) $ putStrLn $ "ERR: ah (9223372036854775803#) is " ++ show r ++ " and not 4611686018427387943"
  let r = I# (ah 9223372036854775804#) in unless (r == 4611686018427387944) $ putStrLn $ "ERR: ah (9223372036854775804#) is " ++ show r ++ " and not 4611686018427387944"
  let r = I# (ah 9223372036854775805#) in unless (r == 4611686018427387944) $ putStrLn $ "ERR: ah (9223372036854775805#) is " ++ show r ++ " and not 4611686018427387944"
  let r = I# (ah 9223372036854775806#) in unless (r == 4611686018427387945) $ putStrLn $ "ERR: ah (9223372036854775806#) is " ++ show r ++ " and not 4611686018427387945"
  let r = I# (ah 9223372036854775807#) in unless (r == 4611686018427387945) $ putStrLn $ "ERR: ah (9223372036854775807#) is " ++ show r ++ " and not 4611686018427387945"

main = do
    aa_check
    ab_check
    ac_check
    ad_check
    ae_check
    af_check
    ag_check
    ah_check


