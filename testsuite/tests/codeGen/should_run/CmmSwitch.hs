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
ac 1# = 42#
ac 2# = 43#
ac 3# = 43#
ac _ = 1337#

{-# NOINLINE ad #-}
ad :: Int# -> Int#
ad 1# = 42#
ad 2# = 43#
ad 3# = 43#
ad 4# = 44#
ad _ = 1337#

{-# NOINLINE ae #-}
ae :: Int# -> Int#
ae 1# = 42#
ae 2# = 43#
ae 3# = 43#
ae 4# = 44#
ae 5# = 44#
ae _ = 1337#

{-# NOINLINE af #-}
af :: Int# -> Int#
af -1# = 41#
af 0# = 42#
af 1# = 42#
af 2# = 43#
af 3# = 43#
af 4# = 44#
af 5# = 44#
af 6# = 45#
af 7# = 45#
af 8# = 46#
af 9# = 46#
af 10# = 47#
af _ = 1337#

{-# NOINLINE ag #-}
ag :: Int# -> Int#
ag -10# = 37#
ag -9# = 37#
ag -8# = 38#
ag -7# = 38#
ag -6# = 39#
ag -5# = 39#
ag -4# = 40#
ag -3# = 40#
ag -2# = 41#
ag -1# = 41#
ag 0# = 42#
ag 1# = 42#
ag 2# = 43#
ag 3# = 43#
ag 4# = 44#
ag 5# = 44#
ag 6# = 45#
ag 7# = 45#
ag 8# = 46#
ag 9# = 46#
ag 10# = 47#
ag _ = 1337#

{-# NOINLINE ah #-}
ah :: Int# -> Int#
ah -20# = 32#
ah -19# = 32#
ah -18# = 33#
ah -17# = 33#
ah -16# = 34#
ah -15# = 34#
ah -14# = 35#
ah -13# = 35#
ah -12# = 36#
ah -11# = 36#
ah -10# = 37#
ah 0# = 42#
ah 1# = 42#
ah 2# = 43#
ah 3# = 43#
ah 4# = 44#
ah 5# = 44#
ah 6# = 45#
ah 7# = 45#
ah 8# = 46#
ah 9# = 46#
ah 10# = 47#
ah _ = 1337#

{-# NOINLINE ai #-}
ai :: Int# -> Int#
ai -20# = 32#
ai -19# = 32#
ai -18# = 33#
ai -17# = 33#
ai -16# = 34#
ai -15# = 34#
ai -14# = 35#
ai -13# = 35#
ai -12# = 36#
ai -11# = 36#
ai -10# = 37#
ai 1# = 42#
ai 2# = 43#
ai 3# = 43#
ai 4# = 44#
ai 5# = 44#
ai 6# = 45#
ai 7# = 45#
ai 8# = 46#
ai 9# = 46#
ai 10# = 47#
ai _ = 1337#

{-# NOINLINE aj #-}
aj :: Int# -> Int#
aj -9223372036854775808# = -4611686018427387862#
aj 0# = 42#
aj 9223372036854775807# = 4611686018427387945#
aj _ = 1337#

{-# NOINLINE ak #-}
ak :: Int# -> Int#
ak 9223372036854775797# = 4611686018427387940#
ak 9223372036854775798# = 4611686018427387941#
ak 9223372036854775799# = 4611686018427387941#
ak 9223372036854775800# = 4611686018427387942#
ak 9223372036854775801# = 4611686018427387942#
ak 9223372036854775802# = 4611686018427387943#
ak 9223372036854775803# = 4611686018427387943#
ak 9223372036854775804# = 4611686018427387944#
ak 9223372036854775805# = 4611686018427387944#
ak 9223372036854775806# = 4611686018427387945#
ak 9223372036854775807# = 4611686018427387945#
ak _ = 1337#

{-# NOINLINE al #-}
al :: Int# -> Int#
al -9223372036854775808# = -4611686018427387862#
al -9223372036854775807# = -4611686018427387862#
al -9223372036854775806# = -4611686018427387861#
al -9223372036854775805# = -4611686018427387861#
al -9223372036854775804# = -4611686018427387860#
al -9223372036854775803# = -4611686018427387860#
al -9223372036854775802# = -4611686018427387859#
al -9223372036854775801# = -4611686018427387859#
al -9223372036854775800# = -4611686018427387858#
al -9223372036854775799# = -4611686018427387858#
al -9223372036854775798# = -4611686018427387857#
al 9223372036854775797# = 4611686018427387940#
al 9223372036854775798# = 4611686018427387941#
al 9223372036854775799# = 4611686018427387941#
al 9223372036854775800# = 4611686018427387942#
al 9223372036854775801# = 4611686018427387942#
al 9223372036854775802# = 4611686018427387943#
al 9223372036854775803# = 4611686018427387943#
al 9223372036854775804# = 4611686018427387944#
al 9223372036854775805# = 4611686018427387944#
al 9223372036854775806# = 4611686018427387945#
al 9223372036854775807# = 4611686018427387945#
al _ = 1337#

{-# NOINLINE am #-}
am :: Word# -> Word#
am 0## = 42##
am 1## = 42##
am 2## = 43##
am 3## = 43##
am 4## = 44##
am 5## = 44##
am 6## = 45##
am 7## = 45##
am 8## = 46##
am 9## = 46##
am 10## = 47##
am _ = 1337##

{-# NOINLINE an #-}
an :: Word# -> Word#
an 1## = 42##
an 2## = 43##
an 3## = 43##
an 4## = 44##
an 5## = 44##
an 6## = 45##
an 7## = 45##
an 8## = 46##
an 9## = 46##
an 10## = 47##
an _ = 1337##

{-# NOINLINE ao #-}
ao :: Word# -> Word#
ao 0## = 42##
ao _ = 1337##

{-# NOINLINE ap #-}
ap :: Word# -> Word#
ap 0## = 42##
ap 1## = 42##
ap _ = 1337##

{-# NOINLINE aq #-}
aq :: Word# -> Word#
aq 0## = 42##
aq 1## = 42##
aq 2## = 43##
aq _ = 1337##

{-# NOINLINE ar #-}
ar :: Word# -> Word#
ar 0## = 42##
ar 1## = 42##
ar 2## = 43##
ar 3## = 43##
ar _ = 1337##

{-# NOINLINE as #-}
as :: Word# -> Word#
as 0## = 42##
as 1## = 42##
as 2## = 43##
as 3## = 43##
as 4## = 44##
as _ = 1337##

{-# NOINLINE at #-}
at :: Word# -> Word#
at 1## = 42##
at _ = 1337##

{-# NOINLINE au #-}
au :: Word# -> Word#
au 1## = 42##
au 2## = 43##
au _ = 1337##

{-# NOINLINE av #-}
av :: Word# -> Word#
av 1## = 42##
av 2## = 43##
av 3## = 43##
av _ = 1337##

{-# NOINLINE aw #-}
aw :: Word# -> Word#
aw 1## = 42##
aw 2## = 43##
aw 3## = 43##
aw 4## = 44##
aw _ = 1337##

{-# NOINLINE ax #-}
ax :: Word# -> Word#
ax 1## = 42##
ax 2## = 43##
ax 3## = 43##
ax 4## = 44##
ax 5## = 44##
ax _ = 1337##

{-# NOINLINE ay #-}
ay :: Word# -> Word#
ay 0## = 42##
ay 18446744073709551615## = 9223372036854775849##
ay _ = 1337##

{-# NOINLINE az #-}
az :: Word# -> Word#
az 18446744073709551605## = 9223372036854775844##
az 18446744073709551606## = 9223372036854775845##
az 18446744073709551607## = 9223372036854775845##
az 18446744073709551608## = 9223372036854775846##
az 18446744073709551609## = 9223372036854775846##
az 18446744073709551610## = 9223372036854775847##
az 18446744073709551611## = 9223372036854775847##
az 18446744073709551612## = 9223372036854775848##
az 18446744073709551613## = 9223372036854775848##
az 18446744073709551614## = 9223372036854775849##
az 18446744073709551615## = 9223372036854775849##
az _ = 1337##

{-# NOINLINE ba #-}
ba :: Word# -> Word#
ba 0## = 42##
ba 1## = 42##
ba 2## = 43##
ba 3## = 43##
ba 4## = 44##
ba 5## = 44##
ba 6## = 45##
ba 7## = 45##
ba 8## = 46##
ba 9## = 46##
ba 10## = 47##
ba 18446744073709551605## = 9223372036854775844##
ba 18446744073709551606## = 9223372036854775845##
ba 18446744073709551607## = 9223372036854775845##
ba 18446744073709551608## = 9223372036854775846##
ba 18446744073709551609## = 9223372036854775846##
ba 18446744073709551610## = 9223372036854775847##
ba 18446744073709551611## = 9223372036854775847##
ba 18446744073709551612## = 9223372036854775848##
ba 18446744073709551613## = 9223372036854775848##
ba 18446744073709551614## = 9223372036854775849##
ba 18446744073709551615## = 9223372036854775849##
ba _ = 1337##

aa_check :: IO ()
aa_check = do 
  let r = I# (aa 0#) in unless (r == 1337) $ putStrLn $ "ERR: aa (0#) is " ++ show r ++ " and not 1337"
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
  let r = I# (aa 11#) in unless (r == 1337) $ putStrLn $ "ERR: aa (11#) is " ++ show r ++ " and not 1337"

ab_check :: IO ()
ab_check = do 
  let r = I# (ab -1#) in unless (r == 1337) $ putStrLn $ "ERR: ab (-1#) is " ++ show r ++ " and not 1337"
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
  let r = I# (ab 11#) in unless (r == 1337) $ putStrLn $ "ERR: ab (11#) is " ++ show r ++ " and not 1337"

ac_check :: IO ()
ac_check = do 
  let r = I# (ac 0#) in unless (r == 1337) $ putStrLn $ "ERR: ac (0#) is " ++ show r ++ " and not 1337"
  let r = I# (ac 1#) in unless (r == 42) $ putStrLn $ "ERR: ac (1#) is " ++ show r ++ " and not 42"
  let r = I# (ac 2#) in unless (r == 43) $ putStrLn $ "ERR: ac (2#) is " ++ show r ++ " and not 43"
  let r = I# (ac 3#) in unless (r == 43) $ putStrLn $ "ERR: ac (3#) is " ++ show r ++ " and not 43"
  let r = I# (ac 4#) in unless (r == 1337) $ putStrLn $ "ERR: ac (4#) is " ++ show r ++ " and not 1337"

ad_check :: IO ()
ad_check = do 
  let r = I# (ad 0#) in unless (r == 1337) $ putStrLn $ "ERR: ad (0#) is " ++ show r ++ " and not 1337"
  let r = I# (ad 1#) in unless (r == 42) $ putStrLn $ "ERR: ad (1#) is " ++ show r ++ " and not 42"
  let r = I# (ad 2#) in unless (r == 43) $ putStrLn $ "ERR: ad (2#) is " ++ show r ++ " and not 43"
  let r = I# (ad 3#) in unless (r == 43) $ putStrLn $ "ERR: ad (3#) is " ++ show r ++ " and not 43"
  let r = I# (ad 4#) in unless (r == 44) $ putStrLn $ "ERR: ad (4#) is " ++ show r ++ " and not 44"
  let r = I# (ad 5#) in unless (r == 1337) $ putStrLn $ "ERR: ad (5#) is " ++ show r ++ " and not 1337"

ae_check :: IO ()
ae_check = do 
  let r = I# (ae 0#) in unless (r == 1337) $ putStrLn $ "ERR: ae (0#) is " ++ show r ++ " and not 1337"
  let r = I# (ae 1#) in unless (r == 42) $ putStrLn $ "ERR: ae (1#) is " ++ show r ++ " and not 42"
  let r = I# (ae 2#) in unless (r == 43) $ putStrLn $ "ERR: ae (2#) is " ++ show r ++ " and not 43"
  let r = I# (ae 3#) in unless (r == 43) $ putStrLn $ "ERR: ae (3#) is " ++ show r ++ " and not 43"
  let r = I# (ae 4#) in unless (r == 44) $ putStrLn $ "ERR: ae (4#) is " ++ show r ++ " and not 44"
  let r = I# (ae 5#) in unless (r == 44) $ putStrLn $ "ERR: ae (5#) is " ++ show r ++ " and not 44"
  let r = I# (ae 6#) in unless (r == 1337) $ putStrLn $ "ERR: ae (6#) is " ++ show r ++ " and not 1337"

af_check :: IO ()
af_check = do 
  let r = I# (af -2#) in unless (r == 1337) $ putStrLn $ "ERR: af (-2#) is " ++ show r ++ " and not 1337"
  let r = I# (af -1#) in unless (r == 41) $ putStrLn $ "ERR: af (-1#) is " ++ show r ++ " and not 41"
  let r = I# (af 0#) in unless (r == 42) $ putStrLn $ "ERR: af (0#) is " ++ show r ++ " and not 42"
  let r = I# (af 1#) in unless (r == 42) $ putStrLn $ "ERR: af (1#) is " ++ show r ++ " and not 42"
  let r = I# (af 2#) in unless (r == 43) $ putStrLn $ "ERR: af (2#) is " ++ show r ++ " and not 43"
  let r = I# (af 3#) in unless (r == 43) $ putStrLn $ "ERR: af (3#) is " ++ show r ++ " and not 43"
  let r = I# (af 4#) in unless (r == 44) $ putStrLn $ "ERR: af (4#) is " ++ show r ++ " and not 44"
  let r = I# (af 5#) in unless (r == 44) $ putStrLn $ "ERR: af (5#) is " ++ show r ++ " and not 44"
  let r = I# (af 6#) in unless (r == 45) $ putStrLn $ "ERR: af (6#) is " ++ show r ++ " and not 45"
  let r = I# (af 7#) in unless (r == 45) $ putStrLn $ "ERR: af (7#) is " ++ show r ++ " and not 45"
  let r = I# (af 8#) in unless (r == 46) $ putStrLn $ "ERR: af (8#) is " ++ show r ++ " and not 46"
  let r = I# (af 9#) in unless (r == 46) $ putStrLn $ "ERR: af (9#) is " ++ show r ++ " and not 46"
  let r = I# (af 10#) in unless (r == 47) $ putStrLn $ "ERR: af (10#) is " ++ show r ++ " and not 47"
  let r = I# (af 11#) in unless (r == 1337) $ putStrLn $ "ERR: af (11#) is " ++ show r ++ " and not 1337"

ag_check :: IO ()
ag_check = do 
  let r = I# (ag -11#) in unless (r == 1337) $ putStrLn $ "ERR: ag (-11#) is " ++ show r ++ " and not 1337"
  let r = I# (ag -10#) in unless (r == 37) $ putStrLn $ "ERR: ag (-10#) is " ++ show r ++ " and not 37"
  let r = I# (ag -9#) in unless (r == 37) $ putStrLn $ "ERR: ag (-9#) is " ++ show r ++ " and not 37"
  let r = I# (ag -8#) in unless (r == 38) $ putStrLn $ "ERR: ag (-8#) is " ++ show r ++ " and not 38"
  let r = I# (ag -7#) in unless (r == 38) $ putStrLn $ "ERR: ag (-7#) is " ++ show r ++ " and not 38"
  let r = I# (ag -6#) in unless (r == 39) $ putStrLn $ "ERR: ag (-6#) is " ++ show r ++ " and not 39"
  let r = I# (ag -5#) in unless (r == 39) $ putStrLn $ "ERR: ag (-5#) is " ++ show r ++ " and not 39"
  let r = I# (ag -4#) in unless (r == 40) $ putStrLn $ "ERR: ag (-4#) is " ++ show r ++ " and not 40"
  let r = I# (ag -3#) in unless (r == 40) $ putStrLn $ "ERR: ag (-3#) is " ++ show r ++ " and not 40"
  let r = I# (ag -2#) in unless (r == 41) $ putStrLn $ "ERR: ag (-2#) is " ++ show r ++ " and not 41"
  let r = I# (ag -1#) in unless (r == 41) $ putStrLn $ "ERR: ag (-1#) is " ++ show r ++ " and not 41"
  let r = I# (ag 0#) in unless (r == 42) $ putStrLn $ "ERR: ag (0#) is " ++ show r ++ " and not 42"
  let r = I# (ag 1#) in unless (r == 42) $ putStrLn $ "ERR: ag (1#) is " ++ show r ++ " and not 42"
  let r = I# (ag 2#) in unless (r == 43) $ putStrLn $ "ERR: ag (2#) is " ++ show r ++ " and not 43"
  let r = I# (ag 3#) in unless (r == 43) $ putStrLn $ "ERR: ag (3#) is " ++ show r ++ " and not 43"
  let r = I# (ag 4#) in unless (r == 44) $ putStrLn $ "ERR: ag (4#) is " ++ show r ++ " and not 44"
  let r = I# (ag 5#) in unless (r == 44) $ putStrLn $ "ERR: ag (5#) is " ++ show r ++ " and not 44"
  let r = I# (ag 6#) in unless (r == 45) $ putStrLn $ "ERR: ag (6#) is " ++ show r ++ " and not 45"
  let r = I# (ag 7#) in unless (r == 45) $ putStrLn $ "ERR: ag (7#) is " ++ show r ++ " and not 45"
  let r = I# (ag 8#) in unless (r == 46) $ putStrLn $ "ERR: ag (8#) is " ++ show r ++ " and not 46"
  let r = I# (ag 9#) in unless (r == 46) $ putStrLn $ "ERR: ag (9#) is " ++ show r ++ " and not 46"
  let r = I# (ag 10#) in unless (r == 47) $ putStrLn $ "ERR: ag (10#) is " ++ show r ++ " and not 47"
  let r = I# (ag 11#) in unless (r == 1337) $ putStrLn $ "ERR: ag (11#) is " ++ show r ++ " and not 1337"

ah_check :: IO ()
ah_check = do 
  let r = I# (ah -21#) in unless (r == 1337) $ putStrLn $ "ERR: ah (-21#) is " ++ show r ++ " and not 1337"
  let r = I# (ah -20#) in unless (r == 32) $ putStrLn $ "ERR: ah (-20#) is " ++ show r ++ " and not 32"
  let r = I# (ah -19#) in unless (r == 32) $ putStrLn $ "ERR: ah (-19#) is " ++ show r ++ " and not 32"
  let r = I# (ah -18#) in unless (r == 33) $ putStrLn $ "ERR: ah (-18#) is " ++ show r ++ " and not 33"
  let r = I# (ah -17#) in unless (r == 33) $ putStrLn $ "ERR: ah (-17#) is " ++ show r ++ " and not 33"
  let r = I# (ah -16#) in unless (r == 34) $ putStrLn $ "ERR: ah (-16#) is " ++ show r ++ " and not 34"
  let r = I# (ah -15#) in unless (r == 34) $ putStrLn $ "ERR: ah (-15#) is " ++ show r ++ " and not 34"
  let r = I# (ah -14#) in unless (r == 35) $ putStrLn $ "ERR: ah (-14#) is " ++ show r ++ " and not 35"
  let r = I# (ah -13#) in unless (r == 35) $ putStrLn $ "ERR: ah (-13#) is " ++ show r ++ " and not 35"
  let r = I# (ah -12#) in unless (r == 36) $ putStrLn $ "ERR: ah (-12#) is " ++ show r ++ " and not 36"
  let r = I# (ah -11#) in unless (r == 36) $ putStrLn $ "ERR: ah (-11#) is " ++ show r ++ " and not 36"
  let r = I# (ah -10#) in unless (r == 37) $ putStrLn $ "ERR: ah (-10#) is " ++ show r ++ " and not 37"
  let r = I# (ah -9#) in unless (r == 1337) $ putStrLn $ "ERR: ah (-9#) is " ++ show r ++ " and not 1337"
  let r = I# (ah -1#) in unless (r == 1337) $ putStrLn $ "ERR: ah (-1#) is " ++ show r ++ " and not 1337"
  let r = I# (ah 0#) in unless (r == 42) $ putStrLn $ "ERR: ah (0#) is " ++ show r ++ " and not 42"
  let r = I# (ah 1#) in unless (r == 42) $ putStrLn $ "ERR: ah (1#) is " ++ show r ++ " and not 42"
  let r = I# (ah 2#) in unless (r == 43) $ putStrLn $ "ERR: ah (2#) is " ++ show r ++ " and not 43"
  let r = I# (ah 3#) in unless (r == 43) $ putStrLn $ "ERR: ah (3#) is " ++ show r ++ " and not 43"
  let r = I# (ah 4#) in unless (r == 44) $ putStrLn $ "ERR: ah (4#) is " ++ show r ++ " and not 44"
  let r = I# (ah 5#) in unless (r == 44) $ putStrLn $ "ERR: ah (5#) is " ++ show r ++ " and not 44"
  let r = I# (ah 6#) in unless (r == 45) $ putStrLn $ "ERR: ah (6#) is " ++ show r ++ " and not 45"
  let r = I# (ah 7#) in unless (r == 45) $ putStrLn $ "ERR: ah (7#) is " ++ show r ++ " and not 45"
  let r = I# (ah 8#) in unless (r == 46) $ putStrLn $ "ERR: ah (8#) is " ++ show r ++ " and not 46"
  let r = I# (ah 9#) in unless (r == 46) $ putStrLn $ "ERR: ah (9#) is " ++ show r ++ " and not 46"
  let r = I# (ah 10#) in unless (r == 47) $ putStrLn $ "ERR: ah (10#) is " ++ show r ++ " and not 47"
  let r = I# (ah 11#) in unless (r == 1337) $ putStrLn $ "ERR: ah (11#) is " ++ show r ++ " and not 1337"

ai_check :: IO ()
ai_check = do 
  let r = I# (ai -21#) in unless (r == 1337) $ putStrLn $ "ERR: ai (-21#) is " ++ show r ++ " and not 1337"
  let r = I# (ai -20#) in unless (r == 32) $ putStrLn $ "ERR: ai (-20#) is " ++ show r ++ " and not 32"
  let r = I# (ai -19#) in unless (r == 32) $ putStrLn $ "ERR: ai (-19#) is " ++ show r ++ " and not 32"
  let r = I# (ai -18#) in unless (r == 33) $ putStrLn $ "ERR: ai (-18#) is " ++ show r ++ " and not 33"
  let r = I# (ai -17#) in unless (r == 33) $ putStrLn $ "ERR: ai (-17#) is " ++ show r ++ " and not 33"
  let r = I# (ai -16#) in unless (r == 34) $ putStrLn $ "ERR: ai (-16#) is " ++ show r ++ " and not 34"
  let r = I# (ai -15#) in unless (r == 34) $ putStrLn $ "ERR: ai (-15#) is " ++ show r ++ " and not 34"
  let r = I# (ai -14#) in unless (r == 35) $ putStrLn $ "ERR: ai (-14#) is " ++ show r ++ " and not 35"
  let r = I# (ai -13#) in unless (r == 35) $ putStrLn $ "ERR: ai (-13#) is " ++ show r ++ " and not 35"
  let r = I# (ai -12#) in unless (r == 36) $ putStrLn $ "ERR: ai (-12#) is " ++ show r ++ " and not 36"
  let r = I# (ai -11#) in unless (r == 36) $ putStrLn $ "ERR: ai (-11#) is " ++ show r ++ " and not 36"
  let r = I# (ai -10#) in unless (r == 37) $ putStrLn $ "ERR: ai (-10#) is " ++ show r ++ " and not 37"
  let r = I# (ai -9#) in unless (r == 1337) $ putStrLn $ "ERR: ai (-9#) is " ++ show r ++ " and not 1337"
  let r = I# (ai 0#) in unless (r == 1337) $ putStrLn $ "ERR: ai (0#) is " ++ show r ++ " and not 1337"
  let r = I# (ai 1#) in unless (r == 42) $ putStrLn $ "ERR: ai (1#) is " ++ show r ++ " and not 42"
  let r = I# (ai 2#) in unless (r == 43) $ putStrLn $ "ERR: ai (2#) is " ++ show r ++ " and not 43"
  let r = I# (ai 3#) in unless (r == 43) $ putStrLn $ "ERR: ai (3#) is " ++ show r ++ " and not 43"
  let r = I# (ai 4#) in unless (r == 44) $ putStrLn $ "ERR: ai (4#) is " ++ show r ++ " and not 44"
  let r = I# (ai 5#) in unless (r == 44) $ putStrLn $ "ERR: ai (5#) is " ++ show r ++ " and not 44"
  let r = I# (ai 6#) in unless (r == 45) $ putStrLn $ "ERR: ai (6#) is " ++ show r ++ " and not 45"
  let r = I# (ai 7#) in unless (r == 45) $ putStrLn $ "ERR: ai (7#) is " ++ show r ++ " and not 45"
  let r = I# (ai 8#) in unless (r == 46) $ putStrLn $ "ERR: ai (8#) is " ++ show r ++ " and not 46"
  let r = I# (ai 9#) in unless (r == 46) $ putStrLn $ "ERR: ai (9#) is " ++ show r ++ " and not 46"
  let r = I# (ai 10#) in unless (r == 47) $ putStrLn $ "ERR: ai (10#) is " ++ show r ++ " and not 47"
  let r = I# (ai 11#) in unless (r == 1337) $ putStrLn $ "ERR: ai (11#) is " ++ show r ++ " and not 1337"

aj_check :: IO ()
aj_check = do 
  let r = I# (aj -9223372036854775808#) in unless (r == -4611686018427387862) $ putStrLn $ "ERR: aj (-9223372036854775808#) is " ++ show r ++ " and not -4611686018427387862"
  let r = I# (aj -9223372036854775807#) in unless (r == 1337) $ putStrLn $ "ERR: aj (-9223372036854775807#) is " ++ show r ++ " and not 1337"
  let r = I# (aj -1#) in unless (r == 1337) $ putStrLn $ "ERR: aj (-1#) is " ++ show r ++ " and not 1337"
  let r = I# (aj 0#) in unless (r == 42) $ putStrLn $ "ERR: aj (0#) is " ++ show r ++ " and not 42"
  let r = I# (aj 1#) in unless (r == 1337) $ putStrLn $ "ERR: aj (1#) is " ++ show r ++ " and not 1337"
  let r = I# (aj 9223372036854775806#) in unless (r == 1337) $ putStrLn $ "ERR: aj (9223372036854775806#) is " ++ show r ++ " and not 1337"
  let r = I# (aj 9223372036854775807#) in unless (r == 4611686018427387945) $ putStrLn $ "ERR: aj (9223372036854775807#) is " ++ show r ++ " and not 4611686018427387945"

ak_check :: IO ()
ak_check = do 
  let r = I# (ak 9223372036854775796#) in unless (r == 1337) $ putStrLn $ "ERR: ak (9223372036854775796#) is " ++ show r ++ " and not 1337"
  let r = I# (ak 9223372036854775797#) in unless (r == 4611686018427387940) $ putStrLn $ "ERR: ak (9223372036854775797#) is " ++ show r ++ " and not 4611686018427387940"
  let r = I# (ak 9223372036854775798#) in unless (r == 4611686018427387941) $ putStrLn $ "ERR: ak (9223372036854775798#) is " ++ show r ++ " and not 4611686018427387941"
  let r = I# (ak 9223372036854775799#) in unless (r == 4611686018427387941) $ putStrLn $ "ERR: ak (9223372036854775799#) is " ++ show r ++ " and not 4611686018427387941"
  let r = I# (ak 9223372036854775800#) in unless (r == 4611686018427387942) $ putStrLn $ "ERR: ak (9223372036854775800#) is " ++ show r ++ " and not 4611686018427387942"
  let r = I# (ak 9223372036854775801#) in unless (r == 4611686018427387942) $ putStrLn $ "ERR: ak (9223372036854775801#) is " ++ show r ++ " and not 4611686018427387942"
  let r = I# (ak 9223372036854775802#) in unless (r == 4611686018427387943) $ putStrLn $ "ERR: ak (9223372036854775802#) is " ++ show r ++ " and not 4611686018427387943"
  let r = I# (ak 9223372036854775803#) in unless (r == 4611686018427387943) $ putStrLn $ "ERR: ak (9223372036854775803#) is " ++ show r ++ " and not 4611686018427387943"
  let r = I# (ak 9223372036854775804#) in unless (r == 4611686018427387944) $ putStrLn $ "ERR: ak (9223372036854775804#) is " ++ show r ++ " and not 4611686018427387944"
  let r = I# (ak 9223372036854775805#) in unless (r == 4611686018427387944) $ putStrLn $ "ERR: ak (9223372036854775805#) is " ++ show r ++ " and not 4611686018427387944"
  let r = I# (ak 9223372036854775806#) in unless (r == 4611686018427387945) $ putStrLn $ "ERR: ak (9223372036854775806#) is " ++ show r ++ " and not 4611686018427387945"
  let r = I# (ak 9223372036854775807#) in unless (r == 4611686018427387945) $ putStrLn $ "ERR: ak (9223372036854775807#) is " ++ show r ++ " and not 4611686018427387945"

al_check :: IO ()
al_check = do 
  let r = I# (al -9223372036854775808#) in unless (r == -4611686018427387862) $ putStrLn $ "ERR: al (-9223372036854775808#) is " ++ show r ++ " and not -4611686018427387862"
  let r = I# (al -9223372036854775807#) in unless (r == -4611686018427387862) $ putStrLn $ "ERR: al (-9223372036854775807#) is " ++ show r ++ " and not -4611686018427387862"
  let r = I# (al -9223372036854775806#) in unless (r == -4611686018427387861) $ putStrLn $ "ERR: al (-9223372036854775806#) is " ++ show r ++ " and not -4611686018427387861"
  let r = I# (al -9223372036854775805#) in unless (r == -4611686018427387861) $ putStrLn $ "ERR: al (-9223372036854775805#) is " ++ show r ++ " and not -4611686018427387861"
  let r = I# (al -9223372036854775804#) in unless (r == -4611686018427387860) $ putStrLn $ "ERR: al (-9223372036854775804#) is " ++ show r ++ " and not -4611686018427387860"
  let r = I# (al -9223372036854775803#) in unless (r == -4611686018427387860) $ putStrLn $ "ERR: al (-9223372036854775803#) is " ++ show r ++ " and not -4611686018427387860"
  let r = I# (al -9223372036854775802#) in unless (r == -4611686018427387859) $ putStrLn $ "ERR: al (-9223372036854775802#) is " ++ show r ++ " and not -4611686018427387859"
  let r = I# (al -9223372036854775801#) in unless (r == -4611686018427387859) $ putStrLn $ "ERR: al (-9223372036854775801#) is " ++ show r ++ " and not -4611686018427387859"
  let r = I# (al -9223372036854775800#) in unless (r == -4611686018427387858) $ putStrLn $ "ERR: al (-9223372036854775800#) is " ++ show r ++ " and not -4611686018427387858"
  let r = I# (al -9223372036854775799#) in unless (r == -4611686018427387858) $ putStrLn $ "ERR: al (-9223372036854775799#) is " ++ show r ++ " and not -4611686018427387858"
  let r = I# (al -9223372036854775798#) in unless (r == -4611686018427387857) $ putStrLn $ "ERR: al (-9223372036854775798#) is " ++ show r ++ " and not -4611686018427387857"
  let r = I# (al -9223372036854775797#) in unless (r == 1337) $ putStrLn $ "ERR: al (-9223372036854775797#) is " ++ show r ++ " and not 1337"
  let r = I# (al 9223372036854775796#) in unless (r == 1337) $ putStrLn $ "ERR: al (9223372036854775796#) is " ++ show r ++ " and not 1337"
  let r = I# (al 9223372036854775797#) in unless (r == 4611686018427387940) $ putStrLn $ "ERR: al (9223372036854775797#) is " ++ show r ++ " and not 4611686018427387940"
  let r = I# (al 9223372036854775798#) in unless (r == 4611686018427387941) $ putStrLn $ "ERR: al (9223372036854775798#) is " ++ show r ++ " and not 4611686018427387941"
  let r = I# (al 9223372036854775799#) in unless (r == 4611686018427387941) $ putStrLn $ "ERR: al (9223372036854775799#) is " ++ show r ++ " and not 4611686018427387941"
  let r = I# (al 9223372036854775800#) in unless (r == 4611686018427387942) $ putStrLn $ "ERR: al (9223372036854775800#) is " ++ show r ++ " and not 4611686018427387942"
  let r = I# (al 9223372036854775801#) in unless (r == 4611686018427387942) $ putStrLn $ "ERR: al (9223372036854775801#) is " ++ show r ++ " and not 4611686018427387942"
  let r = I# (al 9223372036854775802#) in unless (r == 4611686018427387943) $ putStrLn $ "ERR: al (9223372036854775802#) is " ++ show r ++ " and not 4611686018427387943"
  let r = I# (al 9223372036854775803#) in unless (r == 4611686018427387943) $ putStrLn $ "ERR: al (9223372036854775803#) is " ++ show r ++ " and not 4611686018427387943"
  let r = I# (al 9223372036854775804#) in unless (r == 4611686018427387944) $ putStrLn $ "ERR: al (9223372036854775804#) is " ++ show r ++ " and not 4611686018427387944"
  let r = I# (al 9223372036854775805#) in unless (r == 4611686018427387944) $ putStrLn $ "ERR: al (9223372036854775805#) is " ++ show r ++ " and not 4611686018427387944"
  let r = I# (al 9223372036854775806#) in unless (r == 4611686018427387945) $ putStrLn $ "ERR: al (9223372036854775806#) is " ++ show r ++ " and not 4611686018427387945"
  let r = I# (al 9223372036854775807#) in unless (r == 4611686018427387945) $ putStrLn $ "ERR: al (9223372036854775807#) is " ++ show r ++ " and not 4611686018427387945"

am_check :: IO ()
am_check = do 
  let r = W# (am 0##) in unless (r == 42) $ putStrLn $ "ERR: am (0##) is " ++ show r ++ " and not 42"
  let r = W# (am 1##) in unless (r == 42) $ putStrLn $ "ERR: am (1##) is " ++ show r ++ " and not 42"
  let r = W# (am 2##) in unless (r == 43) $ putStrLn $ "ERR: am (2##) is " ++ show r ++ " and not 43"
  let r = W# (am 3##) in unless (r == 43) $ putStrLn $ "ERR: am (3##) is " ++ show r ++ " and not 43"
  let r = W# (am 4##) in unless (r == 44) $ putStrLn $ "ERR: am (4##) is " ++ show r ++ " and not 44"
  let r = W# (am 5##) in unless (r == 44) $ putStrLn $ "ERR: am (5##) is " ++ show r ++ " and not 44"
  let r = W# (am 6##) in unless (r == 45) $ putStrLn $ "ERR: am (6##) is " ++ show r ++ " and not 45"
  let r = W# (am 7##) in unless (r == 45) $ putStrLn $ "ERR: am (7##) is " ++ show r ++ " and not 45"
  let r = W# (am 8##) in unless (r == 46) $ putStrLn $ "ERR: am (8##) is " ++ show r ++ " and not 46"
  let r = W# (am 9##) in unless (r == 46) $ putStrLn $ "ERR: am (9##) is " ++ show r ++ " and not 46"
  let r = W# (am 10##) in unless (r == 47) $ putStrLn $ "ERR: am (10##) is " ++ show r ++ " and not 47"
  let r = W# (am 11##) in unless (r == 1337) $ putStrLn $ "ERR: am (11##) is " ++ show r ++ " and not 1337"

an_check :: IO ()
an_check = do 
  let r = W# (an 0##) in unless (r == 1337) $ putStrLn $ "ERR: an (0##) is " ++ show r ++ " and not 1337"
  let r = W# (an 1##) in unless (r == 42) $ putStrLn $ "ERR: an (1##) is " ++ show r ++ " and not 42"
  let r = W# (an 2##) in unless (r == 43) $ putStrLn $ "ERR: an (2##) is " ++ show r ++ " and not 43"
  let r = W# (an 3##) in unless (r == 43) $ putStrLn $ "ERR: an (3##) is " ++ show r ++ " and not 43"
  let r = W# (an 4##) in unless (r == 44) $ putStrLn $ "ERR: an (4##) is " ++ show r ++ " and not 44"
  let r = W# (an 5##) in unless (r == 44) $ putStrLn $ "ERR: an (5##) is " ++ show r ++ " and not 44"
  let r = W# (an 6##) in unless (r == 45) $ putStrLn $ "ERR: an (6##) is " ++ show r ++ " and not 45"
  let r = W# (an 7##) in unless (r == 45) $ putStrLn $ "ERR: an (7##) is " ++ show r ++ " and not 45"
  let r = W# (an 8##) in unless (r == 46) $ putStrLn $ "ERR: an (8##) is " ++ show r ++ " and not 46"
  let r = W# (an 9##) in unless (r == 46) $ putStrLn $ "ERR: an (9##) is " ++ show r ++ " and not 46"
  let r = W# (an 10##) in unless (r == 47) $ putStrLn $ "ERR: an (10##) is " ++ show r ++ " and not 47"
  let r = W# (an 11##) in unless (r == 1337) $ putStrLn $ "ERR: an (11##) is " ++ show r ++ " and not 1337"

ao_check :: IO ()
ao_check = do 
  let r = W# (ao 0##) in unless (r == 42) $ putStrLn $ "ERR: ao (0##) is " ++ show r ++ " and not 42"
  let r = W# (ao 1##) in unless (r == 1337) $ putStrLn $ "ERR: ao (1##) is " ++ show r ++ " and not 1337"

ap_check :: IO ()
ap_check = do 
  let r = W# (ap 0##) in unless (r == 42) $ putStrLn $ "ERR: ap (0##) is " ++ show r ++ " and not 42"
  let r = W# (ap 1##) in unless (r == 42) $ putStrLn $ "ERR: ap (1##) is " ++ show r ++ " and not 42"
  let r = W# (ap 2##) in unless (r == 1337) $ putStrLn $ "ERR: ap (2##) is " ++ show r ++ " and not 1337"

aq_check :: IO ()
aq_check = do 
  let r = W# (aq 0##) in unless (r == 42) $ putStrLn $ "ERR: aq (0##) is " ++ show r ++ " and not 42"
  let r = W# (aq 1##) in unless (r == 42) $ putStrLn $ "ERR: aq (1##) is " ++ show r ++ " and not 42"
  let r = W# (aq 2##) in unless (r == 43) $ putStrLn $ "ERR: aq (2##) is " ++ show r ++ " and not 43"
  let r = W# (aq 3##) in unless (r == 1337) $ putStrLn $ "ERR: aq (3##) is " ++ show r ++ " and not 1337"

ar_check :: IO ()
ar_check = do 
  let r = W# (ar 0##) in unless (r == 42) $ putStrLn $ "ERR: ar (0##) is " ++ show r ++ " and not 42"
  let r = W# (ar 1##) in unless (r == 42) $ putStrLn $ "ERR: ar (1##) is " ++ show r ++ " and not 42"
  let r = W# (ar 2##) in unless (r == 43) $ putStrLn $ "ERR: ar (2##) is " ++ show r ++ " and not 43"
  let r = W# (ar 3##) in unless (r == 43) $ putStrLn $ "ERR: ar (3##) is " ++ show r ++ " and not 43"
  let r = W# (ar 4##) in unless (r == 1337) $ putStrLn $ "ERR: ar (4##) is " ++ show r ++ " and not 1337"

as_check :: IO ()
as_check = do 
  let r = W# (as 0##) in unless (r == 42) $ putStrLn $ "ERR: as (0##) is " ++ show r ++ " and not 42"
  let r = W# (as 1##) in unless (r == 42) $ putStrLn $ "ERR: as (1##) is " ++ show r ++ " and not 42"
  let r = W# (as 2##) in unless (r == 43) $ putStrLn $ "ERR: as (2##) is " ++ show r ++ " and not 43"
  let r = W# (as 3##) in unless (r == 43) $ putStrLn $ "ERR: as (3##) is " ++ show r ++ " and not 43"
  let r = W# (as 4##) in unless (r == 44) $ putStrLn $ "ERR: as (4##) is " ++ show r ++ " and not 44"
  let r = W# (as 5##) in unless (r == 1337) $ putStrLn $ "ERR: as (5##) is " ++ show r ++ " and not 1337"

at_check :: IO ()
at_check = do 
  let r = W# (at 0##) in unless (r == 1337) $ putStrLn $ "ERR: at (0##) is " ++ show r ++ " and not 1337"
  let r = W# (at 1##) in unless (r == 42) $ putStrLn $ "ERR: at (1##) is " ++ show r ++ " and not 42"
  let r = W# (at 2##) in unless (r == 1337) $ putStrLn $ "ERR: at (2##) is " ++ show r ++ " and not 1337"

au_check :: IO ()
au_check = do 
  let r = W# (au 0##) in unless (r == 1337) $ putStrLn $ "ERR: au (0##) is " ++ show r ++ " and not 1337"
  let r = W# (au 1##) in unless (r == 42) $ putStrLn $ "ERR: au (1##) is " ++ show r ++ " and not 42"
  let r = W# (au 2##) in unless (r == 43) $ putStrLn $ "ERR: au (2##) is " ++ show r ++ " and not 43"
  let r = W# (au 3##) in unless (r == 1337) $ putStrLn $ "ERR: au (3##) is " ++ show r ++ " and not 1337"

av_check :: IO ()
av_check = do 
  let r = W# (av 0##) in unless (r == 1337) $ putStrLn $ "ERR: av (0##) is " ++ show r ++ " and not 1337"
  let r = W# (av 1##) in unless (r == 42) $ putStrLn $ "ERR: av (1##) is " ++ show r ++ " and not 42"
  let r = W# (av 2##) in unless (r == 43) $ putStrLn $ "ERR: av (2##) is " ++ show r ++ " and not 43"
  let r = W# (av 3##) in unless (r == 43) $ putStrLn $ "ERR: av (3##) is " ++ show r ++ " and not 43"
  let r = W# (av 4##) in unless (r == 1337) $ putStrLn $ "ERR: av (4##) is " ++ show r ++ " and not 1337"

aw_check :: IO ()
aw_check = do 
  let r = W# (aw 0##) in unless (r == 1337) $ putStrLn $ "ERR: aw (0##) is " ++ show r ++ " and not 1337"
  let r = W# (aw 1##) in unless (r == 42) $ putStrLn $ "ERR: aw (1##) is " ++ show r ++ " and not 42"
  let r = W# (aw 2##) in unless (r == 43) $ putStrLn $ "ERR: aw (2##) is " ++ show r ++ " and not 43"
  let r = W# (aw 3##) in unless (r == 43) $ putStrLn $ "ERR: aw (3##) is " ++ show r ++ " and not 43"
  let r = W# (aw 4##) in unless (r == 44) $ putStrLn $ "ERR: aw (4##) is " ++ show r ++ " and not 44"
  let r = W# (aw 5##) in unless (r == 1337) $ putStrLn $ "ERR: aw (5##) is " ++ show r ++ " and not 1337"

ax_check :: IO ()
ax_check = do 
  let r = W# (ax 0##) in unless (r == 1337) $ putStrLn $ "ERR: ax (0##) is " ++ show r ++ " and not 1337"
  let r = W# (ax 1##) in unless (r == 42) $ putStrLn $ "ERR: ax (1##) is " ++ show r ++ " and not 42"
  let r = W# (ax 2##) in unless (r == 43) $ putStrLn $ "ERR: ax (2##) is " ++ show r ++ " and not 43"
  let r = W# (ax 3##) in unless (r == 43) $ putStrLn $ "ERR: ax (3##) is " ++ show r ++ " and not 43"
  let r = W# (ax 4##) in unless (r == 44) $ putStrLn $ "ERR: ax (4##) is " ++ show r ++ " and not 44"
  let r = W# (ax 5##) in unless (r == 44) $ putStrLn $ "ERR: ax (5##) is " ++ show r ++ " and not 44"
  let r = W# (ax 6##) in unless (r == 1337) $ putStrLn $ "ERR: ax (6##) is " ++ show r ++ " and not 1337"

ay_check :: IO ()
ay_check = do 
  let r = W# (ay 0##) in unless (r == 42) $ putStrLn $ "ERR: ay (0##) is " ++ show r ++ " and not 42"
  let r = W# (ay 1##) in unless (r == 1337) $ putStrLn $ "ERR: ay (1##) is " ++ show r ++ " and not 1337"
  let r = W# (ay 18446744073709551614##) in unless (r == 1337) $ putStrLn $ "ERR: ay (18446744073709551614##) is " ++ show r ++ " and not 1337"
  let r = W# (ay 18446744073709551615##) in unless (r == 9223372036854775849) $ putStrLn $ "ERR: ay (18446744073709551615##) is " ++ show r ++ " and not 9223372036854775849"

az_check :: IO ()
az_check = do 
  let r = W# (az 18446744073709551604##) in unless (r == 1337) $ putStrLn $ "ERR: az (18446744073709551604##) is " ++ show r ++ " and not 1337"
  let r = W# (az 18446744073709551605##) in unless (r == 9223372036854775844) $ putStrLn $ "ERR: az (18446744073709551605##) is " ++ show r ++ " and not 9223372036854775844"
  let r = W# (az 18446744073709551606##) in unless (r == 9223372036854775845) $ putStrLn $ "ERR: az (18446744073709551606##) is " ++ show r ++ " and not 9223372036854775845"
  let r = W# (az 18446744073709551607##) in unless (r == 9223372036854775845) $ putStrLn $ "ERR: az (18446744073709551607##) is " ++ show r ++ " and not 9223372036854775845"
  let r = W# (az 18446744073709551608##) in unless (r == 9223372036854775846) $ putStrLn $ "ERR: az (18446744073709551608##) is " ++ show r ++ " and not 9223372036854775846"
  let r = W# (az 18446744073709551609##) in unless (r == 9223372036854775846) $ putStrLn $ "ERR: az (18446744073709551609##) is " ++ show r ++ " and not 9223372036854775846"
  let r = W# (az 18446744073709551610##) in unless (r == 9223372036854775847) $ putStrLn $ "ERR: az (18446744073709551610##) is " ++ show r ++ " and not 9223372036854775847"
  let r = W# (az 18446744073709551611##) in unless (r == 9223372036854775847) $ putStrLn $ "ERR: az (18446744073709551611##) is " ++ show r ++ " and not 9223372036854775847"
  let r = W# (az 18446744073709551612##) in unless (r == 9223372036854775848) $ putStrLn $ "ERR: az (18446744073709551612##) is " ++ show r ++ " and not 9223372036854775848"
  let r = W# (az 18446744073709551613##) in unless (r == 9223372036854775848) $ putStrLn $ "ERR: az (18446744073709551613##) is " ++ show r ++ " and not 9223372036854775848"
  let r = W# (az 18446744073709551614##) in unless (r == 9223372036854775849) $ putStrLn $ "ERR: az (18446744073709551614##) is " ++ show r ++ " and not 9223372036854775849"
  let r = W# (az 18446744073709551615##) in unless (r == 9223372036854775849) $ putStrLn $ "ERR: az (18446744073709551615##) is " ++ show r ++ " and not 9223372036854775849"

ba_check :: IO ()
ba_check = do 
  let r = W# (ba 0##) in unless (r == 42) $ putStrLn $ "ERR: ba (0##) is " ++ show r ++ " and not 42"
  let r = W# (ba 1##) in unless (r == 42) $ putStrLn $ "ERR: ba (1##) is " ++ show r ++ " and not 42"
  let r = W# (ba 2##) in unless (r == 43) $ putStrLn $ "ERR: ba (2##) is " ++ show r ++ " and not 43"
  let r = W# (ba 3##) in unless (r == 43) $ putStrLn $ "ERR: ba (3##) is " ++ show r ++ " and not 43"
  let r = W# (ba 4##) in unless (r == 44) $ putStrLn $ "ERR: ba (4##) is " ++ show r ++ " and not 44"
  let r = W# (ba 5##) in unless (r == 44) $ putStrLn $ "ERR: ba (5##) is " ++ show r ++ " and not 44"
  let r = W# (ba 6##) in unless (r == 45) $ putStrLn $ "ERR: ba (6##) is " ++ show r ++ " and not 45"
  let r = W# (ba 7##) in unless (r == 45) $ putStrLn $ "ERR: ba (7##) is " ++ show r ++ " and not 45"
  let r = W# (ba 8##) in unless (r == 46) $ putStrLn $ "ERR: ba (8##) is " ++ show r ++ " and not 46"
  let r = W# (ba 9##) in unless (r == 46) $ putStrLn $ "ERR: ba (9##) is " ++ show r ++ " and not 46"
  let r = W# (ba 10##) in unless (r == 47) $ putStrLn $ "ERR: ba (10##) is " ++ show r ++ " and not 47"
  let r = W# (ba 11##) in unless (r == 1337) $ putStrLn $ "ERR: ba (11##) is " ++ show r ++ " and not 1337"
  let r = W# (ba 18446744073709551604##) in unless (r == 1337) $ putStrLn $ "ERR: ba (18446744073709551604##) is " ++ show r ++ " and not 1337"
  let r = W# (ba 18446744073709551605##) in unless (r == 9223372036854775844) $ putStrLn $ "ERR: ba (18446744073709551605##) is " ++ show r ++ " and not 9223372036854775844"
  let r = W# (ba 18446744073709551606##) in unless (r == 9223372036854775845) $ putStrLn $ "ERR: ba (18446744073709551606##) is " ++ show r ++ " and not 9223372036854775845"
  let r = W# (ba 18446744073709551607##) in unless (r == 9223372036854775845) $ putStrLn $ "ERR: ba (18446744073709551607##) is " ++ show r ++ " and not 9223372036854775845"
  let r = W# (ba 18446744073709551608##) in unless (r == 9223372036854775846) $ putStrLn $ "ERR: ba (18446744073709551608##) is " ++ show r ++ " and not 9223372036854775846"
  let r = W# (ba 18446744073709551609##) in unless (r == 9223372036854775846) $ putStrLn $ "ERR: ba (18446744073709551609##) is " ++ show r ++ " and not 9223372036854775846"
  let r = W# (ba 18446744073709551610##) in unless (r == 9223372036854775847) $ putStrLn $ "ERR: ba (18446744073709551610##) is " ++ show r ++ " and not 9223372036854775847"
  let r = W# (ba 18446744073709551611##) in unless (r == 9223372036854775847) $ putStrLn $ "ERR: ba (18446744073709551611##) is " ++ show r ++ " and not 9223372036854775847"
  let r = W# (ba 18446744073709551612##) in unless (r == 9223372036854775848) $ putStrLn $ "ERR: ba (18446744073709551612##) is " ++ show r ++ " and not 9223372036854775848"
  let r = W# (ba 18446744073709551613##) in unless (r == 9223372036854775848) $ putStrLn $ "ERR: ba (18446744073709551613##) is " ++ show r ++ " and not 9223372036854775848"
  let r = W# (ba 18446744073709551614##) in unless (r == 9223372036854775849) $ putStrLn $ "ERR: ba (18446744073709551614##) is " ++ show r ++ " and not 9223372036854775849"
  let r = W# (ba 18446744073709551615##) in unless (r == 9223372036854775849) $ putStrLn $ "ERR: ba (18446744073709551615##) is " ++ show r ++ " and not 9223372036854775849"

main = do
    aa_check
    ab_check
    ac_check
    ad_check
    ae_check
    af_check
    ag_check
    ah_check
    ai_check
    aj_check
    ak_check
    al_check
    am_check
    an_check
    ao_check
    ap_check
    aq_check
    ar_check
    as_check
    at_check
    au_check
    av_check
    aw_check
    ax_check
    ay_check
    az_check
    ba_check


