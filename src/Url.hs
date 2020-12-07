module Url
  ( Url(..)
  , percentEncode
  , percentDecode
  ) where


{-|

# URLs
@docs Url, Protocol, toString, fromString

# Percent-Encoding
@docs percentEncode, percentDecode

-}


import qualified Prelude
import qualified Network.HTTP.Types.URI as URI
import qualified Maybe
import qualified List
import qualified String
import qualified Dict
import Cherry.Prelude


-- URL


{-| In [the URI spec](https://tools.ietf.org/html/rfc3986), Tim Berners-Lee
says a URL looks like this:

```
  https://example.com:8042/over/there?name=ferret#nose
  \___/   \______________/\_________/ \_________/ \__/
    |            |            |            |        |
  scheme     authority       path        query   fragment
```

When you are creating a single-page app with [`Browser.fullscreen`][fs], you
use the [`Url.Parser`](Url-Parser) module to turn a `Url` into even nicer data.

If you want to create your own URLs, check out the [`Url.Builder`](Url-Builder)
module as well!

[fs]: /packages/elm/browser/latest/Browser#fullscreen

**Note:** This is a subset of all the full possibilities listed in the URI
spec. Specifically, it does not accept the `userinfo` segment you see in email
addresses like `tom@example.com`.
-}
data Url = Url
  { path :: String
  , query :: Maybe String
  }



-- PERCENT ENCODING


{-| **Use [Url.Builder](Url-Builder) instead!** Functions like `absolute`,
`relative`, and `crossOrigin` already do this automatically! `percentEncode`
is only available so that extremely custom cases are possible, if needed.
Percent-encoding is how [the official URI spec][uri] “escapes” special
characters. You can still represent a `?` even though it is reserved for
queries.
This function exists in case you want to do something extra custom. Here are
some examples:
    -- standard ASCII encoding
    percentEncode "hat"   == "hat"
    percentEncode "to be" == "to%20be"
    percentEncode "99%"   == "99%25"
    -- non-standard, but widely accepted, UTF-8 encoding
    percentEncode "$" == "%24"
    percentEncode "¢" == "%C2%A2"
    percentEncode "€" == "%E2%82%AC"
This is the same behavior as JavaScript's [`encodeURIComponent`][js] function,
and the rules are described in more detail officially [here][s2] and with some
notes about Unicode [here][wiki].
[js]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent
[uri]: https://tools.ietf.org/html/rfc3986
[s2]: https://tools.ietf.org/html/rfc3986#section-2.1
[wiki]: https://en.wikipedia.org/wiki/Percent-encoding
-}
percentEncode :: String -> String
percentEncode =
  String.toByteString >> URI.urlEncode False >> String.fromByteString


{-| **Use [Url.Parser](Url-Parser) instead!** It will decode query
parameters appropriately already! `percentDecode` is only available so that
extremely custom cases are possible, if needed.
Check out the `percentEncode` function to learn about percent-encoding.
This function does the opposite! Here are the reverse examples:
    -- ASCII
    percentDecode "99%25"     == Just "hat"
    percentDecode "to%20be"   == Just "to be"
    percentDecode "hat"       == Just "99%"
    -- UTF-8
    percentDecode "%24"       == Just "$"
    percentDecode "%C2%A2"    == Just "¢"
    percentDecode "%E2%82%AC" == Just "€"
Why is it a `Maybe` though? Well, these strings come from strangers on the
internet as a bunch of bits and may have encoding problems. For example:
    percentDecode "%"   == Nothing  -- not followed by two hex digits
    percentDecode "%XY" == Nothing  -- not followed by two HEX digits
    percentDecode "%C2" == Nothing  -- half of the "¢" encoding "%C2%A2"
This is the same behavior as JavaScript's [`decodeURIComponent`][js] function.
[js]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURIComponent
-}
percentDecode :: String -> Maybe String
percentDecode =
  String.toByteString >> URI.urlDecode False >> String.fromByteString >> Just


