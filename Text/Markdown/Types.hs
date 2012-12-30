module Text.Markdown.Types where

import Data.Text (Text)
import Data.Default (Default (def))
import Data.Set (Set, empty)

-- | A settings type providing various configuration options.
--
-- See <http://www.yesodweb.com/book/settings-types> for more information on
-- settings types. In general, you can use @def@.
data MarkdownSettings = MarkdownSettings
    { msXssProtect :: Bool
      -- ^ Whether to automatically apply XSS protection to embedded HTML. Default: @True@.
    , msStandaloneHtml :: Set Text
      -- ^ HTML snippets which stand on their own. We do not require a blank line following these pieces of HTML.
      --
      -- Default: empty set.
      --
      -- Since: 0.1.2
    }

instance Default MarkdownSettings where
    def = MarkdownSettings
        { msXssProtect = True
        , msStandaloneHtml = empty
        }
