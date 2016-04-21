module Handler.Home where

import Import

getHomeR :: Handler ()
getHomeR = redirect $ StaticR index_html

