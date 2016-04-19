module Handler.Home where

import Import

getHomeR :: Handler ()
getHomeR = redirect index_html

