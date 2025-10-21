module Render where

import Prelude

import Data.Array (head, range, zip)
import Data.Foldable (sequence_)
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import MyPrelude (segmented)
import OfficialRoles as OfficialRoles
import Partial.Unsafe (unsafePartial)
import Role (Role)
import SortOrder (sorted)
import Web.DOM (Document, Element)
import Web.DOM.Document (createElement)
import Web.DOM.Element as Element
import Web.DOM.Node (appendChild)
import Web.DOM.Node as Node
import Web.HTML (window)
import Web.HTML.HTMLDocument (body, toDocument)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document)

render :: Effect Unit
render = do
  w <- window
  doc <- document w
  let d = toDocument doc
  mb <- body doc
  let r = unsafePartial $ (fromJust $ head OfficialRoles.roles)
      b = unsafePartial (fromJust mb)
  rN <- roleNode r d
  book <- bookNode d
  appendChild (Element.toNode rN) (Element.toNode (HTMLElement.toElement b))
  appendChild (Element.toNode book) (Element.toNode (HTMLElement.toElement b))
  
  pure unit


bookNode :: Document -> Effect Element
bookNode d = do
  let roles = sorted OfficialRoles.roles
      book = map (segmented 3) $ segmented 12 roles
  ul <- createElement "ul" d 
  let ulN = Element.toNode ul
  sequence_ 
    $ flip map (zip (range 1 1000) book) 
    $ \(Tuple pageNumber _page) -> do
      li <- createElement "li" d
      _subUl <- createElement "ul" d

      -- sequence_ $ map role

      let liN = Element.toNode li
      Node.setTextContent ("Writing page " <> show pageNumber) liN 
      appendChild liN ulN

  pure ul


roleNode :: Role -> Document -> Effect Element
roleNode r d = do
  e <- createElement "h2" d
  let eN = Element.toNode e
  Node.setTextContent r.name eN
  
  pure e
  