{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, FlexibleContexts #-}
module DepTree
( depTreeApp
) where

import Update (DepTree)

import Control.Eff (Eff, SetMember, Member)
import Control.Eff.Lift (Lift, lift)
import Control.Concurrent.STM (TVar, readTVar, atomically)
import qualified Control.Exception (Exception)

import Data.Reflection (Given, given)
import qualified Data.List as List (lookup, elem, foldl')
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as B (unpack)
import qualified Data.ByteString.Lazy as L (ByteString, length, fromStrict)
import qualified Data.Text.Lazy as TL (Text)
import qualified Data.Text.Lazy.Encoding as TL (decodeUtf8, encodeUtf8)
import qualified Data.HashMap.Strict as HM (lookup)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)

import Data.GraphViz (DotGraph(..), DotStatements(..), DotNode(..), DotEdge(..), GraphID(..))
import Data.GraphViz.Commands (GraphvizCommand(Dot), GraphvizOutput(Svg), graphvizWithHandle)
import Data.GraphViz.Commands.IO (hGetStrict)

import qualified Network.Wai as Wai (Request, Response)

import Wf.Application.Exception (Exception, throwException)
import Wf.Network.Http.Response (defaultResponse, setContentType, setContentLength)
import Wf.Network.Wai (toWaiResponse)
import Wf.Web.Api (ApiInfo(..))


depTreeApp
    :: (Given ApiInfo, SetMember Lift (Lift IO) r, Member Exception r)
    => TVar DepTree
    -> Wai.Request
    -> Eff r Wai.Response
depTreeApp tv _ = do
    package <- getUrlParam "package"
    (n, e) <- lift . atomically $ (readTVar tv >>= return . depTree package)
    body <- lift $ toSvg package n e
    return . toWaiResponse . setContentType "image/svg+xml" . setContentLength (fromIntegral $ L.length body) $ defaultResponse body

    where
    depTree name dtree = pickNodesAndEdges dtree ([], [])  name
    throwError s = throwException . Error $ s
    params = apiInfoParameters given
    getUrlParam a = case List.lookup a params of
        Just x -> return x
        Nothing -> throwError "no url parameter"


pickNodesAndEdges :: DepTree -> ([B.ByteString], [(B.ByteString, B.ByteString)]) -> B.ByteString -> ([B.ByteString], [(B.ByteString, B.ByteString)])
pickNodesAndEdges dtree (nodes, edges) cur
    | List.elem cur nodes = (nodes, edges)
    | otherwise = List.foldl' (pickNodesAndEdges dtree) (nodes', edges') ds
        where
        ds = fromMaybe [] $ HM.lookup cur dtree
        nodes' = cur : nodes
        edges' = map ((,) cur) ds ++ edges


toGraphviz :: TL.Text -> [String] -> [(String, String)] -> DotGraph String
toGraphviz name nodes edges = DotGraph
    { strictGraph = True
    , directedGraph = True
    , graphID = Just (Str name)
    , graphStatements =
        DotStmts
        { attrStmts = []
        , subGraphs = []
        , nodeStmts = map toDotNode nodes
        , edgeStmts = map toDotEdge edges
        }
    }
    where
    toDotNode a = DotNode a []
    toDotEdge (a, b) = DotEdge a b []

toSvg :: B.ByteString -> [B.ByteString] -> [(B.ByteString, B.ByteString)] -> IO L.ByteString
toSvg name nodes edges = fmap TL.encodeUtf8 $ graphvizWithHandle Dot g Svg hGetStrict
    where
    name' = TL.decodeUtf8 . L.fromStrict $ name
    nodes' = map B.unpack nodes
    edges' = map (\(a, b) -> (B.unpack a, B.unpack b)) edges
    g = toGraphviz name' nodes' edges'


data Error = Error String deriving (Eq, Show, Typeable)

instance Control.Exception.Exception Error
