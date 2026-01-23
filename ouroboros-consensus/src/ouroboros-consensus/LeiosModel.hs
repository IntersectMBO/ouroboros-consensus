{-# LANGUAGE LambdaCase #-}

module LeiosModel (module LeiosModel) where

-- * Block production

data Tx

data RankingBlock = LeiosRB Certificate | PraosBlock [Tx]

isLeiosRB :: RankingBlock -> Bool
isLeiosRB = undefined

data Certificate

blockTxs :: RankingBlock -> [Tx]
blockTxs = undefined

validateBlock :: RankingBlock -> Bool
validateBlock = undefined

data BlockHeader
  = PraosHeaderPlus
  { announcedEB :: Maybe (Announcement, Word)
  , certifiesEB :: Bool
  }

announceEB :: EndorserBlock -> Announcement
announceEB = undefined

sizeEB :: EndorserBlock -> Word
sizeEB = undefined

diffuseEBHeader :: [Peer] -> Announcement -> m ()
diffuseEBHeader = undefined

type Announcement = Hash EndorserBlock

data Hash a

data EndorserBlock
  = EndorserBlock
  { txReferences :: OrderedMap TxReference Word
  }

getEBContents :: EndorserBlock -> [TxReference]
getEBContents = undefined

makeEB :: [Tx] -> EndorserBlock
makeEB = undefined

data OrderedMap k v

hashEB :: EndorserBlock -> Hash EndorserBlock
hashEB = undefined

data TxReference

data Mempool

validateTx :: Mempool -> Tx -> ValidatedTx
validateTx = undefined

data ValidatedTx

addTx :: Mempool -> Tx -> Mempool
addTx = undefined

mempoolTxs :: Mempool -> m [Tx]
mempoolTxs = undefined

data Chain

data Slot

getTipCertificate :: Chain -> m (Maybe Certificate)
getTipCertificate = undefined

canBeIncluded :: Slot -> Certificate -> Bool
canBeIncluded = undefined

forgeBlocks ::
  Monad m =>
  Chain ->
  Mempool ->
  Slot ->
  m (BlockHeader, RankingBlock, Maybe EndorserBlock)
forgeBlocks c m s = do
  allTxs <- mempoolTxs m
  (rb, ebTxs) <-
    getTipCertificate c >>= \case
      Just cert
        | canBeIncluded s cert ->
            pure (LeiosRB cert, takeTxs paramMaxEBClosure allTxs)
      _ -> do
        let (rbTxs, ebTxs) = splitTxs paramMaxRB allTxs
        pure (PraosBlock rbTxs, ebTxs)

  case ebTxs of
    [] ->
      return
        ( PraosHeaderPlus
            { announcedEB = Nothing
            , certifiesEB = isLeiosRB rb
            }
        , rb
        , Nothing
        )
    _ -> do
      let eb = makeEB ebTxs
      return
        ( PraosHeaderPlus
            { announcedEB = Just (announceEB eb, sizeEB eb)
            , certifiesEB = isLeiosRB rb
            }
        , rb
        , Just eb
        )

data MaxBytes

paramMaxRB :: MaxBytes
paramMaxRB = undefined

paramMaxEBClosure :: MaxBytes
paramMaxEBClosure = undefined

splitTxs :: MaxBytes -> [Tx] -> ([Tx], [Tx])
splitTxs = undefined

takeTxs :: MaxBytes -> [Tx] -> [Tx]
takeTxs = undefined

-- * EB diffusion

data TxCache

knownTx :: TxCache -> Tx -> Bool
knownTx = undefined

data Peer

requestTx :: [Peer] -> TxReference -> m Tx
requestTx = undefined

offerEndorserBlock :: EndorserBlock -> [Peer] -> m ()
offerEndorserBlock = undefined

requestEndorserBlock :: [Peer] -> Announcement -> m EndorserBlock
requestEndorserBlock = undefined
