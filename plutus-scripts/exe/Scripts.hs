{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}

module Scripts where

import PlutusLedgerApi.V3 (
    Redeemer (..),
    ScriptContext (..),
    ScriptInfo (..),
    ScriptPurpose (..),
    TxCert (..),
    TxInInfo (..),
    TxInfo (..),
    TxOutRef (..),
    Vote (..),
    fromBuiltinData,
 )
import PlutusTx (
    CompiledCode,
    compile,
    makeIsDataIndexed,
 )
import PlutusTx.AssocMap (
    all,
    lookup,
 )
import PlutusTx.Bool (
    Bool (..),
    otherwise,
    (&&),
 )
import PlutusTx.Builtins (
    BuiltinByteString,
    BuiltinData,
    Integer,
    error,
 )
import PlutusTx.Prelude (
    BuiltinUnit,
    Maybe (..),
    find,
    ($),
    (.),
    (==),
 )
import Shared (wrapFourArgs, wrapOneArg, wrapThreeArgs, wrapTwoArgs)

{-# INLINEABLE findTxInByTxOutRef #-}
findTxInByTxOutRef :: TxOutRef -> TxInfo -> Maybe TxInInfo
findTxInByTxOutRef outRef TxInfo{txInfoInputs} =
    find
        (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == outRef)
        txInfoInputs

{-# INLINEABLE alwaysVoteYesDrep #-}
alwaysVoteYesDrep :: ScriptContext -> Bool
alwaysVoteYesDrep ctx = case scriptContextScriptInfo ctx of
    -- If the drep is delegated, only allow the credential to vote yes
    VotingScript voter -> case lookup voter (txInfoVotes txInfo) of
        Just votes -> all (== VoteYes) votes
        Nothing -> False
    -- If this script is certifying, only allow the credential to Register and Update the Drep
    CertifyingScript _ cert -> case cert of
        TxCertRegDRep _ _ -> True
        TxCertUpdateDRep _ -> True
        _ -> False
    _ -> False
  where
    txInfo = scriptContextTxInfo ctx

{-# INLINEABLE wrappedAlwaysVoteYesDrep #-}
wrappedAlwaysVoteYesDrep :: BuiltinData -> BuiltinUnit
wrappedAlwaysVoteYesDrep = wrapOneArg alwaysVoteYesDrep

alwaysVoteYesDrepCode :: CompiledCode (BuiltinData -> BuiltinUnit)
alwaysVoteYesDrepCode = $$(compile [||wrappedAlwaysVoteYesDrep||])

-- Testing purposes

{-# INLINEABLE alwaysTrueMint #-}
alwaysTrueMint :: BuiltinData -> Bool
alwaysTrueMint _ = True

{-# INLINEABLE wrappedAlwaysTrueMint #-}
wrappedAlwaysTrueMint :: BuiltinData -> BuiltinUnit
wrappedAlwaysTrueMint = wrapOneArg alwaysTrueMint

alwaysTrueMintCode :: CompiledCode (BuiltinData -> BuiltinUnit)
alwaysTrueMintCode = $$(compile [||wrappedAlwaysTrueMint||])
