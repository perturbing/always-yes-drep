{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
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

{-# HLINT ignore "Use null"                     #-}

module Scripts where

import PlutusLedgerApi.V3 (
    Redeemer (..),
    ScriptContext (..),
    ScriptInfo (..),
    ScriptPurpose (..),
    TxInfo (..),
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
    ($),
    (.),
    (==),
 )
import Shared (wrapFourArgs, wrapOneArg, wrapThreeArgs, wrapTwoArgs)

{-# INLINEABLE alwaysVoteYesDrep #-}
alwaysVoteYesDrep :: ScriptContext -> Bool
alwaysVoteYesDrep ctx = case scriptContextScriptInfo ctx of
    VotingScript voter -> case lookup voter (txInfoVotes txInfo) of
        Just votes -> all (== VoteYes) votes
        Nothing -> False
      where
        txInfo = scriptContextTxInfo ctx
    _ -> False

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
