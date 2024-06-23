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
    fromBuiltinData,
 )
import PlutusTx (
    CompiledCode,
    compile,
    makeIsDataIndexed,
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

{-# INLINEABLE alwaysTrueMint #-}
alwaysTrueMint :: BuiltinData -> Bool
alwaysTrueMint _ = True

{-# INLINEABLE wrappedAlwaysTrueMint #-}
wrappedAlwaysTrueMint :: BuiltinData -> BuiltinUnit
wrappedAlwaysTrueMint = wrapOneArg alwaysTrueMint

alwaysTrueMintCode :: CompiledCode (BuiltinData -> BuiltinUnit)
alwaysTrueMintCode = $$(compile [||wrappedAlwaysTrueMint||])
