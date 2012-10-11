{-# LANGUAGE NoImplicitPrelude #-}

module Index where

import           Language.Fay.FFI
import           Language.Fay.Prelude

main :: Fay ()
main = log "x"

log :: Foreign f => f -> Fay ()
log = ffi "console.log(%1)"
