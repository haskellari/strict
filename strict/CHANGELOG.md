# 0.4.0.2 -- UNRELEASED

- Add a `boot` flag for a minimal dependency footprint. This is intended for
  testing purposes e.g. to break dependency cycles in GHC bootstrapping libs.

# 0.4.0.1

- Allow `bytestring-0.11`
- Remove duplicate `semigroups` dependency

# 0.4

- Add instances for type-classes in current `base`, `binary`, `deepseq` and `hashable`
- Add combinators mirroring `Data.Maybe`, `Data.Either` and `Data.Tuple`
- Add `Strict lazy strict` type-class with `toStrict` / `toLazy`
  conversion functions
- Modules are explicitly marked `Safe` or `Trustworthy`
- Add strict `These`
- `:!:` (`Pair` constructor) is non-associative
