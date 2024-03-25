# 0.5.0.1

- Add partially lawful instances for Applicative, Monad, and related typeclasses

# 0.5

- Depend on `bifunctor-classes-compat` instead of `bifunctors`
  See changelog note in `bifunctors-5.6`: https://hackage.haskell.org/package/bifunctors-5.6/changelog
  This is breaking change, but affects only GHC-8.0 and older users.

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
