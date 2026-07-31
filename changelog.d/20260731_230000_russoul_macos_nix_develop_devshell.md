<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

<!--
### Breaking

- A bullet item for the Breaking category.

-->

<!--
### Non-Breaking

- A bullet item for the Non-Breaking category.

-->

### Patch

- Fixed `nix develop` (and `devShells.default`/`ghc96` in general) being broken on non-Linux platforms (e.g. macOS/`aarch64-darwin`). The `exesOnly` CI-cost-reduction path in `nix/ci.nix` was accidentally also removing the `devShell` output on those platforms. The dev shell is now always available, while Hydra's `required` aggregate still excludes it on non-Linux platforms exactly as before, so CI build cost is unaffected.
