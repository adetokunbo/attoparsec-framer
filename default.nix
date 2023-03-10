let
  # Pin haskell-nix to a recent (as of 2022/11/23) commit
  h8x-pin = "https://github.com/input-output-hk/haskell.nix/archive/983c6d74b9d2de9bb195392974c4b861a78160e2.tar.gz";
  h8x-src = builtins.fetchTarball h8x-pin;
  h8x = import h8x-src {};

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import

    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these. But you
    # can also just use your own, e.g. '<nixpkgs>'.

    h8x.sources.nixpkgs-2205

    # These arguments passed to nixpkgs, include some patches and also the
    # haskell.nix functionality itself as an overlay.

    h8x.nixpkgsArgs;

in pkgs.haskell-nix.cabalProject {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "attoparsec-framer";
    src = ./.;

  };

  # Specify the GHC version to use.
  compiler-nix-name = "ghc8107";

  # Specify the hackage index state
  index-state = "2022-11-23T00:00:00Z";

  modules = [
    { enableProfiling = true;
      enableLibraryProfiling = true;
    }
  ];
}
