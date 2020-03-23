{ obelisk ? import ./.obelisk/impl {
    system = builtins.currentSystem;
    iosSdkVersion = "10.2";
    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;
  }
}:
with obelisk;
project ./. ({ pkgs, hackGet, ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";

  overrides = self: super: with pkgs.haskell.lib;
  let beam-src = pkgs.fetchFromGitHub {
        owner = "tathougies";
        repo = "beam";
        rev = "ff6d16daa189355db4cf24e90d8173768c1418bb";
        sha256 = "11f1nrw3s7ihf3lyskjx1mdfi4s5d3rfn0fwwmcc8xl2dgjdlnk8";
      };
  in {
    beam-core = dontCheck (self.callCabal2nix "beam-core" "${beam-src}/beam-core" {});
    beam-migrate = doJailbreak (dontCheck (self.callCabal2nix "beam-migrate" "${beam-src}/beam-migrate" {}));
    beam-postgres = dontCheck (self.callCabal2nix "beam-postgres" "${beam-src}/beam-postgres" {});
    formattable = doJailbreak (dontCheck (self.callHackageDirect {
      pkg = "formattable";
      ver = "0.1.1";
      sha256 = "12ivb374zymkqzq3w9a9vhxbri5bpymi1di6kk45hp2f6b8lafpz";
    } {}));
    lens-csv = self.callCabal2nix "lens-csv" (pkgs.fetchFromGitHub {
      owner = "ChrisPenner";
      repo = "lens-csv";
      rev = "1bcac70074a25bc9b7fb2728fc47b8ea5c6a0e9b";
      sha256 = "1lmjwm4xfjh2p7mqnzpndp3d8h4ka9xk4aixar8z3q4zci8z3vmq";
    }) {};
  };
})
