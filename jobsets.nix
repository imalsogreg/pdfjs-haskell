{ prs, nixpkgs }:

let
  pkgs = import nixpkgs {};
  mkFetchGithub = value: {
    inherit value;
    type = "git";
    emailresponsible = false;
  };
in
with pkgs.lib;
let
  defaults = jobs: {
    inherit (jobs) description;
    enabled = 1;
    hidden = false;
    keepnr = 10;
    schedulingshares = 100;
    checkinterval = 120;
    enableemail = false;
    emailoverride = "";
    nixexprinput = "pdfjs-haskell";
    nixexprpath = "release.nix";
    type = 0;
    inputs = jobs.inputs // {
      nixpkgs = {
        type = "git";
        value = "https://github.com/WebGHC/nixpkgs a5cc7b77c090ede3ac380962ea876b83d847592c";
        emailresponsible = false;
      };
      reflex-platform = {
        type = "git";
        value = "https://github.com/reflex-frp/reflex-platform c21ec7036c8351185cd2feef9ce577b6a61c4f22";
        emailresponsible = false;
      };
    };
  };
  branchJobset = branch: defaults {
    description = "pdfjs-haskell-${branch}";
    inputs = {
      pdfjs-haskell = {
        value = "https://github.com/imalsogreg/pdfjs-haskell ${branch}";
        type = "git";
        emailresponsible = false;
      };
    };
  };
  makePr = num: info: {
    name = "pdfjs-haskell-pr-${num}";
    value = defaults {
      description = "#${num}: ${info.title}";
      inputs = {
        pdfjs-haskell = {
          #NOTE: This should really use "pull/${num}/merge"; however, GitHub's
          #status checks only operate on PR heads.  This creates a race
          #condition, which can currently only be solved by requiring PRs to be
          #up to date before they're merged.  See
          #https://github.com/isaacs/github/issues/1002
          value = "https://github.com/imalsogreg/pdfjs-haskell pull/${num}/head 1";
          type = "git";
          emailresponsible = false;
        };
      };
    };
  };
  processedPrs = mapAttrs' makePr (builtins.fromJSON (builtins.readFile prs));
  jobsetsAttrs = processedPrs //
    genAttrs ["master" "hydra-wip" "qa"] branchJobset;
in {
  jobsets = pkgs.writeText "spec.json" (builtins.toJSON jobsetsAttrs);
}
