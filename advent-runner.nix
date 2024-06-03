{ mkDerivation, base, fetchgit, lib, timeit }:
mkDerivation {
  pname = "advent-runner";
  version = "0.1.1.0";
  src = fetchgit {
    url = "https://github.com/octorine/advent-runner.git";
    sha256 = "0ljxjvawqz608lrhvnpnfmapcvfwhj84mdqwisxdfsqai60v89a3";
    rev = "4551d4130a322900a53f1a34fde8d334577c08d1";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base timeit ];
  testHaskellDepends = [ base ];
  description = "A library to help with writing and running solutions to Advent of Code";
  license = lib.licenses.mit;
}
