{ mkDerivation, aeson, base, bytestring, containers, geojson
, stdenv
}:
mkDerivation {
  pname = "StatMerge";
  version = "0.1.0.0";
  src = ./StatMerge;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers geojson
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
