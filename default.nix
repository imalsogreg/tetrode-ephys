{ mkDerivation, async, base, binary, bytestring, cereal, cmdargs
, containers, cpu, data-binary-ieee754, diagrams, diagrams-lib
, diagrams-rasterific, directory, gloss, GLURaw, hashable
, JuicyPixels, lens, OpenGLRaw, parsec, pipes, pipes-binary
, pipes-bytestring, pipes-parse, pipes-rt, safe, safecopy, stdenv
, stm, text, time, transformers, unordered-containers, vector
, vector-binary-instances
}:
mkDerivation {
  pname = "tetrode-ephys";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    async base binary bytestring cereal cmdargs containers cpu
    data-binary-ieee754 diagrams diagrams-lib diagrams-rasterific
    directory gloss GLURaw hashable JuicyPixels lens OpenGLRaw parsec
    pipes pipes-binary pipes-bytestring pipes-parse pipes-rt safe
    safecopy stm text time transformers unordered-containers vector
    vector-binary-instances
  ];
  homepage = "http://github.com/ImAlsoGreg/haskell-tetrode-ephys";
  description = "Haskell stuff for tetrodes, clusters, and stimuli";
  license = stdenv.lib.licenses.bsd3;
}
