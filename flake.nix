{

  inputs = {
    miso.url = "github:dmjio/miso/1.11.0";
  };

  outputs = inputs:
    inputs.miso.inputs.flake-utils.lib.eachDefaultSystem (system: {
      devShells = {
        default = inputs.miso.outputs.devShells.${system}.default;
        native = inputs.miso.outputs.devShells.${system}.native;
        wasm = inputs.miso.outputs.devShells.${system}.wasm;
      };
    });
}
