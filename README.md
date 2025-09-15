# miso-sample-app

Sample client-side app using Miso, with wasm backend + vscode config +
github CI + docker deployment.

[try online](https://juliendehos.github.io/miso-sample-app)


## setup

- install Nix Flakes

- install Cachix

- use miso's cachix:

```sh
cachix use haskell-miso-cachix
```


## build and run (wasm)

```
nix develop .#wasm --command bash -c "make && make serve"
```

or (dev):

```
nix develop .#wasm
make build && make serve
```


## build and run (docker)

```
nix develop .#wasm --command bash -c "make"
nix-build docker.nix
docker load < result
docker run --rm -it -p 3000:3000 miso-sample-app:latest
```


## edit with vscode

```
nix-shell
code .
```

