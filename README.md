# Plutus Platform starter project

This project gives a simple starter project for using the Plutus Platform.

## Setting up

For now, the only supported tooling setup is to use the provided VSCode devcontainer to get an environment with the correct tools set up.

- Install Docker
- Install VSCode
  - Install the [Remote Development extension pack](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.vscode-remote-extensionpack)
  - You do *not* need to install the Haskell extension
- Get the docker image (for now, we need to build this with Nix)
  - Clone https://github.com/input-output-hk/plutus 
  - Set up your machine to build things with Nix, following the Plutus README (make sure to set up the binary cache!)
  - Build and load the docker container: `docker load < $(nix-build default.nix -A devcontainer)`
- Clone this repository and open it in VSCode
  - It will ask if you want to open it in the container, say yes.
  - `cabal build` from the terminal should work
  - Opening a Haskell file should give you IDE features (it takes a little while to set up the first time)
