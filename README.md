# Plutus Platform starter project

This project gives a simple starter project for using the Plutus Platform.

## Setting up

For now, the only supported tooling setup is to use the provided VSCode devcontainer to get an environment with the correct tools set up.

- Install Docker
- Install VSCode
  - Install the [Remote Development extension pack](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.vscode-remote-extensionpack)
  - You do *not* need to install the Haskell extension
- Clone this repository and open it in VSCode
  - It will ask if you want to open it in the container, say yes.
  - The first time it will take a few minutes to download the devcontainer image from dockerhub,
  - `cabal build` from the terminal should work
  - Opening a Haskell file should give you IDE features (it takes a little while to set up the first time)

Note: This uses the [plutus-starter-devcontainer image on dockerhub](https://hub.docker.com/r/inputoutput/plutus-starter-devcontainer), if
you wish to build the image yourself, you can do so as follows:
  - Clone https://github.com/input-output-hk/plutus ,
  - Set up your machine to build things with Nix, following the Plutus README (make sure to set up the binary cache!),
  - Build and load the docker container: `docker load < $(nix-build default.nix -A devcontainer)`,
  - Adjust the `.devcontainer/devcontainer.json` file to point to your local image.

## The Plutus Application Backend (PAB) example

We have provided an example PAB application in `./pab`. With the PAB we can serve and interact
with contracts over a web API. You can read more about the PAB here: [PAB Architecture](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/ARCHITECTURE.adoc).

Here, the PAB is configured with one contract, the `Game` contract from `./examples/src/Plutus/Contracts/Game.hs`.

Here's an example of running and interacting with this contract via the API. For this it will help if you
have `jq` installed.

1. Build the PAB executable:

```
cabal build plutus-starter-pab
```

2. Run the PAB binary:

```
cabal exec -- plutus-starter-pab
````

This will then start up the server on port 8080. The devcontainer process will then automatically expose this port so that you can connect to it from any terminal (it doesn't have to be a terminal running in the devcontainer).

First, let's verify that the game is present in the server:

3. Check what contracts are present:

```
curl -s http://localhost:8080/api/new/contract/definitions | jq
```

You should receive a list of contracts and the endpoints that can be called on them, and the arguments
required for those endpoints.

We're interested in the `GameContract` one.

#### Playing the guessing game over the API

The game has two players (wallets). One will initialise the contract and lock a value inside. Another
wallet will then make guesses. Supposing they guess correctly, they'll receive the funds that were
locked; otherwise, they won't!

1. Start the instances:

```
# Wallet 1
curl -s -H "Content-Type: application/json" \
  --request POST \
  --data '{"caID": "GameContract", "caWallet":{"getWallet": 1}}' \
  http://localhost:8080/api/new/contract/activate | jq

# Wallet 2
curl -s -H "Content-Type: application/json" \
  --request POST \
  --data '{"caID": "GameContract", "caWallet":{"getWallet": 2}}' \
  http://localhost:8080/api/new/contract/activate | jq
```

From these two queries you will get back two contract instance IDs. These will be needed
in the subsequent steps for running actions against. We can optionally take a look at the state
of the contract with the `status` API:

2. Get the status

```
export INSTANCE_ID=...
curl -s http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/status | jq
```

This has a lot of information; and in particular we can see what endpoints are still available
to call.

3. Start the game by locking some value inside

Now, let's call the `lock` endpoint to start the game. In order to do so, we need to construct
a JSON representation of the `LockParams` that the endpoint takes (look at `Game.hs`). The easiest
way is to simply build the term in haskell and ask `aeson` to encode it. From the terminal:

```
cabal repl
> import Plutus.Contracts.Game
> import Ledger.Ada
> args = LockParams { secretWord = "eagle", amount = lovelaceValueOf 90 }
> import Data.Aeson
> import Data.ByteString.Lazy.Char8 as BSL
> BSL.putStrLn $ encode args
{"amount":{"getValue":[[{"unCurrencySymbol":""},[[{"unTokenName":""},90]]]]},"secretWord":"eagle"}
```

Great! This is all we need to call the `lock` endpoint, so let's do that now with
the instance from Wallet 1:

4. Lock some value (Wallet 1)

```
export INSTANCE_ID=...
curl -H "Content-Type: application/json" \
  --request POST \
  --data '{"amount":{"getValue":[[{"unCurrencySymbol":""},[[{"unTokenName":""},90]]]]},"secretWord":"eagle"}' \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/endpoint/lock
```

We can do likewise to work out what the JSON for `GuessParams` is, and then make a guess from
Wallet 2:

5. Make a guess (Wallet 2)

```
export INSTANCE_ID=...
curl -H "Content-Type: application/json" \
  --request POST \
  --data '{"guessWord": "duck"}' \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/endpoint/guess
```

Note that this guess is wrong, so in the log of the server we will see that the transaction
didn't validate.

As an exercise, you can now spin up another instance for Wallet 2 and make a correct guess, and
confirm that the transaction validates and the Ada is transferred into the right wallet.

Note that you can verify the balances by looking at the log of `plutus-starter-pab` 
when exiting it by pressing return.

Finally, also node that the PAB also exposes a websocket, which you can read about in
the general [PAB Architecture documentation](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/ARCHITECTURE.adoc).
