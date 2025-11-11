Crowdfund Treasury

A basic Plutus V3 smart contract on the Cardano blockchain for crowdfunding. People can send ADA to the script address. If the funding goal is reached before the deadline, the creator can collect the funds. Otherwise, contributors can claim a refund.

Setup

Requires Haskell, Cabal, and a running Cardano node.

Test on the Cardano Preview network.

You need keys inside the test-keys/ folder (alice.skey, bob.skey, ding.skey).

Commands

cabal run SC check: Checks the setup.

cabal run SC cbor-writer: Generates the CBOR file for the script.

cabal run SC sAddr: Shows the script address.

cabal run SC fund: Contribute ADA to the script.

cabal run SC collect: Creator collects the funds.

cabal run SC refund: Contributors claim a refund.
