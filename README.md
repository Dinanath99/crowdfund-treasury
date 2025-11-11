# Crowdfund Treasury

Cardano blockchain pe ek basic Plutus V3 smart contract crowdfunding ke liye. Log ADA bhej sakte hain script address pe. Agar funding goal deadline tak mil jata hai, to creator funds collect karta hai. Nahi to, contributors refund le sakte hain.

## Setup
- Haskell, Cabal, aur Cardano node chahiye.
- Cardano preview network pe test karo.
- `test-keys/` mein keys (alice.skey, bob.skey, ding.skey) chahiye.

## Commands
- `cabal run SC check`: Setup check kare.
- `cabal run SC cbor-writer`: Script ka CBOR file banaye.
- `cabal run SC sAddr`: Script address dekhe.
- `cabal run SC fund`: ADA contribute kare.
- `cabal run SC collect`: Creator funds collect kare.
- `cabal run SC refund`: Contributors refund le.