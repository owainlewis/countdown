# Countdown

Haskell study from Graham Huttons papers and video. Also adds a solver for the word game.

TODO word game is inefficient and needs to use the Trie properly

A simple optimization would be to ignore words less than 4 letters because they're unlikely to win the game anyway.
This means we have far few combinations to work through (even with the brute force solver)

## Word Game

```haskell

import Words ( bestWord, solve )

-- Find the best word solution to win the game

bestWord "ojonased"

-- Get all results

solve "ojonased"

-- ["dojo","dojo","jeon","soja","joes","onos","onos","soon","soon","naos",
-- "aeon","dona","ones","nose","noes","sone","eons","nods","dons","node",
-- "done","odas","ados","soda","odea","odes","dose","does","jeon","soja",
-- "joes","jane","jean","jade","naos","aeon","dona","ones","nose","noes",
-- "sone","eons","nods","dons","node","done","odas","ados","soda","odea",
-- "odes","dose","does","anes","sane","ands","sand","dans","dean","sned",
-- "send","ends","dens","sade","dojos","dojos","jones","noose","noose",
-- "snood","snood","odeon","odeon","aeons","donas","anode","nosed","nodes",
-- "sonde","jones","janes","jeans","jades","aeons","donas","anode","nosed",
-- "nodes","sonde","saned","sedan","deans","odeons","odeons","noosed",
-- "nodose","noosed","nodose","anodes","anodes"]
```
