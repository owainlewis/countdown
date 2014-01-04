# Countdown

Haskell study from Graham Huttons papers and video. Also adds a solver for the word game.

TODO word game is inefficient and needs to use the Trie properly

A simple optimization would be to ignore words less than 5 letters because they're unlikely to win the game anyway.

This means we have far few combinations to work through (even with the brute force solver)

## Word Game

```haskell
import Words ( bestWord, solve )
bestWord "ojonased"

-- Get all results
solve "ojonased"

["jo","on","no","os","so","oe","od","do","jo","on","no","os","so","oe","od","do",
  "na","an","ne","en","as","ae","ad","es","ed","de","joe","ono","ono","noo","noo",
  "ons","nos","son","one","eon","nod","don","oda","ado","ose","oes","ods","sod","dos",
  "ode","doe","joe","ons","nos","son","one","eon","nod","don","oda","ado","ose","oes",
  "ods","sod","dos","ode","doe","nae","ane","and","dan","sen","ens","end","den","sae","sea",
  "ads","sad","eds","dojo","dojo","jeon","soja","joes","onos","onos","soon","soon","naos","aeon",
  "dona","ones","nose","noes","sone","eons","nods","dons","node","done","odas","ados","soda","odea",
  "odes","dose","does","jeon","soja","joes","jane","jean","jade","naos","aeon","dona","ones","nose","noes",
  "sone","eons","nods","dons","node","done","odas","ados","soda","odea","odes","dose","does","anes","sane",
  "ands","sand","dans","dean","sned","send","ends","dens","sade","dojos","dojos","jones","noose","noose","snood",
  "snood","odeon","odeon","aeons","donas","anode","nosed","nodes","sonde","jones","janes","jeans","jades","aeons",
  "donas","anode","nosed","nodes","sonde","saned","sedan","deans","odeons","odeons","noosed",
  "nodose","noosed","nodose","anodes","anodes"]
```
