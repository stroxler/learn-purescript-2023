# A Monad Transformers Adventure game

This small app provides an intro to
- Options.Applicative for cli interfaces
- The Readline library
- Monad transformer stacks

There's also a basic parser combinator, sort of,
which I only realized after the fact isn't needed
for the game (it was part of the Purescript By Example
code in Chapter 11 so I included it).

A typical game session with `spago run`:
```
> spago run --exec-args '-p stroxler -d'
[info] Build succeeded.
[warn] None of your project files import modules from some projects that are in the direct dependencies of your project.
These dependencies are unused. To fix this warning, remove the following packages from the list of dependencies in your config:
- test-unit
adventure-game> describe
I don't understand that command
adventure-game> look
You are at (0, 0)
You are in a dark forest. You can see a path to the north.
You can see the Matches.
adventure-game> take
I don't understand that command
adventure-game> take matches
I don't know what item you are referring to
adventure-game> take Matches
You now have Matches
adventure-game> north
adventure-game> debug
{ inventory: (fromFoldable [Matches]), items: (fromFoldable [(Tuple { x: 0, y: 0 } (fromFoldable [])),(Tuple { x: 0, y: 1 } (fromFoldable [Candle]))]), playerPosition: { x: 0, y: 1 } }
adventure-game> look
You are at (0, 1)
You are in a clearing.
You can see the Candle.
adventure-game> take Candle
You now have Candle
adventure-game> use Candle
I don't know what to do with just a candle.
adventure-game> use Matches
You have lit the candle with the matches.
Congratulations, { debugMode: true, playerName: "stroxler" }
You win!
adventure-game>
```
