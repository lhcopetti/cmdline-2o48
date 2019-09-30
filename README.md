# cmdline-2o48

A command line implementation for the famous 2048 game. Unlike its counterpart, this game actually has an ending so you don't have to keep playing until your fingers wear off.

### Goals

* Write a game in the command-line
* Be more confortable around IO
* Have a big monad stack
* Handle stateful computations in FP

### Screenshots


#### Start of the Game
![alt text][start-game]

#### Middle Game
![alt text][middle-game]

#### Entering custom board
![alt text][enter-custom-board]

#### Custom board loaded
![alt text][load-custom-board]

#### How to build and run

To build the project, run:

```
stack build
```

The tests can be run with:

```
stack test
```

If everything is good, then attempt to run it on your terminal:

```
stack exec cmdline-2o48-exe
```

[start-game]: https://github.com/lhcopetti/cmdline-2o48/raw/master/doc/start-game.png "Start of the Game"
[middle-game]: https://github.com/lhcopetti/cmdline-2o48/raw/master/doc/middle-game.png "Middle of the Game"
[enter-custom-board]: https://github.com/lhcopetti/cmdline-2o48/raw/master/doc/enter-custom-board.png "Enter custom board"
[load-custom-board]: https://github.com/lhcopetti/cmdline-2o48/raw/master/doc/load-custom-board.png "Load custom board"
