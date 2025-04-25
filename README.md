# Go Terminal App

A terminal-based implementation of the board game Go, built with Haskell and the Brick TUI library.

## Features

- Interactive game board with coordinate labels
- Mouse and keyboard controls
- Visual highlighting for selections
- Pass and Resign options
- Stone capture mechanics
- Network play support (experimental)

## How to Play

- Use arrow keys to navigate the board
- Press Enter to select/place a stone
- Click on a position to select it
- Click Pass or Resign buttons to perform those actions
- Press Esc to exit the game

## Building and Running

```bash
# Build the application
stack build

# Run in local mode (default)
stack run

# Run in network mode as first player
stack run -- --network Player1 127.0.0.1 9001

# Run in network mode as second player connecting to first
stack run -- --network Player2 127.0.0.1 9002 127.0.0.1:9001
```

## Requirements

- GHC (Glasgow Haskell Compiler)
- Stack build tool
- Brick library
- Network dependencies (for multiplayer)
  - `gossip-hyparview` and `gossip-plumtree` packages
