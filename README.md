<h1 align="center">
    🧫 Life Engine
</h1>

<p align="center">
    <img src="assets/bacteria.png" width="200">
</p>

<p align="center">
    Small game about evolution of organisms written in Haskell
</p>

-   [Rules](#rules)
-   [Examples](#examples)
-   [Build](#build)

## Rules

### Cells

The world is a grid made up of square cells.
There can be different types of cells.
Organisms are structures of different anatomy cells.

Here are all anatomy cells

-   **Mouth** - the most important cell which eats food in directly adjacent coordinates. Every organism needs to eat as much food as there are cells in its body in order to reproduce. It's the only cell that is essential for every organism.
-   **Producer** - this cell generates food in adjacent cells. Every frame it has a small random chance of producing a food cell around it. That random chance can be tuned.
-   **Mover** - mover cell allows an organism to move and rotate randomly. Organism just needs one mover cell to move, adding more doesn't make it faster. Organisms with mover cells can't produce food even if they have producer cells, but it can be changed.
-   **Killer** - this cell harms other organisms when it touches them in directly adjacent cells
-   **Armor** - this cell defends against the killer cell simply by ignoring its damage.
-   **Eye** - the eye allows an organism to see and alter its movement based on its perceptions

### Lifecycle and Behavior

> Will be added later

## Examples

> Will be added later

## Build

Run
`cabal run`
to start the game
