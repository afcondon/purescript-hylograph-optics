# purescript-hylograph-optics

Optics (lenses, prisms, traversals) for Hylograph data structures.

## Overview

Provides optics for navigating and transforming rose trees (`Data.Tree` from `tree-rose`) and `hylograph-graph` graphs. Built on `profunctor-lenses`. These are optics over the *data* a visualization is made from; there are none over HATS trees.

## Installation

```bash
spago install hylograph-optics
```

## Modules

- `Hylograph.Optics` - Re-exports common optics
- `Hylograph.Optics.Tree` - Optics for rose trees
- `Hylograph.Optics.Graph` - Optics for graph structures

## Example

```purescript
import Data.Lens (over, view, toListOf)
import Data.Tree (Tree)
import Hylograph.Optics.Tree (_root, _leaves)

-- Focus on the root value
rootLabel :: Tree String -> String
rootLabel = view _root

-- Modify every leaf
shout :: Tree String -> Tree String
shout = over _leaves (_ <> "!")

-- Collect the leaves
leaves :: Tree String -> List String
leaves = toListOf _leaves
```

## Part of Hylograph

- **hylograph-optics** - Optics utilities (this package)
- **hylograph-graph** - Graph data structures
- **hylograph-layout** - Layout algorithms

## License

MIT
