# purescript-hylograph-optics

Optics (lenses, prisms, traversals) for Hylograph data structures.

## Overview

Provides optics for navigating and transforming tree and graph data structures used in Hylograph visualizations. Built on `profunctor-lenses`.

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
import Hylograph.Optics.Tree (nodeValue, children)

-- Focus on root value
root = view nodeValue myTree

-- Modify all children
updated = over children (map transform) myTree
```

## Part of Hylograph

- **hylograph-optics** - Optics utilities (this package)
- **hylograph-graph** - Graph data structures
- **hylograph-layout** - Layout algorithms

## License

MIT
