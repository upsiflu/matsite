# Articles

The Elm produces a list of DOM nodes, one per segment, without hierarchies.

Each node stores its own **offset**, as css variables:
- `screens` screen-wide (only segments with body)
- `columns` column-wide (only segments with body)
- `headers` one per segment (only segments without body)
- `units`   one per segment

as well as its own **width**:
- `ownScreens` screen-wide (only segments with body)
- `ownColumns` column-wide (only segments with body)
- `ownHeaders` one per segment (only segments without body)


The CSS is responsible for transitions between regions.

# Regions

north, south,
west, nearWest,
center, peek (shown only when the focus has no body), cache (hidden!),
nearEast, east

For each region, Elm provides a total length:
- north-units
- west-units
- nearWest-screens
- nearWest-columns
- nearWest-headers

# Screen Layout

Depending on the orientation of the focus, whether it has a body or not, and whether it is the background, we can distinguish six layouts:


| Orien.     | Focus situation | F, A width | Peek | Extent
| ------ | -----           | -------- | --- | ---
| `.🪗🀱 | :not(.focusHasBody)` | units | yes | infinite
| `.🪗🁣 | :not(.focusHasBody)`  | units | yes | infinite
| `.🪗🀱 | .focusHasBody `      | c+s+h | no | infinite
| `.🪗🁣 | .focusHasBody`        | c+s+h | no | infinite
| `.🪗🀱 | .focusIsBackground`  | units | no | screen
| `.🪗🁣 | .focusIsBackground`   | units | no | screen


As you can see in the table, the widths of Focus and Aisles depend on whether the focus has a body.
- The center region is one column wide if the focus has no body but will extend with the focus width if it does.

# Article Placement

Conceptually, we add two vectors: first, the region origin,
second, the segment offset. We use coordinates relative to the accordion's top left so that the positions can be easily transitioned (animated).

The region vectors depend on the orientation and the extent.

- South and North are outside the screen in _infinite_ extents and at the edges in _screen_ extents.
- West, NearWest, NearEast and East extend outwards from the center region in _infinite_ extents, but gather at the edges in _screen_ extents.
- In _horizontal_ layouts, the whole hereHeight is dedicated to the Focus, whereas in _vertical_ layouts, the NearWest and NearEast regions live atop, resp. below the Here.