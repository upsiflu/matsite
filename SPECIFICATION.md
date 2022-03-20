**Specification of the interactivity and layout**

- [Segments](#segments)
  - [Layout of a Segment](#layout-of-a-segment)
  - [Interactivity of a Segment](#interactivity-of-a-segment)
- [Accordion](#accordion)
  - [Url](#url)
  - [Layout of the Accordion](#layout-of-the-accordion)
    - [Folding a Branch](#folding-a-branch)
    - [Folding a Tree](#folding-a-tree)
- [Animations](#animations)
- [Future features](#future-features)

---

# Segments

A Segment consists of a unique **id**, an optional **caption**, and an optional **body**. It also has an **orientation** (vertical or horizontal). Adjacent segments should have the same orientation, but this is not enforced by the data model. Segments do not change through navigation, only through editing. Their data is stored as indexed by their id.

## Layout of a Segment

The `ViewMode` determines how Segments are rendered differently, depending on their position in the tree, and on the Accordion's state.

- When a Segment is _collapsed_, only the caption will display; when _expanded_, both caption and body (if any) will display. For now, the Accordion decides on this mode. Besides **Default** and **Collapsed**, there is a third mode, namely **Placeholder**, which causes the Segment to render as a buffer for the present to remain centered.

- A Segment's momentary **role** depends on the relative `path` from the Focus in the Tree

1. `*P*` **Parent**: collapsed
2. `*F*` **Focus**: expanded if accordion is expanded
3. `A,B` **Aisle**: expanded if accordion is expanded
4. `P,F` **Breadcrumb**: collapsed
5. `L,R` **BreadcrumbAisle**: collapsed
6. `:_:` **Periphery**: imploded (takes no space at all)
7. `. .` **None**


## Interactivity of a Segment

- [x] You can click on a segment's _caption_ it to navigate to it. This will re-route you to the Segment id page or fragment. If it was already focused, the accordion will **collapse**, else it will navigate to the new focus and **expand**.

# Accordion

The Accordion is either _expanded_ or _collapsed_ (use `flip` to switch between these modes), and it has a `ZipperTree` of Segments. When serializing, the Segments are replaced by their `id`s, and when deserializing, the Accordion will try to fetch the appropriate data, either from within its namespace (compile-time data) or from the cloud database.

## Url

The current `Parent` _(not the `Focus`)_ is serialized as a _path_ in the Url. An empty path means that the focus is the root.

## Layout of the Accordion

On the screen, you can always see three vertically laid-out partitions: the `past`, the `present`, and the `future`. Past and future are centered and purely vertical. The present is horizontally laid out and centered on the `focus`, which means we need to render `placeholders` to the left and right of the visible parts:

```elm
                         P1
                         P0
                         *P
_b0_b1_r0_r1:L1 L0 X1 X0 *F Y0 Y1 R0 R1:l1_l0_a1_a0_ 
                         F0
                         F1
```

- _`L` are horizontal Segments from left `Aisle` or `Breadcrumb`_
- _`R` are horizontal right `BreadcrumbAisle` Segments_
- _`*F`, `A` and `B` are the `Focus` with horizontal `Aisle`s_
- _`P` are vertical Segments from left `BreadcrumbAisle` including the `Breadcrumb`s_, plus vertical left `Aisle`s
- _`F` are vertical right `BreadcrumbAisle` Segments, plus vertical right `Aisle`s_

To achieve this effect, we use the following algorithm:

- [ ] Pair all Segments with the `Imploded` Viewmode
- [ ] Then mark `Parent`, `Focus`, `Aisle`, `Breadcrumb`, `BreadcrumbAisle`
- [ ] Fold the tree using the rules stated below

### Folding a Branch

Note that the following algorithm discards the sub-branches of sub-branches, i.e. it generates only the first-generation branches plus its sub-branch spines. Like a lametta tree.

```elm
init = \a -> 
    { here=a, left=[], right=[], down=[] }
grow =
    { down = \a b -> 
        case a.orientation of
            Horizontal -> { b | right=b.right++[a] }
            Vertical -> { b | down=b.down++[a] }
    , left = \{here} b -> 
        case here.orientation of
            Horizontal -> { b | left = here::b.left }
            Vertical -> { b | down = here::b.down }
    , right = \{here} b ->
            ...

    }
```

### Folding a Tree

```elm
init = \{here, left, right, down} ->
    { left = [], x = left, here = here, y = right, right = []
    , down = down }
grow =
    { up = \a c ->
        case a.orientation of
            Horizontal -> { c | left = a::c.left }
            Vertical -> { c | up = c.up++[a] }
    , left = \{here, left, right, down} c ->
        case here.orientation of
            Horizontal -> { c | left = a::c.left }
            Vertical -> { c | up = c.up++left }
    }
```

# Animations

- [ ] You can scroll to the ends in all directions
- [ ] Whenever the focus changes, the site will scroll smoothly so that it is centered
- [ ] Collapsing and Expanding are smoothly animated



# Future features

- [ ] `BreadcrumAisle`s are _collapsed_ when accordion is collapsed, and _imploded_ when accordion is expanded
  - [ ] To help with discoverability, we implement a (desktop-only) hover effect that shows a preview of its descendant and the first aisle-image while hovering over a collapsed segment. This image will be behind the text, under the mouse pointer.
- [ ] Auto-squeeze horizontal Aisle such that the following segments are within the screen:
  - F0 at the bottom
  - L0 at the left
  - R0 at the right
  - P0 at the top  
- [ ] Store the DescendantFocus as the path