_Moving Across Thresholds Website, March 2022_
_Updated: May 2022_

For more details, consult the [specification document](SPECIFICATION.md).

# Design

- [Design](#design)
  - [Use case](#use-case)
    - [A Section](#a-section)
    - [Navigate](#navigate)
    - [Edit](#edit)
    - [Presets and custom Content](#presets-and-custom-content)

## Use case
Visitors open [`movingAcrossThresholds.com`](movingAcrossThresholds.com) and are greeted with a video. Below the video, they find links to open other **section**s.

### A Section
consists of a `Header` of arbitrary hypertext[^1] (which doubles as an expand/collapse handle) and a `Body`. The latter can be either:

a. An arbitrary `embed` such as [are.na](are.na), [vimeo](vimeo.com) or a predefined form (Subscribe to Newsletter)
b. Formatted text[^1]

[^1]: For the prototype, it's plain HTML. For a future iteration, it can be some sort of markdown or rich text format that is more predictable.

### Navigate
the sections by clicking the `Header`s. Clicking a _focused_ `Header` will collapse it and return to the previous state. Clicking a _blurred_ `Header` expands its `Body` and collapses the current `Body`. 

- When expanding a `Section`, its neighbors will be presented but remain collapsed.[^2]
- The URL reflects the current `Header` so that you can share links into any section[^3].
- When you change the URL, the corresponding `Section` is found and focused.[^4]

The upmost layer (starting with the intro video) is laid out vertically, the next layer below horizontally, the third layer vertical again, ad inf.[^5]


[^2]: On wider screens, neighbors can be successively expanded until the screen is filled. This is not trivial but should probably be implemented in an early iteration on the prototype. Update: All aisle segments are expanded and can be scrolled.
[^3]: In the prototype, the match must be exact. Later, we can integrate an algorithm I'm using on ShellCongress.com which finds the closest match, so it becomes a bit resilient to typos.
[^4]: This is for example broken on the mobile jsc.art site.
[^5]: A nod to Julia Stoschek, again. If you want to switch the orientation of a certain layer, nest it in an in-between layer.

### Edit
Renae or a collaborator can log in through a button hidden in the 'About' section. Once logged in, you can still navigate the site in the normal way but the _focused_ `Section` changes in the following ways:

- The `Header` becomes _editable_. An internal mechanism ensures that `Headers` remain unique and will reject homonyms.[^7]
- The `Body` becomes _editable_.[^7]
- **Move** it in any of the following direction (for the purpose of reordering content):
    - after (if it's the last section, add one after)
    - before (if it's the first section, add one before)
    - up one level (if it's the top level, ignore)
    - down one level (if it's the bottom level, add one below)
- **Add** an empty Article to North, West, East or South. TODO
- **Delete** it: If there is a `Section` before it in the current level, that will be focused instead. Else if there is one after it, that will come into focus. Else, we go one level up. The last remaining `Section` can't be deleted at all; the command will then be ignored.

[^7]: For the prototype, these are arbitrary Html hypertexts. Later iterations should progressively add invariance in order to add order.


### Presets and custom Content
`Accordion` holds a volatile list of presets that can be switched on per-segment during editing. There are Body Presets (for the F) as well as Byline Presets (for the P).

