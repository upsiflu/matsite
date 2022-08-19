This is the new Moving Across Thresholds website.

For an up-to-date documentation, install `yarn` and `elm-doc-preview`, then run `yarn doc` and navigate to `<dev IP>:8000`

The [DESIGN](./DESIGN.md) and [SPECIFICATION document](SPECIFICATION.md)s are out of date.


## Progress

Since May, all issues are tracked on [github issues](https://github.com/upsiflu/matsite/issues).


## Inspiration and Thought process

For more concrete discussion of content and structure, check out these **collaborative docs:**

- [Example Sites; Some example Content](https://docs.google.com/document/d/1WBk1p87gxW8zPPTjid2BupmaUjcJCX3DvfpehNbtFUw/edit?usp=sharing)
- [Layout](https://docs.google.com/document/d/1zC7TirujtAtsySjGhr_0QOIqSRf53j6Xw_1FAXzUpNA/edit?usp=sharing)
- [Structure](https://docs.google.com/document/d/1gWE5tKyMtmpZlIjN4wl592KyJVVSWHRb8MFjTggPXpM/edit?usp=sharing)

## Sketches and Drafts

![](asset/22-03-07-Components.svg)
Sketch of Measures (vertical and horizontal rhythm) and potential Colors, by Flupsi, 22-03-07

![](asset/22-03-04-Sketch.svg)
Sketch of a Look, as the screen expands, by Flupsi, 22-03-04


# Run it

Install `yarn` to execute the scripts defined in [package.json](package.json)

## Code Documentation

`yarn doc`

Install elm-doc-preview and run `edp` to launch a local documentation server.

```sh
edp --no-browser --port 1234
```

## Verify examples

Run `elm-verify-examples` and `elm-test` to verify the correctness of the examples given in the documentation.

```sh
elm-verify-examples && elm-test
```

```sh
elm-verify-examples --run-tests
```

## Run on the local network

`yarn dev`

will run the development version on port 1234 of your development computer

----

```sh
npm install -g elm-live
```

```sh
elm-live src/Main.elm -v --host=192.168.178.93 --pushstate --port=8000 -- --output=main.js --debug
```


# Deployment

Install `firebase` and deploy via

```sh
yarn clear-build-cache && yarn build && firebase deploy
```

