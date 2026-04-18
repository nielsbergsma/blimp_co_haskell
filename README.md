# Blimp&Co - Haskell

This codebase is a re-implementation of the Elm "backoffice" web application, found [here](https://github.com/nielsbergsma/blimp_co_elm/tree/main/frontend/backoffice), in Haskell. It was developed for a Medium article that explores how to apply Domain-Driven Design in an edge-computing environment. More details are covered in the article.

**Note:** This codebase originated as a fork of [miso-sampler](https://github.com/haskell-miso/miso-sampler).

**Note:** The [Medium article is here](https://medium.com/@n.bergsma/from-chaos-to-clarity-applying-domain-driven-design-with-functional-languages-in-a-serverless-wild-674d6bca8149)

## Implementation

This project uses the [GHC WebAssembly backend](https://www.tweag.io/blog/2022-11-22-wasm-backend-merged-in-ghc/) with [MISO](https://haskell-miso.org/) to produce browser-compatible WebAssembly. [Tailwind CSS](https://tailwindcss.com/) is used for styling.

The structure closely follows its Elm counterpart and results in a Single-Page Application (SPA). The source directory is organized as follows:

- **Pages**: Top-level pages.
- **Components**: Reusable components, both stateful and stateless.
- **Data**: Records, decoders, and functions for communication with external sources.

### JavaScript Interoperability

The code uses `foreign import javascript` and `foreign export javascript` statements for interoperation with the JavaScript environment. For instance:

- [src/Routes.hs](src/Routes.hs) contains the application’s routing logic.

This application also demonstrates the use of HTML Web Components. See [js/components.js](js/components.js) for details.

## Live Demonstration

A hosted version of the application is available at [https://backoffice-hs.software-craftsmen.dev](https://backoffice-hs.software-craftsmen.dev).

The resulting WebAssembly file is 2.3 MB (uncompressed) and 941 KB (compressed with zstd). It works in all recent major browsers.

## Installation

Head over to [miso-sampler](https://github.com/haskell-miso/miso-sampler) for setting up your local environment.

To install dependencies, first install the NIX package manager on your system. Then, run the following commands:

```shell
nix develop .#wasm --command bash -c "make"  
```

## Running locally

```shell
# after building
cd dist 
python3 -m http.server 8000
```