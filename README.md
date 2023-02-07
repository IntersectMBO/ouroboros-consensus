# Ouroboros consensus

Implementation of the [Ouroboros-family](docs/References.md) of consensus
algorithms.

:construction: This repository currently contains a proposal for re-organizing
and publishing the Consensus documentation. We might migrate the code from
[ouroboros-network/ouroboros-consensus*](https://github.com/input-output-hk/ouroboros-network)
soon.

## Documentation

Please see [this page](https://input-output-hk.github.io/ouroboros-consensus/).

## How to contribute to the project

Your help is greatly appreciated. Please see [our CONTRIBUTING
document](CONTRIBUTING.md).

## How to submit an issue

Issues can be filled in our [GitHub issue
tracker](https://github.com/input-output-hk/ouroboros-consensus/issues).

## Installation


This website is built using [Docusaurus 2](https://docusaurus.io/), a modern static website generator.

### Local Development

```
$ yarn start
```

This command starts a local development server and opens up a browser window. Most changes are reflected live without having to restart the server.

### Build

```
$ yarn build
```

This command generates static content into the `build` directory and can be served using any static contents hosting service.

### Deployment

Using SSH:

```
$ USE_SSH=true yarn deploy
```

Not using SSH:

```
$ GIT_USER=<Your GitHub username> yarn deploy
```

If you are using GitHub pages for hosting, this command is a convenient way to build the website and push to the `gh-pages` branch.
