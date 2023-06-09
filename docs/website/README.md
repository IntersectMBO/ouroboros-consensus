The website for `ouroboros-consensus` is generated using [docusaurus](https://docusaurus.io/). If you want to build and view the generated website see Section [_Building the website_](#building-the-website).

# Website content

The website source pages can be found in the [`contents`](./contents) directory. We divide our pages into two main categories:

- "About Ouroboros", which contains high-level information about the Consensus protocols that the code in this repository implements. The pages for this category should be placed in the [`contents/about-ouroboros`](./contents/about-ouroboros) directory.
- "For Developers", which contains information relevant for contributors to the Consensus code base. The pages for this category should be placed in the [`contents/for-developers`](./contents/for-developers) directory.

The pages are written in Markdown format.

## Adding a new page

To add a new page create a Markdown file inside the appropriate directory (either [`contents/about-ouroboros`](./contents/about-ouroboros) or [`contents/for-developers`](./contents/for-developers) depending on the category). Then edit the [`sidebars.js`](./sidebars.js) file and add a reference to the new page in the desired place inside the sidebar item's configuration. For instance, if you add a new page `for-developers/foo.md`, Docusaurus will generate a reference called `for-developers/foo`, which can be used to add an entry to the sidebar as follows:

```javascript
/** @type {import('@docusaurus/plugin-content-docs').SidebarsConfig} */
const sidebars = {

  ...

  for_developers: [
    { type: 'category',
      label: 'For Developers',
      items: [
        'for-developers/index',
        'for-developers/ComponentDiagram',
        'for-developers/foo', // <--- Your new page
        'for-developers/ChainSync',
      ]

  ...
```

The order of the page in the `items` list determines the order in which this entry will appear in the sidebar. See [`sidebars.js`](./sidebars.js) for the rationale behind not autogenerating this.

# Building the website

To install the packages required to build the documentation site run:

```
$ yarn
```

After the necessary packages are installed by the above command, the
documentation site can be built by running:

```
$ yarn start
```

This command starts a local development server and opens up a browser window. Most changes are reflected live without having to restart the server.

## Build

```
$ yarn build
```

This command generates static content into the `build` directory and can be served using any static contents hosting service.

## Deployment

We deploy the website using a GitHub action. See [`.github/workflows/ci.yml`](../../.github/workflows/ci.yml).
