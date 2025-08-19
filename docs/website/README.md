The website for `ouroboros-consensus` is generated using [Docusaurus](https://docusaurus.io/). To build and view the generated website, see [_Building the website_](#building-the-website).

# Website Content

The front page is defined in the [`index.js`](src/components/HomepageFeatures/index.js) file. The main sections are configured in [`docusaurus.config.js`](./docusaurus.config.js).

The website's source pages are located in the [`contents`](./contents) directory. In accordance with the [Diátaxis framework](https://diataxis.fr/) for organizing technical documentation, we categorize our pages into four sections:

- **Explanations** ([`contents/explanations`](./contents/explanations))
- **Tutorials** ([`contents/tutorials`](./contents/tutorials))
- **HOWTOs** ([`contents/howtos`](./contents/howtos))
- **Reference** ([`contents/reference`](./contents/references))

Refer to the [Diátaxis framework](https://diataxis.fr/) for guidance on the type of documentation that belongs in each section.

All pages are written in Markdown format.

## Adding a New Page

To add a new page, create a Markdown file inside the appropriate directory (e.g., [`contents/explanations`](./contents/explanations)), based on the Diátaxis category the page belongs to.

Next, update [`sidebars.js`](./sidebars.js) and add a reference to the new page in the sidebar configuration. For example, if you add `reference/foo.md`, Docusaurus will generate a reference called `reference/foo`, which can be added to the sidebar as follows:

```javascript
/** @type {import('@docusaurus/plugin-content-docs').SidebarsConfig} */
const sidebars = {
  ...
  reference: [
    { type: 'category',
      label: 'Reference',
      items: [
        'reference/index',
        'reference/foo', // <--- Your new page
        ...
      ]
    }
  ]
  ...
};
```

The order of entries in the `items` list determines their placement in the sidebar. Refer to [`sidebars.js`](./sidebars.js) for the rationale behind manual sidebar configuration.

# Building the website

To install the required packages for the documentation site, run:

```
$ yarn
```

After installation, build the documentation site by running:

```
$ yarn start
```

This starts a local development server and opens a browser window. Most changes are reflected live without needing to restart the server.

## Build

To generate static content:

```
$ yarn build
```

This command outputs the static content into the build directory, which can be hosted using any static content hosting service.

## Deployment

The website is deployed using a GitHub Action. See [`.github/workflows/ci.yml`](../../.github/workflows/ci.yml).
