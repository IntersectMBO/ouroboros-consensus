// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

import {themes as prismThemes} from 'prism-react-renderer';

// Required to render mathematical equations using KaTeX (https://docusaurus.io/docs/markdown-features/math-equations).
const math = require('remark-math');
const katex = require('rehype-katex');

// generic edition URL that will be used by all parts of the documentation
const editUrl = 'https://github.com/IntersectMBO/ouroboros-consensus/tree/main/docs/';

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'ouroboros-consensus documentation',
  tagline: '',
  favicon: 'img/cardano_icon.ico',

  // Set the production url of your site here
  url: 'https://ouroboros-consensus.cardano.intersectmbo.org/',
  // Set the /<baseUrl>/ pathname under which your site is served
  // For GitHub pages deployment, it is often '/<projectName>/'
  baseUrl: '/',

  // GitHub pages deployment config.
  organizationName: 'IntersectMBO',
  projectName: 'ouroboros-consensus',

  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',

  // Even if you don't use internalization, you can use this field to set useful
  // metadata like html lang. For example, if your site is Chinese, you may want
  // to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  markdown: {
    mermaid: true,
      // We don't want to use MDX, as this requires escaping `<` and `{` symbols, which is quite inconvenient.
      // See: https://docusaurus.io/docs/markdown-features#mdx-vs-commonmark
    format: 'detect',
  },

  themes: ['@docusaurus/theme-mermaid'],

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          // The website is already inside a `docs` directory. So we do not use the default path.
          path: 'contents',
          sidebarPath: require.resolve('./sidebars.js'),
          // Please change this to your repo.
          // Remove this to remove the "edit this page" links.
          editUrl,
          // Add KaTeX support.
          remarkPlugins: [math],
          rehypePlugins: [katex],
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      }),
    ],
  ],

  // Add KaTeX support.
  stylesheets: [
    {
      href: 'https://cdn.jsdelivr.net/npm/katex@0.13.24/dist/katex.min.css',
      type: 'text/css',
      integrity:
        'sha384-odtC+0UGzzFL/6PNoE8rX/SPcQDXBJ+uRepguP4QkPCm2LBxH3FA3y+fKSiJ+AmM',
      crossorigin: 'anonymous',
    },
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      // Replace with your project's social card
      image: 'img/docusaurus-social-card.jpg',
      navbar: {
        title: 'Ouroboros Consensus',
        logo: {
          alt: 'Cardano Logo',
          src: 'img/logo.svg',
        },
        // Navigation bar items. Note that the generated page is prefixed with
        // `docs` regardless of what `presets.docs.path` is set to.
        items: [
          {
            to: '/docs/explanations/',
            position: 'left',
            label: 'Explanations',
          },
          {
            to: '/docs/tutorials/',
            position: 'left',
            label: 'Tutorials',
          },
          {
            to: '/docs/howtos/',
            position: 'left',
            label: 'HOWTOs',
          },
          {
            to: '/docs/references',
            position: 'left',
            label: 'References',
          },
          {
            href: 'https://github.com/IntersectMBO/ouroboros-consensus',
            label: 'GitHub',
            position: 'right',
          },
        ],
      },
      footer: {
        style: 'dark',
        links: [
          {
            title: 'Docs',
            items: [
              {
                label: 'Explanations',
                to: '/docs/explanations/',
              },
              {
                label: 'Tutorials',
                to: '/docs/tutorials/',
              },
	      {
                label: 'HOWTOs',
                to: '/docs/howtos/',
	      },
	      {
                label: 'References',
                to: '/docs/references/',
              }
            ],
          },
          {
            title: 'Community',
            items: [
              {
                label: 'Cardano Updates',
                href: 'https://cardano-updates.cardano.intersectmbo.org',
              },
              {
                label: 'Stack Overflow',
                href: 'https://cardano.stackexchange.com/questions/tagged/consensus',
              },
              {
                label: 'Discord',
                href: 'https://discord.gg/inputoutput',
              },
            ],
          },
          {
            title: 'More',
            items: [
              {
                label: 'GitHub',
                href: 'https://github.com/IntersectMBO/ouroboros-consensus',
              },
              {
                label: 'Haddocks',
                href: 'pathname:///haddocks/index.html',
              },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} Input Output Global, Built with Docusaurus.`,
      },
      prism: {
        theme: prismThemes.github,
        darkTheme: prismThemes.dracula,
        additionalLanguages: ['haskell'],
      },
    }),

  plugins: [
    [
      '@docusaurus/plugin-client-redirects',
      {
        redirects: [
          {
            from: '/docs/for-developers/utxo-hd/overview',
            to: '/docs/references/miscellaneous/utxo-hd/',
          },
          {
            from: '/docs/for-developers/utxo-hd/Overview/',
            to: '/docs/references/miscellaneous/utxo-hd/',
          },
          {
            from: '/docs/for-developers/PreflightGuide/',
            to: '/docs/tutorials/preflight_guide/',
          },
          {
            from: '/docs/for-developers/Glossary/',
            to: '/docs/references/glossary/',
          },
          {
            from: '/docs/for-developers/BootstrapPeersIER/',
            to: '/docs/references/miscellaneous/bootstrap_peers_IER/',
          },
        ],
        createRedirects(existingPath) {
          if (existingPath.includes('/docs/references/miscellaneous/utxo-hd')) {
            return [
              existingPath.replace('/docs/references/miscellaneous/utxo-hd', '/docs/for-developers/utxo-hd'),
            ];
          }
          return undefined;
        },
      },
    ],
  ],
};

module.exports = config;
