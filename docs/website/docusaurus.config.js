// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');

// generic edition URL that will be used by all parts of the documentation
const editUrl = 'https://github.com/input-output-hk/ouroboros-consensus/tree/main/docs/';

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'Ouroboros Consensus',
  tagline: 'The family of protocols powering Cardano',
  favicon: 'img/cardano_icon.ico',

  // Set the production url of your site here
  url: 'https://input-output-hk.github.io/',
  // Set the /<baseUrl>/ pathname under which your site is served
  // For GitHub pages deployment, it is often '/<projectName>/'
  baseUrl: '/ouroboros-consensus',

  // GitHub pages deployment config.
  organizationName: 'input-output-hk',
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
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      // Replace with your project's social card
      image: 'img/docusaurus-social-card.jpg',
      prism: {
        additionalLanguages: ['haskell'],
      },
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
            to: '/docs/about-ouroboros/',
            position: 'left',
            label: 'About Ouroboros',
          },
          {
            to: '/docs/for-developers',
            position: 'left',
            label: 'For Developers',
          },
          {
            href: 'https://github.com/input-output-hk/ouroboros-consensus',
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
                label: 'About Ouroboros',
                to: '/docs/about-ouroboros/',
              },
              {
                label: 'For Developers',
                to: '/docs/for-developers/',
              }
            ],
          },
          {
            title: 'Community',
            items: [
              {
                label: 'Cardano Updates',
                href: 'https://input-output-hk.github.io/cardano-updates',
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
                href: 'https://github.com/input-output-hk/ouroboros-consensus',
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
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
      },
    }),
};

module.exports = config;
