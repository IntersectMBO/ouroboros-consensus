/**
 * Creating a sidebar enables you to:
 - create an ordered group of docs
 - render a sidebar for each doc of that group
 - provide next/previous navigation

 The sidebars can be generated from the filesystem, or explicitly defined here.

 Create as many sidebars as you want.
 */

// @ts-check

/** @type {import('@docusaurus/plugin-content-docs').SidebarsConfig} */
const sidebars = {
  // By default, Docusaurus generates a sidebar from the docs folder structure.
  // We choose not to autogenerate the sidebar entries because that would
  // require prefixing all entries with numbers, which will make inserting a new
  // entry onerous since we would have to modify the entry numbers of each
  // subsequent entry.
  about_ouroboros: [
    { type: 'category',
      label: 'About Ouroboros',
      items: [
        'about-ouroboros/index',
        'about-ouroboros/References'
      ]
    }
  ],

  for_developers: [
    { type: 'category',
      label: 'For Developers',
      items: [
        'for-developers/index',
        'for-developers/Glossary',
        'for-developers/ComponentDiagram',
        'for-developers/AbstractProtocol',
        'for-developers/AddingAnEra',
        'for-developers/ChainSync',
        'for-developers/HardWonWisdom',
        'for-developers/StyleGuide',
        'for-developers/ProfilingTipsAndTricks',
        'for-developers/Benchmarks',
        'for-developers/GitProcess',
        'for-developers/ReleaseProcess',
        'for-developers/SanityChecks',
        'for-developers/QueryVersioning',
        'for-developers/BootstrapPeersIER',
        'for-developers/TechnicalReports',
        'for-developers/PreflightGuide',
        'for-developers/NodeTasks',
        'for-developers/HandlingBlocksFromTheFuture'
      ]
    },
  ]

};

module.exports = sidebars;
