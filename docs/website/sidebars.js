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
  explanation: [
    { type: 'category',
      label: 'Explanation',
      items: ['explanation/index',
	      'explanation/design-goals',
	      'explanation/ledger-interaction',
	      'explanation/queries'
	     ]
    }
  ],
  tutorials: [
    { type: 'category',
      label: 'Tutorials',
      items: ['tutorials/index']
    }
  ],
  howtos: [
    { type: 'category',
      label: 'HOWTOs',
      items: ['howtos/index']
    }
  ],
  reference: [
    { type: 'category',
      label: 'Reference',
      items: ['reference/index']
    }
  ],
};

module.exports = sidebars;
