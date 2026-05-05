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
      label: 'Explanations',
      items: ['explanations/index',
              'explanations/design_goals',
              'explanations/data_flow',
              'explanations/consensus_protocol',
              'explanations/ledger_interaction',
              'explanations/ticking',
              'explanations/queries',
              'explanations/node_tasks',
             ]
    }
  ],
  tutorials: [
    { type: 'category',
      label: 'Tutorials',
      items: ['tutorials/index',
              'tutorials/preflight_guide',
              'tutorials/instantiating_consensus',
             ]
    }
  ],
  howtos: [
    { type: 'category',
      label: 'HOWTOs',
      items: ['howtos/index',
              { type: 'category',
                label: 'Contributing',
                items: ['howtos/contributing/sanity_checks',
                        'howtos/contributing/consensus_git_process',
                        'howtos/contributing/how_to_make_a_release',
                        'howtos/contributing/style_guide',
                       ]
              },
              'howtos/adding_an_era',
              'howtos/versioning_a_new_query',
              'howtos/inspecting_the_selection_of_a_node',
             ]
    }
  ],
  reference: [
    { type: 'category',
      label: 'Reference',
      items: ['references/index',
              'references/glossary',
              'references/key_type_families_and_classes',
              'references/haddocks',
              'references/data_flow',
              'references/block_diagrams_of_data',
              'references/additional_material',
              'references/technical_reports',
              { type: 'category',
                label: 'Miscellaneous',
		items: ['references/miscellaneous/about_ouroboros',
                        'references/miscellaneous/hard_won_wisdom',
                        'references/miscellaneous/versioning_scheme_decision',
                        'references/miscellaneous/cardano_praos_basics',
                        'references/miscellaneous/chain_sync',
                        'references/miscellaneous/bootstrap_peers_IER',
                        'references/miscellaneous/civic_time',
                        'references/miscellaneous/ticking',
                        'references/miscellaneous/handling_blocks_from_the_future',
                        'references/miscellaneous/era_transition_governance',
                        'references/miscellaneous/genesis_design',
                        { type: 'category',
                          label: 'UTxO-HD',
                          items: ['references/miscellaneous/utxo-hd/index',
                                  'references/miscellaneous/utxo-hd/migrating',
                                  'references/miscellaneous/utxo-hd/utxo-hd_in_depth',
                                  'references/miscellaneous/utxo-hd/future_ledger-hd',
                                 ]

                        }
                       ]
	      }
             ]
    }
  ],
};

module.exports = sidebars;
