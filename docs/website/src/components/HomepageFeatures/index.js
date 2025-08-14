import React from 'react';
import clsx from 'clsx';
import styles from './styles.module.css';
import ouroboros from "/static/img/ouroboros-image.webp";
import Admonition from '@theme/Admonition';

export default function HomepageFeatures() {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          <div className="col col--12">
            <div className={styles.intro}>
              <h2>Welcome to the <code>ouroboros-consensus</code>  documentation</h2>

              <p>
                This site provides a comprehensive and detailed technical documentation for the
                <a href="https://github.com/IntersectMBO/ouroboros-consensus"> Haskell implementation</a> of the Consensus,
                Mempool, and Storage Layers of the <a href="https://github.com/input-output-hk/cardano-node"><code>cardano-node</code></a>.
              </p>

	      <Admonition type="warning" title="Work in Progress">
                <p>
                  This documentation is currently under development. Some sections may be incomplete or subject to change. We appreciate your patience and feedback.
                </p>
              </Admonition>

              <p>
                We follow the <a href="https://diataxis.fr/">Diátaxis framework</a>, which helps you find the information
                you need based on your goal:
              </p>

              <ul>
                <li>
		  <a href="/docs/explanations/">Explanations: </a>
		  Understand core concepts, including implementation components, consensus protocols, and network-layer interactions.
		</li>
                <li>
		  <a href="/docs/tutorials/">Tutorials: </a>
		  Follow step-by-step guides to get started and achieve a specific goal.
		</li>
                <li>
		  <a href="/docs/howtos/">HOWTOs: </a>
		  Get practical instructions for tasks like adding an era or working with stored chain data.
		</li>
                <li>
		  <a href="/docs/references/">References: </a>
		  Read technical descriptions of APIs, data structures, serialization, and reports.
		</li>
              </ul>

              <p>
                This documentation focuses on the specific Haskell implementation of the Consensus, Mempool, and Storage components used by <a href="https://github.com/input-output-hk/cardano-node"><code>cardano-node</code></a>.

                For an implementation-agnostic description of the Ouroboros protocols, network interactions, serialization formats, and others, refer to the <a href="https://cardano-scaling.github.io/cardano-blueprint/">Cardano Blueprints</a>.
              </p>
            </div>
          </div>
        </div>
      </div>
    </section>
  );
}
