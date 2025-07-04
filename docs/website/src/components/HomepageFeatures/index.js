import React from 'react';
import clsx from 'clsx';
import styles from './styles.module.css';
import ouroboros from "/static/img/ouroboros-image.webp";

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

              <p>
                We follow the <a href="https://diataxis.fr/">Diátaxis framework</a>, which helps you find the information
                you need based on your goal:
              </p>

              <ul>
                <li><strong>Explanations:</strong> Understand core concepts, including implementation components, consensus protocols, and network-layer interactions.</li>
                <li><strong>Tutorials:</strong> Step-by-step guides to get started and achieve a specific goal.</li>
                <li><strong>HOWTOs:</strong> Practical instructions for tasks like adding an era or working with stored chain data.</li>
                <li><strong>References:</strong> Technical descriptions of APIs, data structures, serialization, and reports.</li>
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
