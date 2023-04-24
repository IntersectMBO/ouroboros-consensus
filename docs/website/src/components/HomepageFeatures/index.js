import React from 'react';
import clsx from 'clsx';
import styles from './styles.module.css';
import ouroboros from "/static/img/ouroboros-image.webp";


export default function HomepageFeatures() {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          <div className={clsx('col col--4 text--center')}>
            <img className={styles.featureSvg} src={ouroboros} />
          </div>
          <div className={clsx('col col--8 text--center')}>
            <h3>State-of-the-Art Consensus Protocol</h3>
            <p>Ouroboros is a family of <b>Proof-of-Stake</b> protocols powering the <a href="https://cardano.org">Cardano</a> blockchain, among others.
              It is backed by years of research and numerous <a href="https://iohk.io/en/research/library/">peer-reviewed publications</a>. This site aims at providing comprehensive and detailed technical
              documentation about the implementation of Ouroboros at the heart of the cardano-node.</p>
          </div>
        </div>
      </div>
    </section>
  );
}
