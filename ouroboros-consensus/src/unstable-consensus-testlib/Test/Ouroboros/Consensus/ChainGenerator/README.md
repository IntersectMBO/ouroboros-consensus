The ChainGenerators are documented in the Genesis test plan.
This file is a terse summary.

These generators yield a schema for "the honest chain" and a schema for one alternative chain that intersects with the honest one.

A _schema of a chain_ merely indicates in which slots a block exists on that chain.
If there are only empty slots between two active slots in a schema, then it is required that the block in the younger slot extends the block in the older slot.

This generated pair of schemas will satisfy various concrete properties that the Praos and Genesis papers' theorems imply.
Those properties are always preserved by adding active slots to the honest schema and/or by removing active slots from the alternative schema.
However, it is generally assumed that tests will use the generated schemas without modifiying them---though counterexample shrinkers might rely on the above, for example.

The generators themselves have some tests in the ouroboros-consensus:test:infra-test suite, but they merely test that the generators are behaving as expected.
(If the "mutation" subset of tests in that suite seem flaky, try increasing the number of seeds attempted per QuickCheck counterexample candidate.)

-----

`uniformTheHonestChain` ensures the following postconditions.

- The schema contains exactly `Len` slots.
  Each slot after the last in the schema is considered a _future slot_.

- There is at least one honest active slot in the schema.

- For each of the `Len` slots, the window of `Scg` slots that begins with that slot contains at least `Kcp` active slots, assuming each future slot is honest active.

`uniformAdversarialChain` ensures the following postconditions.
Let sai abbreviate "slot after the intersection".
Let asai abbreviate "active sai".

- These postconditions are all subject to the assumption that future slots are honest active and alternative inactive.

- The alternative schema begins with the slot after its intersection with the honest schema and ends in the same slot as the honest schema.

- The intersection is chosen such that there is at least one honest asai and at least one alternative asai.

- Ouroboros Praos requires that the `Kcp+1`st alternative asai either doesn't exist or else is at least `Del+1` slots after the `Kcp+1`st honest asai.

    - Moreover, for all `i>0`, the `i+Kcp+1`st alternative asai either is more than `Scg` slots after the first alternative asai, doesn't exist, or else is at least `Del+1` slots after the `i+Kcp+1`st honest asai.
      Informally: the adversary is always free to have chosen a later intersection, and this property ensures doing so would not have benefited them.
      However, the property also assumes (extremely pessimistically) that the adversary can arbitrarily accelerate their election rate one stability window after their first block.

- Ouroboros Genesis requires that the first `Scg` sais contain strictly more honest active than alternative active.

    - Moreover, that inequality must hold for every prefix of that window that includes the `Kcp+1`st honest asai.
      This rules out a corner case in which the Genesis rule would disagree with the Praos rule, which the Genesis authors argued has a similar probability as a Praos Common Prefix violation.
