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

-----

`uniformAdversarialChain` requires that the honest schema permits an intersection with both `Kcp+1` honest asais and also enough sais to also allow at least `Kcp+1` alternative asais.
Specifically, there must be enough sais so that at least `Kcp` slots are _unstable_, ie both more than `Del` after the `Kcp+1`st honest asai and also more than `Scg` after the first alternative asai.

`uniformAdversarialChain` requires that the honest schema permits an intersection with both `Kcp+1` honest asais and also enough sais to also allow at least `Kcp+1` alternative asais.
Specifically, there must be enough sais so that at least `Kcp` slots are _unstable_, ie both more than `Del` after the `Kcp+1`st honest asai and also more than `Scg` after the first alternative asai.

Suppose `1<Kcp`.
The `Kcp+1`st honest asai can be no later than the `2Scg-(Kcp-1)`th sai.
The unstable slots could begin as late as `Del+1` after that, ie the `2Scg-Kcp+Del+2`th sai.
The first alternative asai is unconstrained when `1<Kcp`, and thus merely needs to be no later than the `Scg-Kcp+Del+1`st sai to not push the unstable slots farther into the future than the sai above.
Thus `2Scg+Del+2` sais would suffice to ensure there are at least `Kcp` unstable slots before the end of the schema.

Suppose `Kcp=0`.
Note that there does not need to be an unstable slots, merely at least one alternative asai.
The `Kcp+1`st honest asai is the first honest sai, and so can be no later than the `Scg`th sai, since Genesis requires the first `Scg` sais contains _strictly_ more honest active than alternative active.
(This requires that the honest schema has two active slots in the same window of `Scg+1` slots, one of which may be the genesis block.)
Thus it's possible that the first alternative asai can be no sooner than `Scg+Del+1`.
Thus `Scg+Del+1` sais would suffice to ensure there are at least `Kcp=0` unstable slots before the end of the schema (actually: that there is at least one alternative asai).

Suppose `Kcp=1`.
(Again, the honest schema must have two active slots in the same window of `Scg+1` slots, one of which may be the genesis block.)
This is similar to the `1<Kcp` case, except there might only be one honest active in the first `Scg` sais, and so the first alternative active sai might need to be after the `Scg`th sai, meaning the unstable slots couldn't begin before the `2Scg+2`nd sai.
Also, the `Kcp+1=2`nd honest asai might be as late as the `2Scg`th sai, so the unstable slots might not begin until the `2Scg+Del+1`st sai.
Thus `2Scg + max (Del+2) 3` sais would suffice to ensure there are at least `Kcp=1` unstable slot before the end of the schema.

Since the genesis block can always be chosen as the intersection (as long as the first honest active slot is at most the `Scg`th slot), the above sai counts are also the minimum bound for `Len` in order to ensure at least `Kcp+1` asais for both honest and alternative.
