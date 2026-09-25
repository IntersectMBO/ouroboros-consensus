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
