# Glossary

## Active Slot Coefficient

The active slot coefficient represents the chance a given slot elects one of the registered stake pools. It is usually denoted as $f$.

On the Cardano mainnet we have:

$$f = 1/20$$

So this means a 5% chance of electing one of the registered stakepools.

In the implementation we assume $f$ won't change. Dealing with changing $f$ is a hard problem which requires proper research.

## Chain growth property

This property states that there are at least $k$ blocks in $3k/f$ slots.

Here $f$ refers to the [active slot coefficient](#active-slot-coefficient).
