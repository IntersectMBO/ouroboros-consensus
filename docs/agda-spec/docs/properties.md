# Properties

<a id="sec:properties"></a>

This section describes the properties that the consensus layer should have.
The goal is to include these properties in the executable specification
to enable e.g. property-based testing or formal verification.

## Header-only validation

<a id="sec:header-only-validation"></a>

In any given chain state, the consensus layer needs to be able to validate the
block headers without having to download the block bodies.
[Property 1](#prop:header-only-validation) states that if an extension of a
chain that spans at most $\mathsf{StabilityWindow}$ slots is valid, then validating the
headers of that extension is also valid. This property is useful for its
converse: if the header validation check for a sequence of headers does not
pass, then we know that the block validation that corresponds to those headers
will not pass either. In these properties, we refer to the $\mathsf{CHAIN}$
transition system as defined in the [Cardano ledger specification](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/shelley-ledger.pdf).

<a id="prop:header-only-validation"></a>

**Property 1 (Header only validation).**

For all states $s$ with slot number $t$[^last-applied-slot],
and chain extensions $E$ with corresponding headers $H$ such that:

$$
0 \leq t_E - t \leq \mathsf{StabilityWindow}
$$

we have:

$$
\vdash s \xrightarrow[\mathrm{CHAIN}]{E}^{*} s'
\implies
\mathit{nes} \vdash \tilde{s} \xrightarrow[\mathrm{CHAINHEAD}]{H}^{*} \tilde{s}'
$$

where $s=(\mathit{nes},\tilde{s})$,
$t_E$ is the maximum slot number appearing in the blocks contained in
$E$, and $H$ is obtained from $E$ by extracting the header from each block in $E$.

## Body-only validation

<a id="prop:body-only-validation"></a>

**Property 2 (Body only validation).**

For all states $s$ with slot number $t$, and chain
extensions $E = [b_0, \ldots, b_n]$ with corresponding headers $H = [h_0, \ldots, h_n]$ such that:

$$
0 \leq t_E - t \leq \mathsf{StabilityWindow}
$$

we have that for all $i \in [1, n]$:

$$
\begin{aligned}
&\mathit{nes} \vdash \tilde{s} \xrightarrow[\mathrm{CHAINHEAD}]{H}^{*} s_h \\
&\quad\wedge\quad
\vdash (\mathit{nes},\tilde{s}) \xrightarrow[\mathrm{CHAIN}]{[b_0,\ldots,b_{i-1}]}^{*} s_{i-1} \\
&\quad\implies\quad
\mathit{nes}' \vdash \tilde{s}_{i-1} \xrightarrow[\mathrm{CHAINHEAD}]{h_i} s'_h
\end{aligned}
$$

where $s=(\mathit{nes},\tilde{s})$, $s_{i-1}=(\mathit{nes}',\tilde{s}_{i-1})$,
$t_E$ is the maximum slot number appearing in the blocks contained in $E$.

[Property 2](#prop:body-only-validation) states that if we validate a sequence
of headers, we can validate their bodies independently and be sure that the
blocks will pass the chain validation rule. To see this, given an environment
$e$ and initial state $s$, assume that a sequence of headers
$H = [h_0, \ldots, h_n]$ corresponding to blocks in $E = [b_0, \ldots, b_n]$ is
valid according to the [CHAINHEAD](../src/Spec/ChainHead.lagda.md#fig:ts-rules:chainhead) transition system:

$$
\mathit{nes} \vdash \tilde{s} \xrightarrow[\mathrm{CHAINHEAD}]{H}^{*} \tilde{s}'
$$

Assume the bodies of $E$ are valid
according to the $\mathsf{BBODY}$ rules (defined in the [Cardano ledger specification](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/shelley-ledger.pdf)), but $E$ is not valid according to
the $\mathsf{CHAIN}$ rule. Assume that there is a $b_j \in E$ such that it is
**the first block** that does not pass the $\mathsf{CHAIN}$
validation. Then:

$$
\vdash (\mathit{nes},\tilde{s}) \xrightarrow[\mathrm{CHAIN}]{[b_0,\ldots,b_{j-1}]}^{*} s_j
$$

But by [Property 2](#prop:body-only-validation) we know that

$$
\mathit{nes}_j \vdash \tilde{s}_j \xrightarrow[\mathrm{CHAINHEAD}]{h_j} \tilde{s}_{j+1}
$$

which means that block $b_j$ has valid headers, and this in turn means that the
validation of $b_j$ according to the chain rules must have failed because it
contained an invalid block body. But this contradicts our assumption that the
block bodies were valid.

[^last-applied-slot]: i.e. the component $s_\ell$ of the last applied block of $s$ equals $t$.

[Specification contents](../SPECIFICATION.md)
