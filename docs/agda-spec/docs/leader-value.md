# Leader Value Calculation

<a id="sec:leader-value-calc"></a>

This section details how we determine whether a node is entitled to lead (under
the Praos protocol) given the output of its verifiable random function
calculation.

*Values associated with the leader value calculations*

| Value | Range | Meaning |
| --- | --- | --- |
| $\mathit{certNat}$ | $\{n \mid n \in \mathbb{N}, n \in [0,2^{512})\}$ | Certified natural value from VRF |
| $f$ | $(0,1]$ | Active slot coefficient |
| $\sigma$ | $[0,1]$ | Stake proportion |

## Computing the leader value

The verifiable random function gives us a 64-byte random output. We interpret
this as a natural number $\mathit{certNat}$ in the range $[0,2^{512})$.

## Node eligibility

As per [Ouroboros Praos](https://eprint.iacr.org/2017/573), a node is eligible to lead when its leader value
$p < 1 - (1 - f)^\sigma$. We have, for $f<1$:

$$
\begin{aligned}
p &< 1 - (1-f)^\sigma \\
\iff \left(\frac{1}{1-p}\right) &< \exp(-\sigma \cdot \ln(1-f))
\end{aligned}
$$

The latter inequality can be efficiently computed through use of its Taylor
expansion and error estimation to stop computing terms once we are certain that
the result will be either above or below the target value.

We carry out all computations using fixed precision arithmetic (specifically, we
use 34 decimal bits of precision, since this is enough to represent the fraction
of a single lovelace.)

As such, we define the following:

$$
\begin{aligned}
p &= \frac{\mathit{certNat}}{2^{512}} \\
q &= 1-p \\
c &= \ln(1-f)
\end{aligned}
$$

and define the function *checkLeaderVal* as follows:

$$
\mathit{checkLeaderVal}\;\mathit{certNat}\;f\;\sigma =
\begin{cases}
\mathsf{True}, & f=1 \\
\frac{1}{q} < \exp(-\sigma \cdot c), & \text{otherwise}
\end{cases}
$$

The [formal definition](../src/Spec/Protocol.lagda.md) is parameterized by the
rational `exp` and `ln` operations; the executable foreign interface supplies
these operations.

[Specification contents](../SPECIFICATION.md)
