---
title: Survivable Eclipse Durations
author: nick.frisby@iohk.io
---

# Survivable Eclipse Durations

Disclaimer: This specification resulted from a proof sketch by Alexander Russell; mistakes are likely my own.

## Introduction

The model specified below relates the duration of an single-node eclipse to the probability of the attacker being able to prevent the victim from automatically rejoining the honest network when the eclipse ends.

- It is assumed the adversary was not grinding.
- It is assumed that all honest stake pools are online and caught-up.
  This is somewhat optimistic, but has been very nearly true for most of Cardano's history so far.
- It is assumed the eclipse victim is not a stake pool.
  If the victim is a stake pool, the numbers should be very similar, since a single pool controls only very small fraction of the honest stake.

If the victim is unable to automatically rejoin the honest network, they would need to first identify that fact and then second manually intervene (eg terminate the node, delete its on-disk state, and restart the node) in order to rejoin.

## Model

Aliases.

- Let C be the victim's immutable tip as of the onset of the eclipse.

Random variables.

- Let t1 be a random variable that is the age of C as of the onset of the eclipse.
- Let d be a random variable that is the maximum length of all short forks that branch off of C and end with an honest block.
- Let t2 be a random variable that is how long it takes after the onset of the eclipse for the adversary to have been elected in k - d slots after d+tipSlot(C).

We desire a bound Pr(BAD) < EPSILON where BAD is the event in which the adversary succeeds despite t2 < CUTOFF for some sufficiently long duration CUTOFF and a sufficiently low probability EPSILON.

We know the following, for a 1/3 adversary.

- (1) Assuming all honest stake is online and caught-up, details of the Praos election mechanism ensure Pr(t1 > X) ≤ e^(-2 (k - mu(X) - 1)^2 / X).
  See the appendix below for the derivation.
- (2) The probability Pr(d ≥ Y) has an upper bound approximated by e^(-0.0716 - 0.0523 Y).
  TODO explanation/citation from Alexander Russell (and Peter Gaži?)
- (3) The probability Pr(BAD | t1=x, d=y) has an upper bound of NegativeBinomialTrials(k - y, phi(0.33)).cdf(CUTOFF + x - y).
  The subtraction of y from CUTOFF is pessimistically assuming that the short fork of length d of perfectly dense.

Consider the following expansion of Pr(BAD) via the law of total probability with respect to the event t1 < X && d < Y.

- Pr(BAD) = Pr(t1 ≥ X || d ≥ Y) Pr(BAD | t1 ≥ X || d ≥ Y) + Pr(t1 < X, d < Y) Pr(BAD | t1 < X, d < Y)

If the stronger Pr(t1 ≥ X || d ≥ Y) + Pr(BAD | t1 < X, d < Y) ≤ EPSILON holds, then we definitely have Pr(BAD) ≤ EPSILON, regardless of how much smaller than 1 the probabilities Pr(BAD | t1 ≥ X || d ≥ Y) and Pr(t1 < X, d < Y) actually are, which is useful since we haven't yet established upper bounds for those.

Let u and v abbreviate the upper bounds (1) and (2) above, so that we have Pr(t1 ≥ X) ≤ u and Pr(d ≥ Y) ≤ v.
This implies Pr(t1 ≥ X || d ≥ Y) ≤ 1 - (1 - u)(1 - v) (which simplifies to u + v - uv, FYI).

Finally, since Pr(BAD) is covariant with respect to t1 and also with respect to d (since its occurrence in k - y dominates its occurrence in the cdf argument), we have Pr(BAD | t1 < X, d < Y) ≤ Pr(BAD | t1=X-1, d=Y-1), ie the upper bound (3) above.

## Conclusion

If we can find coherent values of X, Y, CUTOFF, and EPSILON, then an eclipse lasting less than CUTOFF will defeat the victim only with a probability less than EPSILON, unless the adversary has more than 1/3 stake, the adversary was significantly grinding, or else too much of the honest stake was not online, not caught-up, or not sufficiently well-connected to satisfy the Delta value.

## Appendix 1: Bounding age of immutable tip

Let GR_W be a random variable that counts the minimum possible growth of the honest chain within some fixed interval of W slots (ie the growth assuming every message was delayed by the full Delta).
Note that hypothetically toggling whether or not there was an honest success in some specific slot of those W slots can only change the value of GR_W by at most one.
(TODO This seems right to me, but probably deserves a proof sketch.)
Also, note that the expected value of GR_W is mu(W) = W/( Delta + 1/phi_f(0.66) ), according to "the growth constant".
(TODO citation for this?)
Thus, McDiarmid's Inequality can be applied with n=W and c_i=1.

Note that t1 and GR_W are almost duals: whereas GR_W characterizes the fewest blocks that could arise from exactly W slots, t1 instead characterizes exactly how many slots will give rise to exactly k blocks.
In particular, we have Pr(t1 > X) ≤ Pr(GR_X < k).
If GR were the actual chain growth instead of the minimal growth, this would be an equality.
Note that GR_X = k merely implies t1 ≤ X, since GR_X is itself a minimum but t1 is exact; hence it's a less-than inequality.

Thus Pr(t1 > X) ≤ Pr(GR_X < k) ≤ e^(-2 (1 + mu(X) - k)^2 / X) can be derived as follows, for any X such that mu(X) ≥ k, since McDiarmid's epsilon parameter must be positive.

```
   GR_X         < k
⇔  GR_X - mu(X) < k - mu(X)
⇔  GR_X - mu(X) ≤ k - mu(X) - 1
⇔  GR_X - mu(X) ≤ -(1 + mu(X) - k)
⇔  GR_X - mu(X) ≤ -epsilon
```

## Appendix 2: Example values

Recall the following context.

- C is the victim's immutable tip as of the onset of the eclipse.
- t1 is a random variable that is the age of C as of the onset of the eclipse.
- d is a random variable that is the maximum length of all short forks that branch off of C and end with an honest block.
- t2 is a random variable that is how long it takes after the onset of the eclipse for the adversary to have been elected in k - d slots after d+tipSlot(C).
- Each column below relates to the bound Pr(BAD) < EPSILON where BAD is the event in which the adversary succeeds despite t2 < CUTOFF for some sufficiently long duration CUTOFF and a hopefully-sufficiently-low probability EPSILON.
- X and Y are upper bounds such that t1 < X and d < Y.
- Each row is the unique result of a search for the X and Y pair that minimizes EPSILON for trhe given Delta and CUTOFF.
- u and v are upper bounds such that Pr(t1 ≥ X) ≤ u and Pr(d ≥ Y) ≤ v.
- The marginal is Pr(BAD | t1=X-1, d=Y-1).


| ar | Delta | CUTOFF |   | EPSILON |   | X | Y | u | v | marginal |
| -- | ----- | ------ | - | ------- | - | - | - | - | - | -------- |
| 1/3 | 2 | 900 || 3.23517386672589e-07 || 96253 | 301 | 1.32930626e-07 | 1.35553424e-07 | 5.50333546e-08 |
| 1/3 | 2 | 7200 || 3.856375089639755e-06 || 93645 | 254 | 1.58427041e-06 | 1.58361555e-06 | 6.88491637e-07 |
| 1/3 | 5 | 900 || 1.597387332248975e-05 || 102003 | 229 | 7.30845118e-06 | 5.85449978e-06 | 2.81096516e-06 |
| 1/3 | 5 | 1800 || 2.18293950184661e-05 || 101594 | 223 | 9.96050147e-06 | 8.01256198e-06 | 3.85641138e-06 |
| 1/3 | 5 | 2700 || 2.976098529190354e-05 || 101186 | 217 | 1.35245010e-05 | 1.09661204e-05 | 5.27051220e-06 |
| 1/3 | 5 | 3600 || 4.0469709023602656e-05 || 100739 | 212 | 1.88436508e-05 | 1.42436409e-05 | 7.38268569e-06 |
| 1/3 | 5 | 4500 || 5.489001117727603e-05 || 100334 | 206 | 2.53693031e-05 | 1.94940747e-05 | 1.00271280e-05 |
| 1/3 | 5 | 5400 || 7.427118506065455e-05 || 99929 | 200 | 3.40515618e-05 | 2.66799023e-05 | 1.35406295e-05 |
| 1/3 | 5 | 6300 || 0.00010022354013633725 || 99486 | 195 | 4.68203337e-05 | 3.46539099e-05 | 1.87509191e-05 |
| 1/3 | 5 | 7200 || 0.00013488966845019037 || 99084 | 189 | 6.23069138e-05 | 4.74278950e-05 | 2.51578147e-05 |
| 1/3 | 10 | 900 || 0.002300867769704042 || 109933 | 139 | 0.00123089 | 0.00064821 | 0.00042257 |
| 1/3 | 10 | 7200 || 0.011336634270310585 || 106495 | 110 | 0.00618766 | 0.00295399 | 0.00221327 |
