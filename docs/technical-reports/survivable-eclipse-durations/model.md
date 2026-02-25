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

We know the following.

- (1) Assuming all honest stake is online and caught-up, details of the Praos election mechanism ensure Pr(t1 > X) ≤ e^(-2 (k - mu(X) - 1)^2 / X).
  See the appendix below for the derivation.
- (2) The probability Pr(d ≥ Y) has an upper bound approximated by an exponential.
  For a 33% adversary, it's e^(-0.0716 - 0.0523 Y).
  For other increments, see the following table, derived by Peter Gaži via [a recent publication](https://iohk.io/en/research/library/papers/practical-settlement-bounds-for-longest-chain-consensus).

| adversarial power | SLOPE          | INTERCEPT      |
| ----------------- | -------------- | -------------- |
| 0.05              | -0.7989658122  | -0.4016605312  |
| 0.1               | -0.5285237423  | -0.4294425458  |
| 0.15              | -0.3519941805  | -0.3761753155  |
| 0.2               | -0.2293731859  | -0.330014553   |
| 0.25              | -0.1434970731  | -0.2538423842  |
| 0.3               | -0.08211497258 | -0.1841959922  |
| 0.33              | -0.05231194796 | -0.07156497066 |
| 0.35              | -0.04091084381 | -0.1024348218  |
| 0.4               | -0.01509695029 | -0.06990416538 |

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
- Each row is the result of a search for the X and Y pair that minimizes EPSILON for the given Delta and CUTOFF.
- u and v are upper bounds such that Pr(t1 ≥ X) ≤ u and Pr(d ≥ Y) ≤ v.
- The marginal is Pr(BAD | t1=X-1, d=Y-1).


| ar | Delta | CUTOFF |   | EPSILON |   | X | Y | u | v | marginal |
| -- | ----- | ------ | - | ------- | - | - | - | - | - | -------- |
| 0.1 | 2 | 900 || 1.5833933340757943e-69 || 129601 | 370..935 | | | |
| 0.1 | 2 | 1800 || 1.5833933340757943e-69 || 129601 | 370..928 | | | |
| 0.1 | 2 | 3600 || 1.5833933340757943e-69 || 129601 | 370..916 | | | |
| 0.1 | 2 | 5400 || 1.5833933340757943e-69 || 129601 | 370..903 | | | |
| 0.1 | 2 | 7200 || 1.5833933340757943e-69 || 129601 | 370..890 | | | |
| 0.1 | 5 | 900 || 1.9230560914781173e-46 || 129601 | 269..1027 | | | |
| 0.1 | 5 | 1800 || 1.9230560914781173e-46 || 129601 | 269..1021 | | | |
| 0.1 | 5 | 3600 || 1.9230560914781173e-46 || 129601 | 269..1009 | | | |
| 0.1 | 5 | 5400 || 1.9230560914781173e-46 || 129601 | 269..997 | | | |
| 0.1 | 5 | 7200 || 1.9230560914781173e-46 || 129601 | 269..984 | | | |
| 0.1 | 10 | 900 || 3.62820305660143e-24 || 129601 | 171..1132 | | | |
| 0.1 | 10 | 1800 || 3.62820305660143e-24 || 129601 | 171..1126 | | | |
| 0.1 | 10 | 3600 || 3.62820305660143e-24 || 129601 | 171..1114 | | | |
| 0.1 | 10 | 5400 || 3.62820305660143e-24 || 129601 | 171..1102 | | | |
| 0.1 | 10 | 7200 || 3.62820305660143e-24 || 129601 | 171..1091 | | | |
| 0.2 | 2 | 900 || 6.840319013661139e-41 || 122154 | 407 | | | |
| 0.2 | 2 | 1800 || 1.5918917722734485e-40 || 121776 | 403 | | | |
| 0.2 | 2 | 3600 || 8.624334844708336e-40 || 120969 | 396 | | | |
| 0.2 | 2 | 5400 || 4.6049569093214086e-39 || 120220 | 388 | | | |
| 0.2 | 2 | 7200 || 2.438993748339021e-38 || 119420 | 381 | | | |
| 0.2 | 5 | 900 || 1.4702681665699258e-32 || 129601 | 354 | | | |
| 0.2 | 5 | 1800 || 1.472605806985377e-32 || 129601 | 347 | | | |
| 0.2 | 5 | 3600 || 1.5211880103139585e-32 || 129601 | 335 | | | |
| 0.2 | 5 | 5400 || 2.3112663392695925e-32 || 129601 | 323 | | | |
| 0.2 | 5 | 7200 || 9.318401502500436e-32 || 128971 | 315 | | | |
| 0.2 | 10 | 900 || 5.559762182783342e-17 || 129601 | 322..378 | | | |
| 0.2 | 10 | 1800 || 5.559762182783342e-17 || 129601 | 322..367 | | | |
| 0.2 | 10 | 3600 || 5.559762182783342e-17 || 129601 | 322..346 | | | |
| 0.2 | 10 | 5400 || 5.559762182783342e-17 || 129601 | 322..324 | | | |
| 0.2 | 10 | 7200 || 5.559762182783355e-17 || 129601 | 310..312 | | | |
| 0.3 | 2 | 900 || 4.6397497162422064e-12 || 100936 | 327 | | | |
| 0.3 | 2 | 1800 || 7.515471508250259e-12 || 100558 | 321 | | | |
| 0.3 | 2 | 3600 || 1.956527690234275e-11 || 99763 | 310 | | | |
| 0.3 | 2 | 5400 || 5.0433555738838535e-11 || 99014 | 298 | | | |
| 0.3 | 2 | 7200 || 1.286306014824727e-10 || 98228 | 287 | | | |
| 0.3 | 5 | 900 || 1.7424318823840392e-09 || 107337 | 257 | | | |
| 0.3 | 5 | 1800 || 2.6465223247789376e-09 || 106954 | 251 | | | |
| 0.3 | 5 | 3600 || 6.059944641662974e-09 || 106062 | 242 | | | |
| 0.3 | 5 | 5400 || 1.3733642515865873e-08 || 105218 | 232 | | | |
| 0.3 | 5 | 7200 || 3.0805229050467375e-08 || 104377 | 222 | | | |
| 0.3 | 10 | 900 || 3.6021234665432564e-06 || 116378 | 166 | | | |
| 0.3 | 10 | 1800 || 4.978646047179136e-06 || 115854 | 163 | | | |
| 0.3 | 10 | 3600 || 9.436482959058172e-06 || 114899 | 155 | | | |
| 0.3 | 10 | 5400 || 1.7708627824202254e-05 || 113947 | 147 | | | |
| 0.3 | 10 | 7200 || 3.285974997708252e-05 || 112955 | 140 | | | |
| 0.35 | 2 | 900 || 1.9263031455648523e-05 || 94383 | 283 | | | |
| 0.35 | 2 | 1800 || 2.614756997258209e-05 || 94005 | 276 | | | |
| 0.35 | 2 | 3600 || 4.7868196971003117e-05 || 93291 | 261 | | | |
| 0.35 | 2 | 5400 || 8.682887127347237e-05 || 92544 | 247 | | | |
| 0.35 | 2 | 7200 || 0.00015604615130879318 || 91801 | 233 | | | |
| 0.35 | 5 | 900 || 0.00042873545439386603 || 99814 | 211 | | | |
| 0.35 | 5 | 1800 || 0.0005580320584127055 || 99432 | 204 | | | |
| 0.35 | 5 | 3600 || 0.000938327811681011 || 98594 | 192 | | | |
| 0.35 | 5 | 5400 || 0.0015622673095256057 || 97760 | 180 | | | |
| 0.35 | 5 | 7200 || 0.0025742996463979768 || 96930 | 168 | | | |
| 0.35 | 10 | 900 || 0.02149381895709917 || 107226 | 122 | | | |
| 0.35 | 10 | 1800 || 0.02612032352729424 || 106754 | 117 | | | |
| 0.35 | 10 | 3600 || 0.038231179609329306 || 105772 | 108 | | | |
| 0.35 | 10 | 5400 || 0.055244548147987946 || 104751 | 100 | | | |
| 0.35 | 10 | 7200 || 0.07876279237285291 || 103731 | 92 | | | |
| 0.4 | 2 | 900 || 0.06571647551377602 || 89922 | 218 | | | |
| 0.4 | 2 | 1800 || 0.07624067528832018 || 89572 | 209 | | | |
| 0.4 | 2 | 3600 || 0.10204390789124193 || 88906 | 190 | | | |
| 0.4 | 2 | 5400 || 0.1354890757165616 || 88204 | 172 | | | |
| 0.4 | 2 | 7200 || 0.17828184989890705 || 87465 | 155 | | | |
| 0.4 | 5 | 900 || 0.22795257857496215 || 94505 | 143 | | | |
| 0.4 | 5 | 1800 || 0.2573349608023534 || 94095 | 135 | | | |
| 0.4 | 5 | 3600 || 0.32512252751599474 || 93229 | 120 | | | |
| 0.4 | 5 | 5400 || 0.4055108168330902 || 92341 | 105 | | | |
| 0.4 | 5 | 7200 || 0.49833455033965546 || 91385 | 91 | | | |
| 0.4 | 10 | 900 || 0.8369276395990976 || 99384 | 53 | | | |
| 0.4 | 10 | 1800 || 0.8803089455184628 || 98659 | 47 | | | |
| 0.4 | 10 | 3600 || 0.952462203213196 || 97097 | 33 | | | |
| 0.4 | 10 | 5400 || 0.9952386698457745 || 95101 | 17 | | | |
| 0.4 | 10 | 7200 || 1.0000125099740969 || 129601 | 2159..2160 | | | |
