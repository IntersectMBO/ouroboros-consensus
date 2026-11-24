# This script merely needs the numpy and scipy packages.
#
# See the model in the adjacent `./model.md` file. For given Delta and CUTOFF
# parameters, this script finds the X and Y pairs that minimize EPSILON.

import numpy as np
from scipy.stats import nbinom

import sys

# Every function in this module that takes multiple parameters is declared to
# take a first parameter of *. The only reason for that is to force all
# call-sites to use named parameters.

hr = lambda ar: 1 - ar

ascRecip = 20
asc      = 1 / ascRecip
k        = 2160

phi = lambda alpha: 1 - (1 - asc) ** alpha

mu = lambda *, ar, Delta, W: W/( Delta + 1/phi(hr(ar)) )

# Find X, Y, CUTOFF, and EPSILON such that Pr(t1 >= X || d >= Y) + Pr(BAD | t1 < X, d < Y) <= EPSILON

# u = e^(-2 (1 + mu(X) - k)^2 / X), assuming mu(X) >= k
# v = e^(-0.0716 - 0.0523 Y), assuming ar=1/3
# Pr(t1 >= X || d >= Y) <= 1 - (1 - u)(1 - v)

minX = lambda *, ar, Delta: np.ceil(k * ( Delta + 1/phi(hr(ar)) ))   # the least X such that mu(X) >= k

u = lambda *, ar, Delta, X: np.exp(-2 * (1 + mu(ar=ar, Delta=Delta, W=X) - k)**2 / X)   # this requires mu(X) >= k
v = lambda *, ar, Y: np.exp(icepts[ar] + slopes[ar] * Y)   # this requires ar is 0.33 or 0.05, 0.1, ..., 0.35, or 0.4.

# | adversarial power | SLOPE          | INTERCEPT      |
# | ----------------- | -------------- | -------------- |
# | 0.05              | -0.7989658122  | -0.4016605312  |
# | 0.1               | -0.5285237423  | -0.4294425458  |
# | 0.15              | -0.3519941805  | -0.3761753155  |
# | 0.2               | -0.2293731859  | -0.330014553   |
# | 0.25              | -0.1434970731  | -0.2538423842  |
# | 0.3               | -0.08211497258 | -0.1841959922  |
# | 0.33              | -0.05231194796 | -0.07156497066 |
# | 0.35              | -0.04091084381 | -0.1024348218  |
# | 0.4               | -0.01509695029 | -0.06990416538 |
slopes = dict(((0.05, -0.7989658122), (0.1, -0.5285237423), (0.15, -0.3519941805), (0.2, -0.2293731859), (0.25, -0.1434970731), (0.3, -0.08211497258), (0.33, -0.05231194796), (0.35, -0.04091084381), (0.4, -0.01509695029)))
icepts = dict(((0.05, -0.4016605312), (0.1, -0.4294425458), (0.15, -0.3761753155), (0.2, -0.330014553 ), (0.25, -0.2538423842), (0.3, -0.1841959922 ), (0.33, -0.07156497066), (0.35, -0.1024348218 ), (0.4, -0.06990416538)))

def part1(*, ar, Delta, X, Y):
    us = u(ar=ar, Delta=Delta, X=X)
    vs = v(ar=ar, Y=Y)
    return us + vs - us * vs

# Pr(BAD | t1 < X, d < Y) <= Pr(BAD | t1=X-1, d=Y-1), ie NegativeBinomialTrials(k - Y + 1, phi(ar)).cdf(CUTOFF + X - Y)

def part2(*, ar, X, Y, CUTOFF):
  successes = k - Y + 1
  trials    = CUTOFF + X - Y
  failures  = trials - successes
  return nbinom(successes, phi(ar)).cdf(failures)

def search(*, ar, Delta, CUTOFF):
    megaX = np.arange(minX(ar=ar, Delta=Delta), 1 +   3 * k * ascRecip + 1)   # all possible X values
    megaY = np.arange(0, 1 +   k)   # all possible Y values

    # each iteration of this loop requires about 1G of memory at once
    minEpsilon = np.inf
    stride     = 10
    for offset in range(stride):
        X = megaX[offset::stride,np.newaxis]
        Y = megaY[np.newaxis,:]

        epsilon       = part1(ar=ar, Delta=Delta, X=X, Y=Y) + part2(ar=ar, X=X, Y=Y, CUTOFF=CUTOFF)
        newMinEpsilon = epsilon.min()

        if newMinEpsilon < minEpsilon:
            minEpsilon = newMinEpsilon

            xindices, yindices = np.where(epsilon == minEpsilon)

            xys      = np.empty((2, len(xindices)))
            xys[0,:] = X[xindices,0]
            xys[1,:] = Y[0,yindices]

            del xindices, yindices
    
    del megaX, megaY, X, Y, stride

    print(f"ar={ar} Delta={Delta} CUTOFF={CUTOFF} allows a minimum of EPSILON={minEpsilon}.")

    print("X and Y pairs with that EPSILON value (first row is X, second is Y):")
    print(xys)

    parts      = np.empty((3, len(xys[0])))
    parts[0,:] = u(ar=ar, Delta=Delta, X=xys[0,:])
    parts[1,:] = v(ar=ar, Y=xys[1,:])
    parts[2,:] = part2(ar=ar, X=xys[0,:], Y=xys[1,:], CUTOFF=CUTOFF)

    print("The three first-order summands of the corresponding EPSILON calculations (first row is u, second is v, third is Pr(BAD | t1=X-1, d=Y-1):")
    print(parts)

if __name__ == "__main__":
    search(ar=float(sys.argv[1]), Delta=int(sys.argv[2]), CUTOFF=int(sys.argv[3]))
