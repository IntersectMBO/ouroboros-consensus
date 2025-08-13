#!/usr/bin/env nix-shell
#! nix-shell -i python3 --pure
#! nix-shell -p "python3.withPackages (ps: [ ps.matplotlib ps.scipy ])"
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/08b9151ed40350725eb40b1fe96b0b86304a654b.tar.gz

from math import log
import matplotlib.pyplot as plt
import numpy as np
from scipy import stats

f = 1 / 20
k = 2160
phi = lambda s: 1 - (1 - f) ** s

active_per_k_over_f = stats.binom(k / f, f)
until_one_active = stats.geom(f)
active_per_epoch = stats.binom(10 * k / f, phi(0.5))
active_per_epoch_grind_pmf = lambda grind, n: active_per_epoch.cdf(n) ** grind
leaders_per_slot = stats.poisson(-log(1 - f))


fig, axs = plt.subplots(figsize=(8, 3.5), ncols=2, layout="constrained")

X = np.arange(active_per_k_over_f.mean() - 300, active_per_k_over_f.mean() + 300)
axs[0].plot(X, active_per_k_over_f.pmf(X))
axs[0].set_xlabel("number of active slots")
axs[0].set_ylabel("probability")
axs[0].set_title(f"pmf of the number of active slots\nout of {int(k / f)} total")

X = np.arange(1, 120)
axs[1].plot(X, until_one_active.pmf(X), "x", markersize=2)
axs[1].set_xlabel("number of slots (including the last active one)")
axs[1].set_title(f"pmf of the number of slots\nuntil one is active")

fig.savefig("plot-active-slots.png", dpi=300)


fig, ax = plt.subplots()

X = np.arange(0, 7)
color = "tab:red"
ax.plot(X, leaders_per_slot.pmf(X), "o", color=color)
ax.set_xlabel("number of slot leaders")
ax.set_ylabel("probability", color=color)
ax.tick_params(axis="y", labelcolor=color)
ax.set_title(f"pmf of the number of leaders in a single slot (worst case)")

color = "tab:blue"
logax = ax.twinx()
logax.plot(X, leaders_per_slot.logpmf(X) / log(10), "x", color=color)
logax.set_ylabel("log₁₀(probability)", color=color)
logax.tick_params(axis="y", labelcolor=color)

fig.savefig("plot-single-slot-leaders.png", dpi=300)


fig, ax = plt.subplots(figsize=(9, 4))

X = np.arange(10500, 12000)
ax.plot(X, active_per_epoch.cdf(X), label="no grinding")
ax.plot(X, active_per_epoch_grind_pmf(10**5, X), label="10⁵ grinding")
ax.plot(X, active_per_epoch_grind_pmf(10**10, X), label="10¹⁰ grinding")
ax.plot(X, active_per_epoch_grind_pmf(10**20, X), label="10²⁰ grinding")
ax.legend()
ax.set_xlabel("number of active slots")
ax.set_ylabel("probability")
ax.set_title(f"cdf of the number of active slots per mainnet epoch for an attacker with 50% stake")

fig.savefig("plot-grinding.png", dpi=300)
