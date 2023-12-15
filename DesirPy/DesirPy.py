import numpy as np


def d_central(x, cut1, cut2, cut3, cut4, scale_low=1, scale_high=1):
    if cut1 >= cut2:
        raise ValueError("The first cutoff value (cut1) should be smaller than the second cutoff value (cut2). ")
    if cut2 > cut3:
        raise ValueError("The second cutoff value (cut2) should be smaller than or equal to the third cutoff value (cut3). ")
    if cut3 >= cut4:
        raise ValueError("The third cutoff value (cut3) should be smaller than the fourth cutoff value (cut4). ")
    if scale_low <= 0 or scale_high <= 0:
        raise ValueError("Scale values should be greater than zero.")

    d = np.empty(len(x))
    d.fill(np.nan)

    for i, val in enumerate(x):
        if np.isnan(val):
            continue
        if val <= cut1 or val >= cut4:
            d[i] = 0
        elif cut2 <= val <= cut3:
            d[i] = 1
        elif cut1 < val < cut2:
            d[i] = ((val - cut1) / (cut2 - cut1)) ** scale_low
        elif cut3 < val < cut4:
            d[i] = ((val - cut4) / (cut3 - cut4)) ** scale_high

    return d


def d_ends(x, cut1, cut2, cut3, cut4, scale_low=1, scale_high=1):
    if cut1 >= cut2:
        raise ValueError("The first cutoff value (cut1) should be smaller than the second cutoff value (cut2).")
    if cut2 > cut3:
        raise ValueError("The second cutoff value (cut2) should be smaller than or equal to the third cutoff value (cut3).")
    if cut3 >= cut4:
        raise ValueError("The third cutoff value (cut3) should be smaller than the fourth cutoff value (cut4).")
    if scale_low <= 0 or scale_high <= 0:
        raise ValueError("Scale values should be greater than zero.")

    d = np.empty(len(x))
    d.fill(np.nan)

    for i, val in enumerate(x):
        if np.isnan(val):
            continue
        if val <= cut1 or val >= cut4:
            d[i] = 1
        elif cut2 <= val <= cut3:
            d[i] = 0
        elif cut1 < val < cut2:
            d[i] = ((val - cut2) / (cut1 - cut2)) ** scale_low
        elif cut3 < val < cut4:
            d[i] = ((val - cut3) / (cut4 - cut3)) ** scale_high

    return d

def d_max(x, cut1, cut2, scale=1):
    if cut1 >= cut2:
        raise ValueError("The first cutoff value (cut1) should be smaller than the second cutoff value (cut2).")
    if scale <= 0:
        raise ValueError("The scale value should be greater than zero.")

    d = ((x - cut1) / (cut2 - cut1)) ** scale
    d[x < cut1] = 0
    d[x > cut2] = 1

    return d

def d_min(x, cut1, cut2, scale=1):
    if cut1 >= cut2:
        raise ValueError("The first cutoff value (cut1) should be smaller than the second cutoff value (cut2).")
    if scale <= 0:
        raise ValueError("The scale value should be greater than zero.")

    d = ((x - cut2) / (cut1 - cut2)) ** scale
    d[x < cut1] = 1
    d[x > cut2] = 0

    return d


def d_overall(*args, p=0, weights=None):
    if len(args) == 0:
        raise ValueError("You need to include at least one set of desirability scores.")

    d_all = np.column_stack(args)

    if np.any(d_all < 0) or np.any(d_all > 1):
        raise ValueError("All desirability scores should fall within the range 0 to 1.")

    if p == 0:  # Geometric mean
        if weights is None:
            weights = np.ones(d_all.shape[1]) / d_all.shape[1]
        if len(weights) != d_all.shape[1]:
            raise ValueError("The number of provided weights must match the number of desirability score sets.")
        weighted_scores = np.power(d_all, weights)
        product = np.nanprod(weighted_scores, axis=1)
        sum_weights = np.sum(weights)
        result = np.power(product, 1 / sum_weights)
    elif p == 1:  # Arithmetic mean
        result = np.nanmean(d_all, axis=1)
    elif p == 2:  # Euclidean mean
        result = np.sqrt(np.nansum(np.square(d_all), axis=1) / d_all.shape[1])
    elif p == -np.inf:  # Minimum
        result = np.nanmin(d_all, axis=1)
    elif p == np.inf:  # Maximum
        result = np.nanmax(d_all, axis=1)
    else:
        raise ValueError("Invalid value of p. Choose from 0, 1, 2, -inf, inf.")

    return result