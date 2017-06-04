# -*- coding: utf-8 -*-
"""
Lê colunas de interesse e calcula a porcentagem móvel
de acordo com o valor de data_interval
"""
from math import log
# import numpy as np

def entropy(bins, cycles, n_combinations=10):
    """
    Computes normalized entropy for n combinations.
    """
    if cycles <= 1:
        return 0

    if n_combinations <= 1:
        return 0

    probabilities = [i/cycles for i in bins if i > 0]
    
    # bins = np.bincount(discrete_frequency)
    # n_combinations = np.count_nonzero(bins)
    # n_combinations = len(probabilities)

    E = 0.
    for p in probabilities:
        E -= p * log(p, 2)/log(n_combinations, 2)

    return E

if __name__ == "__main__":   
    F = [8, 12, 5, 3, 7, 9, 3, 4, 4, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    print(entropy(F, 100))