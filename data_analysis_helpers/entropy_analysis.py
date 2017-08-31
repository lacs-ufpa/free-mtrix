# -*- coding: utf-8 -*-
"""
Free-mtrix - Free cultural selection and social behavior experiments.
Copyright (C) 2016-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

You should have received a copy of the GNU General Public License
along with this program. If not, see http://www.gnu.org/licenses/.
"""

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
