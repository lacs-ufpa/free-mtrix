# -*- coding: utf-8 -*-
"""
  Free-mtrix - Free cultural selection and social behavior experiments.   
  Copyright (C) 2016-2019 Carlos Rafael Fernandes Picanço.   
  Copyright (C) 2016-2019 Thais Maria Monteiro Guimarães.   
  Copyright (C) 2016-2019 Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License   
  along with this program. If not, see <http://www.gnu.org/licenses/>.   
"""
import os
from math import factorial as f

import numpy as np
from entropy_analysis import entropy
from porcentagem_movel import load_file
from copy import deepcopy


# lines
# amarelo = [1,8]
# verde = [2,7]
# vermelho = [3,6]
# azul = [4,9]
# roxo = [5,10]
# preto = [0]

# color names
amarelo = 'amarela '
verde = 'verde '
vermelho = 'vermelha '
azul = 'azul '
roxo = 'rosa '
preto = 'preto '

COLOR_SET = {
    1: [azul + verde + amarelo, 0],
    2: [azul + verde + vermelho, 0],
    3: [azul + verde + roxo, 0],
    4: [azul + amarelo + vermelho, 0],
    5: [azul + amarelo + roxo, 0],
    6: [azul + vermelho + roxo, 0],
    7: [amarelo + verde + vermelho, 0],
    8: [amarelo + verde + roxo, 0],
    9: [amarelo + roxo + vermelho, 0],
    10: [vermelho + verde + roxo, 0],
    11: [preto + azul + amarelo, 0],
    12: [preto + azul + vermelho, 0],
    13: [preto + azul + verde, 0],
    14: [preto + azul + roxo, 0],
    15: [preto + amarelo + vermelho, 0],
    16: [preto + amarelo + verde, 0],
    17: [preto + amarelo + roxo, 0],
    18: [preto + vermelho + verde, 0],
    19: [preto + vermelho + roxo, 0],
    20: [preto + verde + roxo, 0]
    }

def analyse_condition(session, c):
    print(len(session['LA']))
    all_data = zip(session['LA'][c[0]:c[1]], session['LB'][c[0]:c[1]],session['LC'][c[0]:c[1]])
    total_count = len(session['LA'][c[0]:c[1]])
    print('All combinations in condition:', total_count)
    unique = set(all_data)
    unique =     sorted(list(unique))
    unique_count = len(unique)
    print('Unique combinations in condition:', unique_count)
    print(unique_count/total_count*100,'%% unique')
    print('')

def count_combinations(session, c, print_each_combination=False,offset=0):
    """
    Count combinations from color names
    """
    color_set = deepcopy(COLOR_SET)
    all_data = zip(session['LAC'][c[0]:c[1]], session['LBC'][c[0]:c[1]],session['LCC'][c[0]:c[1]])
    i = 0
    last_a = None
    for a, b, c in all_data:
        i += 1
        if a != last_a:
            for key, value in color_set.items():
                if a != b != c != a:
                    if a.decode() in value[0]:
                        if b.decode() in value[0]:
                            if c.decode() in value[0]: 
                                color_set[key][1] += 1
                                if print_each_combination:
                                    print(str(i+offset)+'\t'+str(key)+'\t'+str(color_set[key][1]))
        last_a = a      
    return color_set, i+offset

if __name__ == "__main__":
    session_names = [
        'variability_data_mc3.txt'
        ]

    conditions = [
            (0,100),
            (100,150),
            (150,250),
            (250,350)
        ]

    for session_name in session_names:
            print(session_name)
            session = load_file(session_name, 0, 0)
            offset = 0
            for i, condition in enumerate(conditions):
                # contagem cumulativa por ciclo
                print('Condition ', i + 1, ' cumulative count per cycle')
                color_set, offset = count_combinations(session, condition, True, offset)

            print('\n''Condition ', i + 1, 'bins')
                        dtypes = [] 
                        entropy_input = []
                        for key, value in color_set.items():
                            print(key,'\t',value[1])
                            entropy_input.append(value[1])

            # entropia relativa das combinações
            print('Entropy:', entropy(entropy_input[0:10],conditions[i][1]-conditions[i][0]),'\n')

    # max = 220 :
    # print(f(11+3-1)/(f(3)*f(11-1)))

    # unique = set()
    # for session_name in session_names:
    #     print(session_name)
    #     session = load_file(session_name, 0, 0)
    #     for i, condition in enumerate(conditions):
    #         print('Condition ', i+1)
    #         analyse_condition(session, condition)

    #     all_data = zip(session['LA'], session['LB'],session['LC'])
    #     total_count = len(session['LA'])
    #     print('All combinations in session:', total_count)
    #     unique = set(all_data)
    #     unique = sorted(list(unique))
    #     unique_count = len(unique)
    #     print('Unique combinations in session:', unique_count)
    #     print(unique_count/total_count*100,'% unique')
    #     print('')
