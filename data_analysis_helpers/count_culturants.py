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

import numpy as np

def load_file(path, skip_header=0, skip_footer=0):
    if not os.path.isfile(path):
        print(path)
        raise IOError("O arquivo não foi encontrado: "+path)

    return np.genfromtxt(path,
        delimiter="\t",
        missing_values=["NA"],
        filling_values=None,
        names=True,
        autostrip=True,
        dtype=None,
        skip_header=skip_header,
        skip_footer=skip_footer
        )

culturants = [
    ('MetaImp1', ['AMARELO', 'AMARELO', 'AMARELO']),
    ('MetaImp2', ['AMARELO', 'AMARELO', 'VERDE']),
    ('MetaImp3', ['AMARELO','AMARELO','VERMELHO']),
    ('MetaImp4', ['AMARELO','AMARELO','AZUL']),
    ('MetaImp5', ['AMARELO','VERDE','VERMELHO']),
    ('MetaImp6', ['AMARELO','VERMELHO','AZUL']),
    ('MetaImp7', ['VERMELHO','VERMELHO','VERMELHO']),
    ('MetaImp8', ['VERMELHO','VERMELHO','AMARELO']),
    ('MetaImp9', ['VERMELHO','VERMELHO','VERDE']),
    ('MetaImp10', ['VERMELHO','VERMELHO','AZUL']),
    ('MetaAut1', ['VERDE','VERDE','VERDE']),
    ('MetaAut2', ['VERDE','VERDE','AMARELO']),
    ('MetaAut3', ['VERDE','VERDE','VERMELHO']),
    ('MetaAut4', ['VERDE','VERDE','AZUL']),
    ('MetaAut5', ['VERDE','VERMELHO','AZUL']),
    ('MetaAut6', ['VERDE','AMARELO','AZUL']),
    ('MetaAut7', ['AZUL','AZUL','AZUL']),
    ('MetaAut8', ['AZUL','AZUL','AMARELO']),
    ('MetaAut9', ['AZUL','AZUL','VERDE']),
    ('MetaAut10', ['AZUL','AZUL','VERMELHO'])
]

def count_culturants(condition, cycle):
    for line in condition:
        cycle += 1
        for culturant in culturants:
            (cult_name, cult_colors) = culturant
            if line[cult_name] == 1:
                print('\t'.join([cult_name, str(cycle), str(culturants.index(culturant)+1)]))
    return cycle

def count_culturants_raw(condition, cycle):
    for line in condition:
        cycle += 1
        color_set = [] # colors chosen by participants
        for line_item in line:
            color_set.append(line_item.decode())

        for culturant in culturants:
            (cult_name, cult_colors) = culturant
            cult_colors_sorted = sorted(cult_colors)
            cult_set_sorted = sorted(color_set)
            if cult_colors_sorted == cult_set_sorted:
                print('\t'.join([cult_name, str(cycle), str(culturants.index(culturant)+1)]))
    return cycle

if __name__ == "__main__":

    cycle_id = 0 
    root_path = os.path.dirname(os.path.abspath(__file__))
    session_path = os.path.join(root_path, 'microcultura1_4l')
    condition_file = os.path.join(session_path, '01.data')
    condition = load_file(condition_file)
    cycle_id = count_culturants(condition, cycle_id)

    condition_file = os.path.join(session_path, '02.data')
    condition = load_file(condition_file)
    cycle_id = count_culturants(condition, cycle_id)

    condition_file = os.path.join(session_path, '03.data')
    condition = load_file(condition_file)
    cycle_id = count_culturants_raw(condition, cycle_id)

    condition_file = os.path.join(session_path, '04.data')
    condition = load_file(condition_file)
    cycle_id = count_culturants(condition, cycle_id)

    condition_file = os.path.join(session_path, '05.data')
    condition = load_file(condition_file)
    cycle_id = count_culturants(condition, cycle_id)

    condition_file = os.path.join(session_path, '06.data')
    condition = load_file(condition_file)
    cycle_id = count_culturants_raw(condition, cycle_id)
