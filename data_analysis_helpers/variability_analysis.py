import os
from math import factorial as f

from porcentagem_movel import load_file
from copy import deepcopy


# amarelo = [1,8]
# verde = [2,7]
# vermelho = [3,6]
# azul = [4,9]
# roxo = [5,10]
# preto = [0]
amarelo = 'amarela '
verde = 'verde '
vermelho = 'vermelha '
azul = 'azul '
roxo = 'rosa '
preto = 'preto '

COLOR_SET = {
    '01': [azul + verde + amarelo, 0],
    '02': [azul + verde + vermelho, 0],
    '03': [azul + verde + roxo, 0],
    '04': [azul + amarelo + vermelho, 0],
    '05': [azul + amarelo + roxo, 0],
    '06': [azul + vermelho + roxo, 0],
    '07': [amarelo + verde + vermelho, 0],
    '08': [amarelo + verde + roxo, 0],
    '09': [amarelo + roxo + vermelho, 0],
    '10': [vermelho + verde + roxo, 0],
    '11': [preto + azul + amarelo, 0],
    '12': [preto + azul + vermelho, 0],
    '13': [preto + azul + verde, 0],
    '14': [preto + azul + roxo, 0],
    '15': [preto + amarelo + vermelho, 0],
    '16': [preto + amarelo + verde, 0],
    '17': [preto + amarelo + roxo, 0],
    '18': [preto + vermelho + verde, 0],
    '19': [preto + vermelho + roxo, 0],
    '20': [preto + verde + roxo, 0]
}

def analyse_condition(session, c):
    print(len(session['LA']))
    all_data = zip(session['LA'][c[0]:c[1]], session['LB'][c[0]:c[1]],session['LC'][c[0]:c[1]])
    total_count = len(session['LA'][c[0]:c[1]])
    print('All combinations in condition:', total_count)
    unique = set(all_data)
    unique =     sorted(list(unique))
    # for line in unique:
    #     print(line)

    unique_count = len(unique)
    print('Unique combinations in condition:', unique_count)
    print(unique_count/total_count*100,'%% unique')
    print('')

def count_combinations(session, c, print_each_combination=False):
    color_set = deepcopy(COLOR_SET)
    all_data = zip(session['LAC'][c[0]:c[1]], session['LBC'][c[0]:c[1]],session['LCC'][c[0]:c[1]])
    i = 0
    last_a = None
    for a, b, c in all_data:
        i += 1
        # print(i)
        if last_a:
            if a != last_a:
                for key, value in color_set.items():
                    if a != b != c != a:
                        if a.decode() in value[0]:
                            if b.decode() in value[0]:
                                if c.decode() in value[0]: 
                                    color_set[key][1] += 1
                                    # print(a,b,c)
                                    if print_each_combination:
                                        print(str(i)+'\t'+key+'\t'+str(color_set[key][1]))
        # else:
        # for key, value in color_set.items():
        #     if a != b != c != a:
        #         if a.decode() in value[0]:
        #             if b.decode() in value[0]:
        #                 if c.decode() in value[0]: 
        #                     color_set[key][1] += 1
        last_a = a      
    return color_set


    # unique = set(all_data)
    # unique = sorted(list(unique))
    # for line in unique:
    #     print(line)

    # unique_count = len(unique)
    # print('Unique combinations in condition:', unique_count)
    # print(unique_count/total_count*100,'% unique')
    # print('')

if __name__ == "__main__":
    #root_path = os.path.dirname(os.path.abspath(__file__))
    session_names = [
        'variability_data_mc4.txt'
        ]

    conditions = [
        (0,98),
        (98,195),
        (195,295),
        (295,362)
    ]

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

    for session_name in session_names:
        print(session_name)
        session = load_file(session_name, 0, 0)
        for i, condition in enumerate(conditions):
            print('Condition ', i + 1)

            # contagem das combinações
            color_set = count_combinations(session,condition)
            for key, value in color_set.items():
                print(key,'\t',value[1])

            # contagem por ciclo
            print('Condition ', i + 1, ' por ciclo')
            _ = count_combinations(session,condition,True)
            