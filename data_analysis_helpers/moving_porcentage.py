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
# Lê colunas de interesse e calcula a porcentagem móvel
# de acordo com o valor de data_interval

# todo: adequar ao padrão de codificação do arquivo

# importa e declara as dependências
import os
import numpy as np

def load_file(path):
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
        skip_header=5,
        skip_footer=2
        )

def analyse_condition(column_name,condition, offset=None):
    if not offset:
        offset = 0
    data_filtered = session[column_name]
    data_filtered = [item for i, item in enumerate(data_filtered) if condition == int(session['Condicao'][i].decode())]
    
    # demarca o início
    begin_row = 0
    end_row = data_interval

    # contador de porcentagens já calculadas
    count = 0

    # percorre os dados de acordo com o intervalo
    while end_row <= len(data_filtered): 
        
        # lê o intervalo definido
        target_data = data_filtered[begin_row:end_row]

        # calcula a porcentagem do intervalo
        porcentage = (sum(target_data)/float(data_interval))
        
        # imprime o resultado na tela
        print(condition,'\t',end_row+offset,'\t',porcentage)

        # vai para o próximo intervalo
        begin_row += data_interval
        end_row += data_interval

        # incrementa o contador de porcentagens calculadas
        count+=1
        
    # calcula eventuais valores quebrados
    remainder = len(data_filtered)%data_interval
    
    # se houver sobra, utilize-a como base da porcentagem final
    if remainder > 0:
        end_row = begin_row+remainder
        target_data = data_filtered[begin_row:end_row]
        porcentage = (sum(target_data)/float(remainder))
        print(condition,'\t',end_row+offset,'\t',porcentage,'\t','sobra:'+str(remainder))
        count+=1
        end_row += data_interval 
    
    end_row -= data_interval

    print('Total: ',count)
    return end_row+offset

def analyse_column(column_name,conditions):
    offset=0
    print('Analisando dados da coluna: '+ column_name)
    for i in conditions:
        offset = analyse_condition(column_name,i,offset)


if __name__ == "__main__":
    # assume o local dos arquivos de sessão como igual ao local deste arquivo
    root_path = os.path.dirname(os.path.abspath(__file__))

    # declara explicitamente os nomes dos arquivos de sessão alvo
    session_names = [
        '033.data'
        ]

    # declara o valor do intervalo usado para mover a porcentagem
    data_interval = 10

    # percorre todos os arquivos de sessão
    for session_name in session_names:

        # carrega os dados da sessão
        session = load_file(session_name)
        
        # analisa e plota os valores da coluna alvo
        conditions = range(1,7)
        analyse_column('AutocontroleP1',conditions)
        analyse_column('AutocontroleP2',conditions) 
        analyse_column('AutocontroleP3',conditions)
        analyse_column('Ref',conditions)
        analyse_column('Pun',conditions)
