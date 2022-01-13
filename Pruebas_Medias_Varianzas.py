# -*- coding: utf-8 -*-
"""
Created on Tue Sep 21 17:54:11 2021

@author: ppa16
"""

#%% introduction

# import packages
import os
import pandas as pd
import datetime
import re
from scipy.stats import levene
from scipy.stats import bartlett
from scipy.stats import shapiro
from scipy.stats import anderson
from scipy.stats import jarque_bera
from scipy.stats import ttest_ind

# create date (today)
hoy = datetime.datetime.today()
fecha = str(hoy.day).zfill(2)+str(hoy.month).zfill(2)+str(hoy.year)

# project working directory
ruta_proy = "D:/01_Trabajos/04_MINEDU_DIPODA/02_Actividades/04_Focalizacion/"
os.chdir(ruta_proy)
os.listdir('03_ouputs')

# import datasets
df = pd.read_excel('./03_ouputs/base_casos_28092021.xlsx')
df.columns.tolist()

df.Estadistico.value_counts()
df.Cuadro.value_counts()
df.Percentil.value_counts()
df.Caso.value_counts()
df.Escenario.value_counts()

#%% bivariate

list_bivar = []
percentiles = ['p95', 'p96', 'p97']
cuadros = df.Cuadro.unique().tolist()
for cua in cuadros:
    for perc in percentiles:
        df_sel = df[(df.Estadistico=='Proporciones') & (df.Cuadro==cua) & (df.Percentil==perc)]
        LosEscenarios = df_sel.Escenario.unique().tolist()
        for var1 in LosEscenarios:
            for var2 in LosEscenarios:
                x = df_sel[(df_sel.Caso=='distrito') & (df_sel.Escenario==var1) & (~df_sel.Indicador.isna())]['Indicador']
                y = df_sel[(df_sel.Caso=='distrito') & (df_sel.Escenario==var2) & (~df_sel.Indicador.isna())]['Indicador']
                stat_bar, p_bar = bartlett(x, y)
                stat_lev, p_lev = levene(x, y, center = 'mean')
                stat_mean, p_mean = ttest_ind(x, y)
                list_bivar.append([cua, perc, var1, var2,'2-sample t-test', stat_mean, p_mean])
                list_bivar.append([cua, perc, var1, var2,'bartlett', stat_bar, p_bar])
                list_bivar.append([cua, perc, var1, var2,'levene', stat_lev, p_lev])
    
test_bivar = pd.DataFrame(list_bivar)
test_bivar.columns = ['Cuadro', 'Percentil','Escenario1', 'Escenario2', 'Nombre_test', 'statistic', 'pvalue']


#%% univariate

list_univar = []
percentiles = ['p95', 'p96', 'p97']
for cua in cuadros:
    for perc in percentiles:
        df_sel = df[(df.Estadistico=='Proporciones') & (df.Cuadro==cua) & (df.Percentil==perc)]
        LosEscenarios = df_sel.Escenario.unique().tolist()
        for var1 in LosEscenarios:
            x = df_sel[(df_sel.Caso=='distrito') & (df_sel.Escenario==var1) & (~df_sel.Indicador.isna())]['Indicador']
            stat_sha, p_sha = shapiro(x)
            stat_jar, p_jar = jarque_bera(x)
            result = anderson(x)
            stat_and, p_and = result.statistic, result.critical_values[2]
            list_univar.append([cua, perc, var1,'shapiro', stat_sha, p_sha])
            list_univar.append([cua, perc, var1,'jarque-bera', stat_jar, p_jar])
            list_univar.append([cua, perc, var1,'anderson', stat_and, p_and])
    
test_univar = pd.DataFrame(list_univar)
test_univar.columns = ['Cuadro', 'Percentil','Escenario1', 'Nombre_test', 'statistic', 'pvalue']


#%% matrix (variances)

test_bivar['Escenario1'] = ['01_Nacional'     if (re.search('nac', a)) else
                            '02_Departamento' if (re.search('dep', a)) else
                            '03_Provincia'    if (re.search('prov', a)) else 
                            '04_Distrito' if (re.search('dist', a)) else ('05_Colegio')
                            for a in test_bivar['Escenario1'].astype(str)]

test_bivar['Escenario2'] = ['01_Nacional'     if (re.search('nac', a)) else
                            '02_Departamento' if (re.search('dep', a)) else
                            '03_Provincia'    if (re.search('prov', a)) else 
                            '04_Distrito' if (re.search('dist', a)) else ('05_Colegio')
                            for a in test_bivar['Escenario2'].astype(str)]

test_bivar.columns.tolist()
mat = (test_bivar.pivot(index=['Cuadro','Percentil', 'Nombre_test', 'Escenario1'], columns='Escenario2', values=['pvalue','statistic'])).reset_index()
mat.columns.tolist()

#%% export

writer = pd.ExcelWriter('./03_ouputs/Pruebas_Estadisticas4_'+fecha+'.xlsx')
test_univar.to_excel(writer, sheet_name='Univariados_var', index=False)
test_bivar.to_excel(writer, sheet_name='Bivariados_var',index=False)
mat.to_excel(writer, sheet_name='Resumen_biv_var')
writer.save()
    
