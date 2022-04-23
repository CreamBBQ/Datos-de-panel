#-------------------------------------------------------------------------------------------------------------------------#

# El codigo tiene problemas de optimizacion. Sobre todo porque la libreria statsmodel requiere matrices NTxNT 
# para hacer el calculo de los estimadores FGSL entonces el tiempo de ejecucion pasa de +-1m para el 
# primer ejercicio a +5h para el ultimo. Es por esto que para mayor comodidad volque todo el codigo a una jupyter notebook 
# donde esta exactamente el mismo codigo, pero en las celdas de resultado ya están cargados los valores que reporte en el 
# trabajo practico. Para acceder a esa notebook se debe seguir el siguiente enlace:
#
#        ****  https://github.com/CreamBBQ/Datos-de-panel/blob/master/TP1/Mu%C3%B1oz1313.ipynb  ****

#-------------------------------------------------------------------------------------------------------------------------#


import pandas as pd 
import numpy as np 
import statsmodels.api as sm 
import scipy.linalg


beta_0 = -3
beta_1 = 0.8
omega = np.array([[4,0,0,0],
                  [0,9,0,0],
                  [0,0,16,0],
                  [0,0,0,25]])


def get_data(N,T): #Genera cada una de las 5000 muestras siguiendo la forma propuesta por el ejercicio
    data = {'i': [], 't': [], 'y': [], 'cte':[] , 'x': [], 'u': []}
    for person in range(1,N+1): 
        for time in range(1,T+1): 
            data['i'].append(person)
            data['t'].append(time)
            data['cte'].append(1)
            x = np.random.uniform(1,30,1).astype(float)
            u = np.random.normal(0, omega.item(time-1,time-1))
            data['x'].append(x)
            data['u'].append(u)
            data['y'].append(beta_0 + beta_1*x + u)
    return pd.DataFrame(data = data)


def get_omega_est(N, T, df): 
    y = df['y'].astype(float)
    x = df[['cte', 'x']].astype(float)
    omega_est = np.zeros((T,T)) #La matriz TxT que se va a almacenar aca es la vista es la vista en clase: sum_j(u_2hat_j*u_2hat_j_T)
    reg_ols = sm.OLS(y,x).fit()
    u_hat = {'i': df['i'], 't': df['t'], 'u_jt': reg_ols.resid.tolist()} 
    u_hat = pd.DataFrame(data = u_hat)
    for l in range(0, N*T,T): #Equivalente a la sumatoria para cada j 
        u_j = np.array([[u_hat['u_jt'][l+i] for i in range(0,4)]]).T 
        omega_est = omega_est + u_j.dot(u_j.T) 
    aux = omega_est.reshape(1,T,T).repeat((N), axis = 0)
    long_omega_est = scipy.linalg.block_diag(*aux) # Esta matriz es de NTxNT y contiene omega_est en bloques diagonales y el resto cero
    return pd.DataFrame(long_omega_est)


def get_fgls(N, T): 
    data = get_data(N, T)
    omega_est = get_omega_est(N, T, data)
    y = data['y'].astype(float)
    x = data[['cte', 'x']].astype(float)
    return sm.GLS(y, x, omega_est).fit()


def get_simulation_report(muestras, N, T): #Genera los datos pedidos por cada uno de los ejercicios del TP
    np.random.seed(1313) 
    count_power_b0 = 0
    count_power_b1 = 0
    count_size_1= 0
    count_size_5 = 0
    coef_list_0 = []
    coef_list_1 = []
    se_list_b0 = []
    se_list_b1 = []
    for muestra in range(0, muestras):
        fgls = get_fgls(N, T)
        coef_list_0.append(fgls.params[0])
        coef_list_1.append(fgls.params[1])
        se_list_b0.append(fgls.bse[0])
        se_list_b1.append(fgls.bse[1])
        test = fgls.t_test('x = 0.8, cte = 0, x = 0.4') #t-test individual para todos los contrastes a reportar 
        if test.pvalue[0] <= 0.01: 
            count_size_1 = count_size_1 + 1
        if test.pvalue[0] <= 0.05: 
            count_size_5 = count_size_5 + 1 
        if test.pvalue[1] <= 0.01: 
            count_power_b0 = count_power_b0 + 1 
        if test.pvalue[2] <= 0.01: 
            count_power_b1 = count_power_b1 + +1
    report = {'Tamanio al 1%': count_size_1/muestras, 
              'Tamanio al 5%': count_size_5/muestras, 
              'Poder de b_0=0 al 1%': count_power_b0/muestras, 
              'Poder de b_1=0.4 al 1%': count_power_b1/muestras,
              'Media de b_0': np.mean(coef_list_0 ),
              'Media de b_1': np.mean(coef_list_1), 
              'Mediana de b_0': np.median(coef_list_0),
              'Mediana de B_1': np.median(coef_list_1),
              'SE de b_0': np.mean(se_list_b0), 
              'SE de b_1': np.mean(se_list_b1)}
    return report


def run(): 
    print('-------EJERCICIO 1: N = 5-------')
    ej1 = get_simulation_report(muestras = 5000, N = 5, T = 4)
    print(ej1)

    print('-------EJERCICIO 2: N = 10-------')
    ej2 = get_simulation_report(muestras = 5000, N = 10, T = 4)
    print(ej2)

    print('-------EJERCICIO 3: N = 30-------')
    ej3 = get_simulation_report(muestras = 5000, N = 30, T = 4)
    print(ej3)


    print('-------EJERCICIO 4: N = 100-------') 
    ej4 = get_simulation_report(muetras = 5000, N = 100, T = 4)
    print(ej4)


    print('-------EJERCICIO 1: N = 200-------')
    ej5 = get_simulation_report(muestras = 5000, N = 200, T = 4)
    print(ej5)

    print('-------EJERCICIO 1: N = 500-------')
    ej6 = get_simulation_report(muestras = 5000, N = 500, T = 4)
    print(ej6)
    

if __name__ == "__main__": 
    run()





