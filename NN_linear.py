#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
NEURAL NETWORK : linear problem y in R
"""



# --------------------------------------------------------------------------- #
# PACKAGES

import numpy as np
import math as m
import pandas as pd
import matplotlib.pyplot as plt
from tqdm import tqdm
import seaborn as sns
from sklearn.datasets import make_circles
from sklearn.metrics import accuracy_score, log_loss, confusion_matrix
from sklearn.compose import make_column_transformer, make_column_selector
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.linear_model import SGDClassifier
from sklearn.impute import SimpleImputer
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error

plt.style.use('dark_background')



# --------------------------------------------------------------------------- #
# FUNCTIONS


# Regression : Linear Activation Function
# Binary Classification : Sigmoid/Logistic Activation Function
# Multiclass Classification : Softmax
# Multilabel Classification : Sigmoid

# Convolutional Neural Network (CNN): ReLU activation function.
# Recurrent Neural Network: Tanh and/or Sigmoid activation function

# ReLU activation function should only be used in the hidden layers.
# Sigmoid/Logistic and Tanh functions should not be used in hidden layers as they make the model more susceptible to problems during training (due to vanishing gradients).
# Swish function is used in neural networks having a depth greater than 40 layers.


def A_sigmoid(Z):
    return 1 / (1 + np.exp(-Z))

def A_tanh(Z):
    return (np.exp(Z) - np.exp(-Z)) / (np.exp(Z) + np.exp(-Z))

def A_ReLU(Z):
    return np.where(Z>0,Z,0)

def A_leakyReLU(Z):
    # leaky ReLU for alpha = 0.1
    alpha = 0.1
    return np.where(Z>alpha*Z,Z,alpha*Z)

def A_ELU(Z):
    # ELU for a = 0.5
    return np.where(Z >= 0, Z, 0.5 * (np.exp(Z) - 1))

def A_swish(Z):
    return Z / (1 + np.exp(-Z))

def A_linear(Z):
    return Z


def initialisation_Xavier(dimensions):
    # Xavier initializations : weight intitialization N(0,1/fan_avg), fan_avg =  (fan_in + fan_out) / 2
    parametres = {}
    C = len(dimensions)
    
    for c in range(1,C):
        fan_in =  dimensions[c-1]        # number of input in the layer
        fan_out = dimensions[c]          # number of neurons in the layer
        fan_avg = (fan_in + fan_out)/2
        parametres['W' + str(c)] = np.random.randn(dimensions[c],dimensions[c-1]) * m.sqrt(1/fan_avg)
        parametres['b' + str(c)] = np.random.randn(dimensions[c],1)*m.sqrt(1/fan_avg) * m.sqrt(1/fan_avg)
    
    return parametres


def forward_propagation(X, parametres):
    
    activations = {'A0' : X}
    C = len(parametres) // 2
    
    for c in range(1, C + 1):
        Z = parametres['W' + str(c)].dot(activations['A' + str(c-1)]) + parametres['b' + str(c)]
        activations['A' + str(c)] = A_ReLU(Z)  # activation function : to choose
    
    return activations
  
  
def back_propagation(y , activations, parametres):
    
    m = y.shape[1]
    C = len(parametres) // 2
    
    dZ = activations['A' + str(C)] - y
    gradients = {}
    
    for c in reversed(range(1,C + 1)):
        gradients['dW' + str(c)] = 1/m * np.dot(dZ, activations['A' + str(c-1)].T)
        gradients['db' + str(c)] = 1/m * np.sum(dZ, axis = 1, keepdims=True)
        if c > 1:
            dZ = np.dot(parametres['W' + str(c)].T, dZ) * activations['A' + str(c-1)] * (1 - activations['A' + str(c - 1)])
    
    return(gradients)


def update(gradients, parametres, learning_rate):
    
    C = len(parametres) // 2
    
    for c in range(1, C + 1):
        parametres['W' + str(c)] = parametres['W' + str(c)] - learning_rate * gradients['dW' + str(c)]
        parametres['b' + str(c)] = parametres['b' + str(c)] - learning_rate * gradients['db' + str(c)]

    return parametres


def predict_linear(X, parametres):
    
  activations = forward_propagation(X, parametres)
  C = len(parametres) // 2
  AC = activations['A' + str(C)]
  y_pred = AC
  
  return y_pred


def NN_linear_train(X, y, hidden_layers, learning_rate, n_iter):
    
    np.random.seed(19021997)
    # initialisation de W et b 
    dimensions = list(hidden_layers)
    dimensions.insert(0, X.shape[0])
    dimensions.append(y.shape[0])
    parametres = initialisation_Xavier(dimensions)
    
    train_RMSE = []
    
    for i in tqdm(range(n_iter)):
        
        activations = forward_propagation(X, parametres)
        gradients = back_propagation(y, activations, parametres)
        parametres = update(gradients, parametres, learning_rate)
        
        if i%10 ==0:
            y_pred = predict_linear(X, parametres)
            current_RMSE =  mean_squared_error(y, y_pred, squared=False)
            train_RMSE.append(current_RMSE)
            

    plt.figure(figsize=(12, 4))
    plt.plot(train_RMSE, label='train RMSE')
    plt.legend()
    plt.show()
    
    train = {'parametres' : parametres, 'RMSE' : current_RMSE, 'y_pred' : y_pred}
    
    return train
    

def NN_linear_test(X, y, parametres_train):
    
    y_pred = predict_linear(X, parametres_train)
    RMSE =  mean_squared_error(y, y_pred, squared=False)
    
    test = {'RMSE' : RMSE, 'y_pred' : y_pred}
    
    return test


def data_preprocessing_NNlinear(data, name_target, normalize_target, numerical_features, categorical_features, test_size):
    data = data.dropna()
    data= data[data[name_target].notna()]
    y = data[name_target]
    X = data.drop([name_target], axis = 1)

    numerical_pipeline = make_pipeline(SimpleImputer(), StandardScaler())
    categorical_pipeline = make_pipeline(SimpleImputer(strategy = "most_frequent"), OneHotEncoder())
    preprocessor = make_column_transformer((numerical_pipeline, numerical_features),(categorical_pipeline, categorical_features))
    X_prep = preprocessor.fit_transform(X)
    mu_y = y.mean()
    sigma_y = y.std()
    y_prep = y
    if normalize_target is True:
        y_prep = (y - mu_y)/sigma_y
    
    X_train, X_test, y_train, y_test = train_test_split(X_prep, y_prep, test_size=test_size, random_state=42)

    X_train = X_train.T
    y_train = np.array(y_train)
    y_train  = y_train.reshape((1, y_train .shape[0]))
   
    X_test = X_test.T
    y_test = np.array(y_test)
    y_test  = y_test.reshape((1, y_test .shape[0]))
    
    if isinstance(X_train, np.ndarray) == False :
        X_train.toarray()
        X_test.toarray()
    
    return {'X_train' : X_train.toarray(), 'y_train' : y_train, 'X_test' : X_test.toarray(), 'y_test' : y_test, 'mu_y' : mu_y, 'sigma_y' : sigma_y}
  
# --------------------------------------------------------------------------- #
# TRAIN & TEST

sns.get_dataset_names()
sns.load_dataset('flights')


###############
# titanic data
titanic = sns.load_dataset('titanic')
titanic.describe().round(3)
titanic.head()
data = data_preprocessing_NNlinear(titanic, 'age', True, ['pclass', 'fare'], ['sex', 'alone','survived'], 0.33)

train = NN_linear_train(data['X_train'] , data['y_train'] , hidden_layers=(32, 32), learning_rate = 0.01, n_iter = 1000)
train['RMSE']
(data['y_train']*data['sigma_y']+data['mu_y']) - (train['y_pred']*data['sigma_y']+data['mu_y'])

test = NN_linear_test(data['X_test'] , data['y_test'] , train['parametres'])
test['RMSE']
(data['y_test']*data['sigma_y']+data['mu_y']) - (test['y_pred']*data['sigma_y']+data['mu_y'])


################
# diamonds data
flights = sns.load_dataset('flights')
flights.head()
flights.describe().round(3)
data = data_preprocessing_NNlinear(flights, 'passengers', False, ['year'], ['month'], 0.33)

train = NN_linear_train(data['X_train'] , data['y_train'] , hidden_layers=(32, 32, 32, 32), learning_rate = 0.01, n_iter = 1000)
train['RMSE']
(data['y_train']) - (train['y_pred'])

test = NN_linear_test(data['X_test'] , data['y_test'] , train['parametres'])
test['RMSE']
(data['y_test']*data['sigma_y']+data['mu_y']) - (test['y_pred']*data['sigma_y']+data['mu_y'])



#################
X = data['X_train']
y = data['y_train']
hidden_layers = (16, 16)
learning_rate = 0.1
n_iter = 1000



# debuguer data_preprocessing_NNlinear
# NN_linear faux predit tres mal --> probleme dans les fonctions ?






