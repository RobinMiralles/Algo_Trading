# -*- coding: utf-8 -*-
"""
NEURAL NETWORK : logistique problem y in {0,1}
"""


# --------------------------------------------------------------------------- #
# PACKAGES

import numpy as np
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
    return max(Z, 0)

def A_leakyReLU(Z):
    # leaky ReLU for alpha = 0.1
    return max(0.1*Z, Z)

def A_ELU(Z):
    # ELU for a = 0.5
    return np.where(Z >= 0, Z, 0.5 * (np.exp(Z) - 1))

def A_swish(Z):
    return Z / (1 + np.exp(-Z))

def initialisation(dimensions):
    
    parametres = {}
    C = len(dimensions)
    
    for c in range(1,C):
        parametres['W' + str(c)] = np.random.randn(dimensions[c],dimensions[c-1])
        parametres['b' + str(c)] = np.random.randn(dimensions[c],1)
    
    return parametres


def forward_propagation(X, parametres):
    
    activations = {'A0' : X}
    C = len(parametres) // 2
    
    for c in range(1, C + 1):
        Z = parametres['W' + str(c)].dot(activations['A' + str(c-1)]) + parametres['b' + str(c)]
        activations['A' + str(c)] = A_sigmoid(Z)
    
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


def predict_logistique(X, parametres):
    
  activations = forward_propagation(X, parametres)
  C = len(parametres) // 2
  AC = activations['A' + str(C)]
  y_pred = np.where(AC >= 0.5, 1, 0)
  
  return y_pred


def NN_logistique_train(X, y, hidden_layers, learning_rate, n_iter):
    
    np.random.seed(19021997)
    # initialisation de W et b 
    dimensions = list(hidden_layers)
    dimensions.insert(0, X.shape[0])
    dimensions.append(y.shape[0])
    parametres = initialisation(dimensions)
    
    train_loss = []
    train_acc = []
    
    for i in tqdm(range(n_iter)):
        
        activations = forward_propagation(X, parametres)
        gradients = back_propagation(y, activations, parametres)
        parametres = update(gradients, parametres, learning_rate)
        
        if i%10 ==0:
            C = len(parametres) // 2
            train_loss.append(log_loss(y, activations['A' + str(C)]))
            y_pred = predict_logistique(X, parametres)
            current_accuracy = accuracy_score(y.flatten(), y_pred.flatten())
            train_acc.append(current_accuracy)
            
    cf_matrix = confusion_matrix(y.flatten(), y_pred.flatten())

    plt.figure(figsize=(12, 4))
    plt.subplot(1, 2, 1)
    plt.plot(train_loss, label='train loss')
    plt.legend()
    plt.subplot(1, 2, 2)
    plt.plot(train_acc, label='train acc')
    plt.legend()
    plt.show()
    
    train = {'parametres' : parametres, 'accuracy' : current_accuracy, 'confusion_matrix' : cf_matrix, 'y_pred' : y_pred}
    
    return train
    

def NN_logistique_test(X, y, parametres_train):
    
    y_pred = predict_logistique(X, parametres_train)
    accuracy = accuracy_score(y.flatten(), y_pred.flatten())
    cf_matrix = confusion_matrix(y.flatten(), y_pred.flatten())
    
    test = {'accuracy' : accuracy, 'confusion_matrix' : cf_matrix, 'y_pred' : y_pred}
    
    return test



# --------------------------------------------------------------------------- #
# TRAIN & TEST


# make circles
X_train, y_train = make_circles(n_samples=100, noise=0.2, factor=0.3, random_state=0)
X_train = X_train.T
y_train  = y_train.reshape((1, y_train .shape[0]))
print('dimensions de X:', X_train .shape)
print('dimensions de y:', y_train .shape)
plt.scatter(X_train [0, :], X_train [1, :], c = y_train , cmap = 'summer')
plt.show()


train = NN_logistique_train(X_train , y_train , hidden_layers=(32, 32, 32), learning_rate = 0.1, n_iter = 1000)

X_test, y_test = make_circles(n_samples=100, noise=0.3, factor=0.3, random_state=0)
X_test = X_test.T
y_test = y_test.reshape((1, y_test.shape[0]))
plt.scatter(X_test[0, :], X_test[1, :], c = y_test, cmap = 'winter')
plt.show()

test = NN_logistique_test(X_test, y_test, train['parametres'])


sns.heatmap(train['confusion_matrix'], annot=True)
sns.heatmap(test['confusion_matrix'], annot=True)


# titanic data
titanic = sns.load_dataset('titanic')
titanic.head()
y = titanic['survived']
X = titanic.drop(['survived', 'alive'], axis = 1)

numerical_features = ['pclass', 'age', 'fare']
categorical_features = ['sex', 'deck', 'alone']
numerical_pipeline = make_pipeline(SimpleImputer(), StandardScaler())
categorical_pipeline = make_pipeline(SimpleImputer(strategy = "most_frequent"), OneHotEncoder())
preprocessor = make_column_transformer((numerical_pipeline, numerical_features),(categorical_pipeline, categorical_features))
X_prep = preprocessor.fit_transform(X)

X_train, X_test, y_train, y_test = train_test_split(X_prep, y, test_size=0.33, random_state=42)

X_train = X_train.T
y_train = np.array(y_train)
y_train  = y_train.reshape((1, y_train .shape[0]))
train = NN_logistique_train(X_train , y_train , hidden_layers=(32, 32, 32), learning_rate = 0.1, n_iter = 1000)
train['confusion_matrix']

X_test = X_test.T
y_test = np.array(y_test)
y_test  = y_test.reshape((1, y_test .shape[0]))
test = NN_logistique_test(X_test , y_test , train['parametres'])
test['confusion_matrix']

sns.heatmap(train['confusion_matrix'], annot=True)
sns.heatmap(test['confusion_matrix'], annot=True)






