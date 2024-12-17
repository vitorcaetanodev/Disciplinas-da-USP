import numpy as np
import matplotlib.pyplot as plt

# Definir a função sigmoide e sua derivada (inclinação)
def sigmoid(x, k):
    return 1 / (1 + np.exp(-k * x))

def sigmoid_derivative(x, k):
    sig = sigmoid(x, k)
    return sig * (1 - sig) * k

# Valores de x para plotar
x = np.linspace(-10, 10, 400)

# Parâmetro de inclinação
k = 3.0

# Calcular sigmoide e sua derivada
y_sigmoid = sigmoid(x, k)
y_sigmoid_natural = sigmoid(x, 1)
y_derivative = sigmoid_derivative(x, k)

# Plotar a sigmoide
plt.figure(figsize=(10, 6))

plt.plot(x, y_sigmoid_natural, label='Sigmoide Natural', color='green')
plt.plot(x, y_sigmoid, label='Sigmoide com inclinacao', color='blue')

# Plotar a derivada da sigmoide (inclinação)
plt.plot(x, y_derivative, label='Inclinação da Sigmoide', color='red', linestyle='--')

# Adicionar legendas e títulos
plt.title('Sigmoide e sua Inclinação com k=3.0')
plt.xlabel('x')
plt.ylabel('Valor')
plt.legend()

# Mostrar o gráfico
plt.grid(True)
plt.show()