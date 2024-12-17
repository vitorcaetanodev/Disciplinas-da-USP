import matplotlib.pyplot as plt

# Dados dos exemplares
exemplares = [
    [1.0, 1.0], [0.8, 1.2], [1.1, 1.5], [0.9, 1.6], [1.4, 1.1], [1.5, 0.7],
    [1.0, 3.0], [0.8, 3.2], [0.7, 3.5], [1.0, 3.4], [1.1, 3.2], [3.0, 2.8],
    [3.2, 2.9], [3.5, 2.7], [3.2, 2.6], [1.5, 1.3], [1.8, 0.9], [1.7, 1.2]
]

# Pesos iniciais dos neurônios
pesos_iniciais = {
    'C1': [0.8, 3.4],
    'C2': [3.0, 2.5],
    'C3': [1.2, 1.3],
    'C4': [1.5, 0.8]
}

# Pesos atualizados dos neurônios C3 e C4
pesos_atualizados = {
    'C3': [1.0, 1.0],
    'C4': [1.19675, 0.9213]
}

# Plotagem dos exemplares
plt.scatter(*zip(*exemplares), color='grey', label='Exemplares')

# Plotagem dos pesos iniciais dos neurônios
for label, peso in pesos_iniciais.items():
    plt.scatter(*peso, label=f'Inicial {label}', marker='o')

# Plotagem dos pesos atualizados dos neurônios
for label, peso in pesos_atualizados.items():
    plt.scatter(*peso, label=f'Atualizado {label}', marker='x')

# Configurações do gráfico
plt.xlabel('Termo A')
plt.ylabel('Termo B')
plt.legend()
plt.title('Gráfico de Dispersão dos Exemplares e Pesos dos Neurônios')
plt.grid(True)
plt.show()