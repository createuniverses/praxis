#ifndef AI_NEURALNETWORK_H
#define AI_NEURALNETWORK_H

#include "ML_Types.h"

#include <vector>

#include <iostream>
using namespace std;

class aiNeuron;
class aiSynapse;

class aiNeuralNetwork
{
public:

			 aiNeuralNetwork();
	virtual	~aiNeuralNetwork();

	void Simulate(void);
	void ErrorSimulate(void);
	void Train(void);

	void SimulateUntilStable();
	void ErrorSimulateUntilStable();

	void				AddNeuron(aiNeuron * neuron);
	void				AddNeurons(U32 n);
	void				RemoveNeuron(unsigned int neuronIndex);
	void				RemoveNeuron(aiNeuron * neuron);

	void				AddSynapse(aiSynapse * synapse);
	void				RemoveSynapse(unsigned int synapseIndex);
	void				RemoveSynapse(aiSynapse * synapse);

	aiNeuron *			GetNeuron(unsigned int index);
	unsigned int		GetNumberOfNeurons(void);

	aiSynapse *			GetSynapse(unsigned int index);
	unsigned int		GetNumberOfSynapses(void);

	int					GetNeuronID(aiNeuron * neuron);
	int					GetSynapseID(aiSynapse * synapse);

	mlFloat				CalculateErrorTotal();
	mlFloat				CalculateActivationTotal();

	mlFloat				CalculateErrorDifferenceTotal();
	mlFloat				CalculateActivationDifferenceTotal();

	void				Serialise(istream & stream);
	void				Serialise(ostream & stream);

private:

	typedef std::vector<aiSynapse *> SynapseVector;
	typedef std::vector<aiNeuron *> NeuronVector;

	SynapseVector		m_synapses;
	NeuronVector		m_neurons;
};

inline aiNeuron * aiNeuralNetwork::GetNeuron(unsigned int index)
{
	return m_neurons[index];
}

inline unsigned int aiNeuralNetwork::GetNumberOfNeurons(void)
{
	return m_neurons.size();
}

inline aiSynapse * aiNeuralNetwork::GetSynapse(unsigned int index)
{
	return m_synapses[index];
}

inline unsigned int aiNeuralNetwork::GetNumberOfSynapses(void)
{
	return m_synapses.size();
}

#endif // AI_NEURALNETWORK_H
