#include "AI_NeuralNetwork.h"

#include "AI_NeuralNetworkSystem.h"

#include "AI_Neuron.h"
#include "AI_Synapse.h"

#include "UT_String.h"

aiNeuralNetwork::aiNeuralNetwork()
{
	ASSERT(aiNeuralNetworkSystem::GetInstance().IsRunning());
}

aiNeuralNetwork::~aiNeuralNetwork()
{
	{
		for(unsigned int neuronIndex = 0; neuronIndex < m_neurons.size(); neuronIndex++)
		{
			aiNeuron * neuron = m_neurons[neuronIndex];
			delete neuron;
		}
	}
	{
		for(unsigned int synapseIndex = 0; synapseIndex < m_synapses.size(); synapseIndex++)
		{
			aiSynapse * synapse = m_synapses[synapseIndex];
			delete synapse;
		}
	}
}

void aiNeuralNetwork::AddNeuron(aiNeuron * neuron)
{
	m_neurons.push_back(neuron);
}

void aiNeuralNetwork::AddNeurons(U32 n)
{
	for(U32 iNeuron = 0; iNeuron < n; iNeuron++)
		AddNeuron(new aiNeuron);
}

void aiNeuralNetwork::RemoveNeuron(unsigned int neuronIndex)
{
	aiNeuron * neuron = m_neurons[neuronIndex];

	for(SynapseVector::iterator synapseIterator = m_synapses.begin(); synapseIterator != m_synapses.end();)
	{
		aiSynapse * synapse = *synapseIterator;

		if(synapse->GetSourceNeuron() == neuron || synapse->GetTargetNeuron() == neuron)
		{
			delete synapse;
			synapseIterator = m_synapses.erase(synapseIterator);
		}
		else
		{
			synapseIterator++;
		}
	}

	delete m_neurons[neuronIndex];
	m_neurons.erase(m_neurons.begin() + neuronIndex);
}

void aiNeuralNetwork::RemoveNeuron(aiNeuron * neuron)
{
	int neuronIndex = GetNeuronID(neuron);

	if(neuronIndex != -1)
		RemoveNeuron(neuronIndex);
}

void aiNeuralNetwork::AddSynapse(aiSynapse * synapse)
{
	m_synapses.push_back(synapse);
}

void aiNeuralNetwork::RemoveSynapse(unsigned int synapseIndex)
{
	delete m_synapses[synapseIndex];
	m_synapses.erase(m_synapses.begin() + synapseIndex);
}

void aiNeuralNetwork::RemoveSynapse(aiSynapse * synapse)
{
	int synapseIndex = GetSynapseID(synapse);

	if(synapseIndex != -1)
		RemoveSynapse(synapseIndex);
}

int aiNeuralNetwork::GetNeuronID(aiNeuron * neuron)
{
	for(unsigned int neuronIndex = 0; neuronIndex < m_neurons.size(); neuronIndex++)
	{
		aiNeuron * neuronInList = m_neurons[neuronIndex];

		if(neuronInList == neuron)
			return neuronIndex;
	}

	return -1;
}

int aiNeuralNetwork::GetSynapseID(aiSynapse * synapse)
{
	for(unsigned int synapseIndex = 0; synapseIndex < m_synapses.size(); synapseIndex++)
	{
		aiSynapse * synapseInList = m_synapses[synapseIndex];

		if(synapseInList == synapse)
			return synapseIndex;
	}

	return -1;
}

void aiNeuralNetwork::Simulate(void)
{
	{
		for(unsigned int neuronIndex = 0; neuronIndex < m_neurons.size(); neuronIndex++)
		{
			aiNeuron * neuron = m_neurons[neuronIndex];

			neuron->PreActivationCalculation();
		}
	}

	{
		for(unsigned int synapseIndex = 0; synapseIndex < m_synapses.size(); synapseIndex++)
		{
			aiSynapse * synapse = m_synapses[synapseIndex];

			synapse->StimulateTargetNeuron();
		}
	}

	{
		for(unsigned int neuronIndex = 0; neuronIndex < m_neurons.size(); neuronIndex++)
		{
			aiNeuron * neuron = m_neurons[neuronIndex];

			neuron->PostActivationCalculation();
		}
	}
}

void aiNeuralNetwork::ErrorSimulate()
{
	{
		for(unsigned int neuronIndex = 0; neuronIndex < m_neurons.size(); neuronIndex++)
		{
			aiNeuron * neuron = m_neurons[neuronIndex];

			neuron->PreErrorCalculation();
		}
	}

	{
		for(unsigned int synapseIndex = 0; synapseIndex < m_synapses.size(); synapseIndex++)
		{
			aiSynapse * synapse = m_synapses[synapseIndex];

			if(synapse->GetTargetNeuron()->IsOutputNeuron())
				synapse->CalculateTargetNeuronErrorFromDesired();

			synapse->AccumulateSourceNeuronError();
		}
	}

	{
		for(unsigned int neuronIndex = 0; neuronIndex < m_neurons.size(); neuronIndex++)
		{
			aiNeuron * neuron = m_neurons[neuronIndex];

			neuron->PostErrorCalculation();
		}
	}
}

void aiNeuralNetwork::Train(void)
{
	{
		for(unsigned int synapseIndex = 0; synapseIndex < m_synapses.size(); synapseIndex++)
		{
			aiSynapse * synapse = m_synapses[synapseIndex];

			synapse->ModifyWeightFromError();
		}
	}

	{
		for(unsigned int neuronIndex = 0; neuronIndex < m_neurons.size(); neuronIndex++)
		{
			aiNeuron * neuron = m_neurons[neuronIndex];

			neuron->ModifyBiasFromError();
		}
	}
}

void aiNeuralNetwork::SimulateUntilStable()
{
	mlFloat tolerance = 0.001f;

	Simulate();

	while(CalculateActivationDifferenceTotal() > tolerance)
	{
		Simulate();
	}
}

void aiNeuralNetwork::ErrorSimulateUntilStable()
{
	mlFloat tolerance = 0.001f;

	ErrorSimulate();

	while(CalculateErrorDifferenceTotal() > tolerance)
	{
		ErrorSimulate();
	}
}

mlFloat aiNeuralNetwork::CalculateErrorTotal()
{
	mlFloat errorTotal = 0.0f;

	for(unsigned int neuronIndex = 0; neuronIndex < m_neurons.size(); neuronIndex++)
	{
		aiNeuron * neuron = m_neurons[neuronIndex];

		errorTotal += neuron->GetError();
	}

	return errorTotal;
}

mlFloat aiNeuralNetwork::CalculateActivationTotal()
{
	mlFloat activationTotal = 0.0f;

	for(unsigned int neuronIndex = 0; neuronIndex < m_neurons.size(); neuronIndex++)
	{
		aiNeuron * neuron = m_neurons[neuronIndex];

		activationTotal += neuron->GetActivation();
	}

	return activationTotal;
}

mlFloat aiNeuralNetwork::CalculateErrorDifferenceTotal()
{
	mlFloat errorTotal = 0.0f;

	for(unsigned int neuronIndex = 0; neuronIndex < m_neurons.size(); neuronIndex++)
	{
		aiNeuron * neuron = m_neurons[neuronIndex];

		errorTotal += mlFabs(neuron->GetError() - neuron->GetPreviousError());
	}

	return errorTotal;
}

mlFloat aiNeuralNetwork::CalculateActivationDifferenceTotal()
{
	mlFloat activationTotal = 0.0f;

	for(unsigned int neuronIndex = 0; neuronIndex < m_neurons.size(); neuronIndex++)
	{
		aiNeuron * neuron = m_neurons[neuronIndex];

		activationTotal += mlFabs(neuron->GetActivation() - neuron->GetPreviousActivation());
	}

	return activationTotal;
}

void aiNeuralNetwork::Serialise(istream & stream)
{
	utVerifyTokenInStream(stream, '\n', "NeuralNetwork");

	utVerifyTokenInStream(stream, ' ', "NumberOfNeurons");
	int nNeurons; stream >> nNeurons;

	utVerifyTokenInStream(stream, ' ', "NumberOfSynapses");
	int nSynapses; stream >> nSynapses;

	for(int iNeuron = 0; iNeuron < nNeurons; iNeuron++)
	{
		utVerifyTokenInStream(stream, '\n', "Neuron");

		aiNeuron * neuron = new aiNeuron;
		AddNeuron(neuron);

		utVerifyTokenInStream(stream, ' ', "NeuronType");
		std::string neuronTypeString = utGetTokenInStream(stream, 1024, '\n');

		if(neuronTypeString == "Input")
		{
			neuron->SetInputNeuronOption(true);
		}
		else if(neuronTypeString == "Output")
		{
			neuron->SetOutputNeuronOption(true);
		}
		else if(neuronTypeString == "Hidden")
		{
		}
		else
		{
            //TRACE("Error in neural network serialise, invalid neuron type %s\n", neuronTypeString.c_str());
            //ASSERT(false);
		}

		utVerifyTokenInStream(stream, '\n', "NeuronEnd");
	}

	for(int iSynapse = 0; iSynapse < nSynapses; iSynapse++)
	{
		utVerifyTokenInStream(stream, '\n', "Synapse");

		aiSynapse * synapse = new aiSynapse;
		AddSynapse(synapse);

		utVerifyTokenInStream(stream, ' ', "Weight");
		mlFloat weight; stream >> weight; synapse->SetWeight(weight);

		utVerifyTokenInStream(stream, ' ', "SourceNeuron");
		int sourceNeuronID; stream >> sourceNeuronID;
		synapse->SetSourceNeuron(GetNeuron(sourceNeuronID));

		utVerifyTokenInStream(stream, ' ', "TargetNeuron");
		int targetNeuronID; stream >> targetNeuronID;
		synapse->SetTargetNeuron(GetNeuron(targetNeuronID));

		utVerifyTokenInStream(stream, '\n', "SynapseEnd");
	}

	utVerifyTokenInStream(stream, '\n', "NeuralNetworkEnd");
}

void aiNeuralNetwork::Serialise(ostream & stream)
{
	stream << "NeuralNetwork\n";

	int nNeurons = GetNumberOfNeurons();
	stream << "NumberOfNeurons " << nNeurons << "\n";

	int nSynapses = GetNumberOfSynapses();
	stream << "NumberOfSynapses " << nSynapses << "\n";

	for(int iNeuron = 0; iNeuron < nNeurons; iNeuron++)
	{
		stream << "Neuron\n";

		aiNeuron * neuron = GetNeuron(iNeuron);

		stream << "NeuronType ";
		if(neuron->IsInputNeuron())
		{
			stream << "Input";
		}
		else if(neuron->IsOutputNeuron())
		{
			stream << "Output";
		}
		else
		{
			stream << "Hidden";
		}
		stream << "\n";

		stream << "NeuronEnd\n";
	}

	for(int iSynapse = 0; iSynapse < nSynapses; iSynapse++)
	{
		stream << "Synapse\n";

		aiSynapse * synapse = GetSynapse(iSynapse);

		stream << "Weight " << synapse->GetWeight() << "\n";
		stream << "SourceNeuron " << GetNeuronID(synapse->GetSourceNeuron()) << "\n";
		stream << "TargetNeuron " << GetNeuronID(synapse->GetTargetNeuron()) << "\n";

		stream << "SynapseEnd\n";
	}

	stream << "NeuralNetworkEnd\n";
}
