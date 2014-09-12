#include "AI_NeuralNetworkPatternTrainer.h"

#include "AI_NeuralNetwork.h"
#include "AI_Neuron.h"
#include "AI_Synapse.h"

#include "UT_Functions.h"
#include "UT_String.h"

const aiNeuralNetworkPatternTrainer::TrainingResult aiTrainingResultBad = aiNeuralNetworkPatternTrainer::TrainingResult(-1, 0.0f);

aiNeuralNetworkPatternTrainer::aiNeuralNetworkPatternTrainer()
{
	m_network			= NULL;
	m_nPairIterations	= 1;
}

aiNeuralNetworkPatternTrainer::aiNeuralNetworkPatternTrainer(aiNeuralNetwork * network)
{
	m_network			= network;
	m_nPairIterations	= 1;
}

aiNeuralNetworkPatternTrainer::TrainingResult
aiNeuralNetworkPatternTrainer::TrainNetworkToTolerance(mlFloat tolerance, U32 nMaximumIterations)
{
	if(!m_network)
		return aiTrainingResultBad;

	GatherInputNeurons();
	GatherOutputNeurons();

	mlFloat error = 0.0f;
	U32 nIterations = 0;

	for(U32 pairIndex = 0; pairIndex < m_pattern.size(); pairIndex++)
	{
		TrainOnPair(pairIndex);
		error += CalculatePatternError();
	}

	error /= m_outputNeurons.size() * m_pattern.size();

	nIterations++;

	while(mlFabs(error) > tolerance && nIterations < nMaximumIterations)
	{
		error = 0.0f;

		for(U32 pairIndex = 0; pairIndex < m_pattern.size(); pairIndex++)
		{
			TrainOnPair(pairIndex);

			error += CalculatePatternError();
		}

		error /= m_outputNeurons.size() * m_pattern.size();

		nIterations++;
	}

	return TrainingResult(nIterations, error);
}

aiNeuralNetworkPatternTrainer::TrainingResult
aiNeuralNetworkPatternTrainer::TrainNetworkForEpochs(U32 nEpochs)
{
	if(!m_network)
		return aiTrainingResultBad;

	GatherInputNeurons();
	GatherOutputNeurons();

	mlFloat error = 0.0f;

	for(U32 epochIndex = 0; epochIndex < nEpochs; epochIndex++)
	{
		error = 0.0f;
		for(U32 pairIndex = 0; pairIndex < m_pattern.size(); pairIndex++)
		{
			TrainOnPair(pairIndex);

			error += CalculatePatternError();
		}

		error /= m_outputNeurons.size() * m_pattern.size();		
	}

	return TrainingResult(nEpochs, error);
}

void aiNeuralNetworkPatternTrainer::PrepareAndTrainOnPair(U32 index)
{
	if(!m_network)
		return;

	GatherInputNeurons();
	GatherOutputNeurons();

	TrainOnPair(index);
}

void aiNeuralNetworkPatternTrainer::TrainOnPair(U32 iPattern)
{
	PatternPair currentPair = m_pattern[iPattern];

	for(U32 inputVectorIndex = 0; inputVectorIndex < currentPair.m_input.size() && inputVectorIndex < m_inputNeurons.size(); inputVectorIndex++)
	{
		aiNeuron * neuron = m_inputNeurons[inputVectorIndex];

		neuron->SetExternalStimulus(currentPair.m_input[inputVectorIndex]);
	}
	for(U32 outputVectorIndex = 0; outputVectorIndex < currentPair.m_output.size() && outputVectorIndex < m_outputNeurons.size(); outputVectorIndex++)
	{
		aiNeuron * neuron = m_outputNeurons[outputVectorIndex];

		neuron->SetDesiredActivation(currentPair.m_output[outputVectorIndex]);
	}

	for(U32 pairIteration = 0; pairIteration < m_nPairIterations; pairIteration++)
	{
		m_network->SimulateUntilStable();
		m_network->ErrorSimulateUntilStable();
		m_network->Train();
	}
}

void aiNeuralNetworkPatternTrainer::GatherInputNeurons()
{
	m_inputNeurons.clear();

	if(!m_network)
		return;

	for(U32 neuronIndex = 0; neuronIndex < m_network->GetNumberOfNeurons(); neuronIndex++)
	{
		aiNeuron * neuron = m_network->GetNeuron(neuronIndex);

		if(neuron->IsInputNeuron())
			m_inputNeurons.push_back(neuron);
	}
}

void aiNeuralNetworkPatternTrainer::GatherOutputNeurons()
{
	m_outputNeurons.clear();

	if(!m_network)
		return;

	for(U32 neuronIndex = 0; neuronIndex < m_network->GetNumberOfNeurons(); neuronIndex++)
	{
		aiNeuron * neuron = m_network->GetNeuron(neuronIndex);

		if(neuron->IsOutputNeuron())
			m_outputNeurons.push_back(neuron);
	}
}

mlFloat aiNeuralNetworkPatternTrainer::CalculatePatternError()
{
	mlFloat error = 0;

	for(U32 outputNeuronIndex = 0; outputNeuronIndex < m_outputNeurons.size(); outputNeuronIndex++)
	{
		aiNeuron * neuron = m_outputNeurons[outputNeuronIndex];

		error += mlFabs(neuron->GetDesiredActivation() - neuron->GetActivation());
	}

	return error;
}

void aiNeuralNetworkPatternTrainer::SetNumberOfPairs(U32 size)
{
	GatherInputNeurons();
	GatherOutputNeurons();

	SetNumberOfPairs(size, GetNumberOfInputNeurons(), GetNumberOfOutputNeurons());
}

void aiNeuralNetworkPatternTrainer::SetNumberOfPairs(U32 size, U32 nInputs, U32 nOutputs)
{
	m_pattern.resize(size);

	for(U32 iPair = 0; iPair < m_pattern.size(); iPair++)
	{
		m_pattern[iPair].m_input.resize(nInputs);
		m_pattern[iPair].m_output.resize(nOutputs);
	}
}

void aiNeuralNetworkPatternTrainer::Reset()
{
	SetNumberOfPairs(GetNumberOfPairs());
}

void aiNeuralNetworkPatternTrainer::RemovePair(U32 index)
{
	m_pattern.erase(m_pattern.begin() + index);
}

aiNeuralNetworkPatternTrainer::PatternPair &
aiNeuralNetworkPatternTrainer::GetPair(U32 index)
{
	return m_pattern[index];
}

U32 aiNeuralNetworkPatternTrainer::GetNumberOfPairs()
{
	return m_pattern.size();
}

void aiNeuralNetworkPatternTrainer::RandomizeWeights()
{
	for(U32 iSynapse = 0; iSynapse < m_network->GetNumberOfSynapses(); iSynapse++)
	{
		aiSynapse * synapse = m_network->GetSynapse(iSynapse);

		synapse->SetWeight(utRand(-1.0f, 1.0f, 100));
	}
}

void aiNeuralNetworkPatternTrainer::SimulateOnPair(U32 index)
{
	if(!m_network)
		return;

	GatherInputNeurons();
	GatherOutputNeurons();

	PatternPair currentPair = m_pattern[index];

	for(U32 inputVectorIndex = 0; inputVectorIndex < currentPair.m_input.size() && inputVectorIndex < m_inputNeurons.size(); inputVectorIndex++)
	{
		aiNeuron * neuron = m_inputNeurons[inputVectorIndex];

		neuron->SetExternalStimulus(currentPair.m_input[inputVectorIndex]);
	}
	for(U32 outputVectorIndex = 0; outputVectorIndex < currentPair.m_output.size() && outputVectorIndex < m_outputNeurons.size(); outputVectorIndex++)
	{
		aiNeuron * neuron = m_outputNeurons[outputVectorIndex];

		neuron->SetDesiredActivation(currentPair.m_output[outputVectorIndex]);
	}

	for(U32 pairIteration = 0; pairIteration < m_nPairIterations; pairIteration++)
	{
		m_network->SimulateUntilStable();
		m_network->ErrorSimulateUntilStable();
	}
}


void aiNeuralNetworkPatternTrainer::Serialise(istream & stream)
{
	utVerifyTokenInStream(stream, '\n', "NeuralNetworkPattern");

	utVerifyTokenInStream(stream, ' ', "NumberOfPairs");
	int nPairs; stream >> nPairs;
	utVerifyTokenInStream(stream, ' ', "NumberOfInputs");
	int nInputs; stream >> nInputs;
	utVerifyTokenInStream(stream, ' ', "NumberOfOutputs");
	int nOutputs; stream >> nOutputs;

	SetNumberOfPairs(nPairs, nInputs, nOutputs);

	for(int iPair = 0; iPair < nPairs; iPair++)
	{
		PatternPair & pair = m_pattern[iPair];

		utVerifyTokenInStream(stream, '\n', "Pair");

		utVerifyTokenInStream(stream, ' ', "Inputs");
		for(int iInput = 0; iInput < nInputs; iInput++)
		{
			stream >> pair.m_input[iInput];
		}
		utVerifyTokenInStream(stream, ' ', "Outputs");
		for(int iOutput = 0; iOutput < nOutputs; iOutput++)
		{
			stream >> pair.m_output[iOutput];
		}

		utVerifyTokenInStream(stream, '\n', "PairEnd");
	}

	utVerifyTokenInStream(stream, '\n', "NeuralNetworkPatternEnd");
}

void aiNeuralNetworkPatternTrainer::Serialise(ostream & stream)
{
	stream << "NeuralNetworkPattern\n";

	int nPairs = m_pattern.size();
	stream << "NumberOfPairs " << nPairs << "\n";

	int nInputs = 0;
	int nOutputs = 0;

	if(nPairs > 0)
	{
		nInputs = m_pattern[0].m_input.size();
		nOutputs = m_pattern[0].m_output.size();
	}

	stream << "NumberOfInputs " << nInputs << "\n";
	stream << "NumberOfOutputs " << nOutputs << "\n";

	for(int iPair = 0; iPair < nPairs; iPair++)
	{
		PatternPair & pair = m_pattern[iPair];

		stream << "Pair\n";

		stream << "Inputs ";
		for(int iInput = 0; iInput < nInputs; iInput++)
		{
			stream << pair.m_input[iInput] << " ";
		}
		stream << "\n";
		stream << "Outputs ";
		for(int iOutput = 0; iOutput < nOutputs; iOutput++)
		{
			stream << pair.m_output[iOutput] << " ";
		}
		stream << "\n";

		stream << "PairEnd\n";
	}

	stream << "NeuralNetworkPatternEnd\n";
}
