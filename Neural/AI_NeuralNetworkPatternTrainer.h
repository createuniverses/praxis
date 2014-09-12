#ifndef AI_NEURALNETWORKPATTERNTRAINER_H
#define AI_NEURALNETWORKPATTERNTRAINER_H

#include "ML_Types.h"

#include <vector>

#include <iostream>
using namespace std;
using namespace std;

class aiNeuralNetwork;
class aiNeuron;

class aiNeuralNetworkPatternTrainer
{
public:

	aiNeuralNetworkPatternTrainer();
	aiNeuralNetworkPatternTrainer(aiNeuralNetwork * network);

	void SetNetwork(aiNeuralNetwork * network) { m_network = network; }

	struct TrainingResult
	{
		TrainingResult(U32 iterations, mlFloat error) : m_nIterations(iterations), m_errorAchieved(error) {}

		U32		m_nIterations;
		mlFloat	m_errorAchieved;
	};

	typedef std::vector<mlFloat> FloatVector;

	struct PatternPair
	{
		FloatVector	m_input;
		FloatVector	m_output;
	};

	TrainingResult TrainNetworkToTolerance(mlFloat tolerance, U32 nMaximumIterations);
	TrainingResult TrainNetworkForEpochs(U32 nEpochs);

	void PrepareAndTrainOnPair(U32 index);
	void SimulateOnPair(U32 index);

	void			Reset();

	void			SetNumberOfPairs(U32 size);
	void			SetNumberOfPairs(U32 size, U32 nInputs, U32 nOutputs);
	void			RemovePair(U32 index);
	PatternPair &	GetPair(U32 index);
	U32				GetNumberOfPairs();

	void			RandomizeWeights();

	U32				GetNumberOfInputNeurons() { return m_inputNeurons.size(); }
	U32				GetNumberOfOutputNeurons() { return m_outputNeurons.size(); }

	void			Serialise(istream & stream);
	void			Serialise(ostream & stream);

private:

	typedef std::vector<aiNeuron *> NeuronSet;

	void TrainOnPair(U32 iPattern);

	void GatherInputNeurons();
	void GatherOutputNeurons();

	typedef std::vector<PatternPair> InputOutputSet;

	InputOutputSet m_pattern;

	aiNeuralNetwork * m_network;

	NeuronSet	m_inputNeurons;
	NeuronSet	m_outputNeurons;

	U32			m_nPairIterations;

	mlFloat CalculatePatternError();
};

extern const aiNeuralNetworkPatternTrainer::TrainingResult aiTrainingResultBad;

#endif // AI_NEURALNETWORKPATTERNTRAINER_H
