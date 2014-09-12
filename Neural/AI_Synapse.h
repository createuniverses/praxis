#ifndef AI_SYNAPSE_H
#define AI_SYNAPSE_H

#include "ML_Maths.h"

class aiNeuron;

class aiSynapse
{
public:
	aiSynapse();
	aiSynapse(mlFloat weight, aiNeuron * sourceNeuron, aiNeuron * targetNeuron);

	mlFloat		GetWeight(void);
	void		SetWeight(const mlFloat & weight);
	
	aiNeuron *	GetSourceNeuron(void);
	aiNeuron *	GetTargetNeuron(void);
	void		SetSourceNeuron(aiNeuron * neuron);
	void		SetTargetNeuron(aiNeuron * neuron);

	mlFloat		GetAlpha(void);
	void		SetAlpha(const mlFloat & alpha);

	mlFloat		GetMomentum(void);
	void		SetMomentum(const mlFloat & momentum);

	mlFloat		GetDecayRate(void);
	void		SetDecayRate(const mlFloat & decayRate);

	bool		IsUsingSystemVariables(void);
	void		SetUsingSystemVariablesOption(const bool & option);

	void StimulateTargetNeuron(void);

	void CalculateTargetNeuronErrorFromDesired(void);
	void AccumulateSourceNeuronError(void);
	void ModifyWeightFromError(void);

private:

	mlFloat		m_weight;

	aiNeuron *	m_sourceNeuron;
	aiNeuron *	m_targetNeuron;

	mlFloat		m_deltaWeight;

	mlFloat		m_alpha;
	mlFloat		m_momentum;
	mlFloat		m_decayRate;

	bool		m_useSystemVariables;
};

inline mlFloat aiSynapse::GetWeight(void)
{
	return m_weight;
}

inline void aiSynapse::SetWeight(const mlFloat & weight)
{
	m_weight = weight;
}

inline aiNeuron * aiSynapse::GetSourceNeuron(void)
{
	return m_sourceNeuron;
}

inline aiNeuron * aiSynapse::GetTargetNeuron(void)
{
	return m_targetNeuron;
}

inline void aiSynapse::SetSourceNeuron(aiNeuron * neuron)
{
	m_sourceNeuron = neuron;
}

inline void aiSynapse::SetTargetNeuron(aiNeuron * neuron)
{
	m_targetNeuron = neuron;
}

inline mlFloat aiSynapse::GetAlpha(void)
{
	return m_alpha;
}

inline void aiSynapse::SetAlpha(const mlFloat & alpha)
{
	m_alpha = alpha;
}

inline mlFloat aiSynapse::GetMomentum(void)
{
	return m_momentum;
}

inline void aiSynapse::SetMomentum(const mlFloat & momentum)
{
	m_momentum = momentum;
}

inline mlFloat aiSynapse::GetDecayRate(void)
{
	return m_decayRate;
}

inline void aiSynapse::SetDecayRate(const mlFloat & decayRate)
{
	m_decayRate = decayRate;
}

inline bool aiSynapse::IsUsingSystemVariables(void)
{
	return m_useSystemVariables;
}

inline void aiSynapse::SetUsingSystemVariablesOption(const bool & option)
{
	m_useSystemVariables = option;
}

#endif // AI_SYNAPSE_H
