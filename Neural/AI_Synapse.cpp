#include "AI_Synapse.h"

#include "AI_Neuron.h"

#include "AI_NeuralNetworkSystem.h"

aiSynapse::aiSynapse()
{
	m_weight		= 0.0f;
	m_sourceNeuron	= NULL;
	m_targetNeuron	= NULL;

	m_alpha			= aiNeuralNetworkSystem::GetInstance().GetAlpha();
	m_momentum		= aiNeuralNetworkSystem::GetInstance().GetMomentum();
	m_decayRate		= aiNeuralNetworkSystem::GetInstance().GetDecayRate();

	m_useSystemVariables = true;
}

aiSynapse::aiSynapse(mlFloat weight, aiNeuron * sourceNeuron, aiNeuron * targetNeuron)
{
	ASSERT(sourceNeuron);
	ASSERT(targetNeuron);

	m_weight		= weight;
	m_sourceNeuron	= sourceNeuron;
	m_targetNeuron	= targetNeuron;

	m_deltaWeight	= 0.0f;

	m_alpha			= aiNeuralNetworkSystem::GetInstance().GetAlpha();
	m_momentum		= aiNeuralNetworkSystem::GetInstance().GetMomentum();
	m_decayRate		= aiNeuralNetworkSystem::GetInstance().GetDecayRate();

	m_useSystemVariables = true;
}

void aiSynapse::StimulateTargetNeuron(void)
{
	if(m_targetNeuron)
		m_targetNeuron->AccumulateStimulus(m_weight * m_sourceNeuron->GetActivation());
}

void aiSynapse::CalculateTargetNeuronErrorFromDesired(void)
{
	if(m_targetNeuron)
		m_targetNeuron->CalculateErrorFromDesired();
}

void aiSynapse::AccumulateSourceNeuronError(void)
{
	if(m_sourceNeuron && m_targetNeuron)
		m_sourceNeuron->AccumulateError(m_weight * m_targetNeuron->GetError());
}

void aiSynapse::ModifyWeightFromError(void)
{
	if(m_useSystemVariables)
	{
		m_alpha			= aiNeuralNetworkSystem::GetInstance().GetAlpha();
		m_momentum		= aiNeuralNetworkSystem::GetInstance().GetMomentum();
		m_decayRate		= aiNeuralNetworkSystem::GetInstance().GetDecayRate();
	}

	m_deltaWeight =
		m_alpha * m_targetNeuron->GetError() * m_sourceNeuron->GetActivation() +
		m_momentum * m_deltaWeight +
		-m_decayRate * m_weight;

	m_weight += m_deltaWeight;

	m_weight = mlClamp(m_weight, -30.0f, 30.0f);
}
