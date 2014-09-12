#include "AI_Neuron.h"

#include "AI_Synapse.h"

#include "UT_Functions.h"

#include "AI_NeuralNetworkSystem.h"

aiNeuron::aiNeuron()
{
	m_activation						= 0.0f;

	m_stimulus							= 0.0f;

	m_externalStimulus					= 0.0f;

	m_sigmoidBias						= 0.0f;

	m_error								= 0.0f;

	m_desiredActivation					= 0.0f;

	m_frameError						= 0.0f;

	m_isInputNeuron						= false;
	m_isOutputNeuron					= false;

	m_useSystemVariables				= true;

	m_biasAlpha							= aiNeuralNetworkSystem::GetInstance().GetBiasAlpha();
	m_sigmoidK							= aiNeuralNetworkSystem::GetInstance().GetSigmoidK();
	m_dampingFactor						= aiNeuralNetworkSystem::GetInstance().GetDamping();

	m_previousActivation	= 0.0f;
	m_previousError			= 0.0f;
}

void aiNeuron::PreActivationCalculation(void)
{
	m_stimulus = 0.0f;
}

void aiNeuron::AccumulateStimulus(mlFloat stimulus)
{
	m_stimulus += stimulus;
}

void aiNeuron::PostActivationCalculation(void)
{
	if(m_useSystemVariables)
	{
		m_dampingFactor	= aiNeuralNetworkSystem::GetInstance().GetDamping();
		m_sigmoidK		= aiNeuralNetworkSystem::GetInstance().GetSigmoidK();
	}

	m_stimulus += m_externalStimulus;

	mlFloat newActivation = utSigmoid(m_stimulus, m_sigmoidK, m_sigmoidBias);

	m_previousActivation = m_activation;
	m_activation = m_dampingFactor * (newActivation - m_activation) + m_activation;
}

void aiNeuron::PreErrorCalculation(void)
{
	m_frameError = 0.0f;
}

void aiNeuron::CalculateErrorFromDesired(void)
{
	if(m_useSystemVariables)
	{
		m_sigmoidK	= aiNeuralNetworkSystem::GetInstance().GetSigmoidK();
	}

	m_frameError = (m_desiredActivation - m_activation) * utSigmoidDerivative(mlClamp(m_stimulus, -0.5f, 0.5f), m_sigmoidK, m_sigmoidBias);
}

void aiNeuron::AccumulateError(mlFloat error)
{
	m_frameError += error;
}

void aiNeuron::PostErrorCalculation(void)
{
	if(m_useSystemVariables)
	{
		m_sigmoidK	= aiNeuralNetworkSystem::GetInstance().GetSigmoidK();
	}

	m_frameError *= utSigmoidDerivative(m_stimulus, m_sigmoidK, m_sigmoidBias);

	m_previousError = m_error;
	m_error = m_frameError;
}

void aiNeuron::ModifyBiasFromError(void)
{
	if(m_useSystemVariables)
	{
		m_biasAlpha	= aiNeuralNetworkSystem::GetInstance().GetBiasAlpha();
	}

	m_sigmoidBias -= m_biasAlpha * m_error;

	m_sigmoidBias = mlClamp(m_sigmoidBias, -30.0f, 30.0f);
}
