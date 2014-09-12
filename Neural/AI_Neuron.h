#ifndef AI_NEURON_H
#define AI_NEURON_H

#include "ML_Maths.h"

#include <string>

class aiSynapse;

class aiNeuron
{
public:
	aiNeuron();

	mlFloat		GetActivation(void);
	void		SetActivation(const mlFloat & activation);

	mlFloat		GetExternalStimulus(void);
	void		SetExternalStimulus(const mlFloat & stimulus);

	mlFloat		GetStimulus(void);
	void		SetStimulus(const mlFloat & stimulus);

	mlFloat		GetSigmoidBias(void);
	void		SetSigmoidBias(const mlFloat & sigmoidBias);

	mlFloat		GetError(void);
	void		SetError(const mlFloat & error);

	mlFloat		GetDesiredActivation(void);
	void		SetDesiredActivation(const mlFloat & activation);

	bool		IsInputNeuron(void);
	void		SetInputNeuronOption(const bool & option);

	bool		IsOutputNeuron(void);
	void		SetOutputNeuronOption(const bool & option);

	mlFloat		GetBiasAlpha(void);
	void		SetBiasAlpha(const mlFloat & biasAlpha);

	mlFloat		GetSigmoidK(void);
	void		SetSigmoidK(const mlFloat & sigmoidK);

	mlFloat		GetDamping(void);
	void		SetDamping(const mlFloat & dampingFactor);

	mlFloat		GetPreviousActivation();
	mlFloat		GetPreviousError();

	bool		IsUsingSystemVariables(void);
	void		SetUsingSystemVariablesOption(const bool & option);

	void		PreActivationCalculation(void);
	void		AccumulateStimulus(mlFloat stimulus);
	void		PostActivationCalculation(void);

	void		PreErrorCalculation(void);
	void		CalculateErrorFromDesired(void);
	void		AccumulateError(mlFloat error);
	void		PostErrorCalculation(void);

	void		ModifyBiasFromError(void);

private:

	mlFloat		m_activation;

	mlFloat		m_stimulus;

	mlFloat		m_externalStimulus;

	mlFloat		m_sigmoidBias;

	mlFloat		m_error;

	mlFloat		m_desiredActivation;

	mlFloat		m_frameError;

	bool		m_isInputNeuron;
	bool		m_isOutputNeuron;

	mlFloat		m_biasAlpha;

	mlFloat		m_sigmoidK;

	mlFloat		m_dampingFactor;

	bool		m_useSystemVariables;

	mlFloat		m_previousActivation;
	mlFloat		m_previousError;
};

inline mlFloat aiNeuron::GetActivation(void)
{
	return m_activation;
}

inline void aiNeuron::SetActivation(const mlFloat & activation)
{
	m_activation = activation;
}

inline mlFloat aiNeuron::GetStimulus(void)
{
	return m_stimulus;
}

inline void aiNeuron::SetStimulus(const mlFloat & stimulus)
{
	m_stimulus = stimulus;
}

inline mlFloat aiNeuron::GetExternalStimulus(void)
{
	return m_externalStimulus;
}

inline void aiNeuron::SetExternalStimulus(const mlFloat & stimulus)
{
	m_externalStimulus = stimulus;
}

inline mlFloat aiNeuron::GetSigmoidBias(void)
{
	return m_sigmoidBias;
}

inline void aiNeuron::SetSigmoidBias(const mlFloat & sigmoidBias)
{
	m_sigmoidBias = sigmoidBias;
}

inline mlFloat aiNeuron::GetError(void)
{
	return m_error;
}

inline void aiNeuron::SetError(const mlFloat & error)
{
	m_error = error;
}

inline mlFloat aiNeuron::GetDesiredActivation(void)
{
	return m_desiredActivation;
}

inline void aiNeuron::SetDesiredActivation(const mlFloat & activation)
{
	m_desiredActivation = activation;
}

inline bool aiNeuron::IsInputNeuron(void)
{
	return m_isInputNeuron;
}

inline void aiNeuron::SetInputNeuronOption(const bool & option)
{
	m_isInputNeuron = option;
}

inline bool aiNeuron::IsOutputNeuron(void)
{
	return m_isOutputNeuron;
}

inline void aiNeuron::SetOutputNeuronOption(const bool & option)
{
	m_isOutputNeuron = option;
}

inline mlFloat aiNeuron::GetBiasAlpha(void)
{
	return m_biasAlpha;
}

inline void aiNeuron::SetBiasAlpha(const mlFloat & biasAlpha)
{
	m_biasAlpha = biasAlpha;
}

inline mlFloat aiNeuron::GetSigmoidK(void)
{
	return m_sigmoidK;
}

inline void aiNeuron::SetSigmoidK(const mlFloat & sigmoidK)
{
	m_sigmoidK = sigmoidK;
}

inline mlFloat aiNeuron::GetDamping(void)
{
	return m_dampingFactor;
}

inline void aiNeuron::SetDamping(const mlFloat & dampingFactor)
{
	m_dampingFactor = dampingFactor;
}

inline bool aiNeuron::IsUsingSystemVariables(void)
{
	return m_useSystemVariables;
}

inline void aiNeuron::SetUsingSystemVariablesOption(const bool & option)
{
	m_useSystemVariables = option;
}

inline mlFloat aiNeuron::GetPreviousActivation()
{
	return m_previousActivation;
}

inline mlFloat aiNeuron::GetPreviousError()
{
	return m_previousError;
}

#endif // AI_NEURON_H
