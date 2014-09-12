#ifndef AI_NEURALNETWORKSYSTEM_H
#define AI_NEURALNETWORKSYSTEM_H

#include "ML_Maths.h"

class aiNeuralNetworkSystem
{
public:

	static aiNeuralNetworkSystem & GetInstance();

	static void Startup();
	static void Shutdown();

	bool IsRunning();

	mlFloat		GetAlpha(void);
	void		SetAlpha(const mlFloat & alpha);

	mlFloat		GetBiasAlpha(void);
	void		SetBiasAlpha(const mlFloat & biasAlpha);

	mlFloat		GetMomentum(void);
	void		SetMomentum(const mlFloat & momentum);

	mlFloat		GetDecayRate(void);
	void		SetDecayRate(const mlFloat & decayRate);

	mlFloat		GetSigmoidK(void);
	void		SetSigmoidK(const mlFloat & sigmoidK);

	mlFloat		GetDamping(void);
	void		SetDamping(const mlFloat & dampingFactor);

private:

	mlFloat		m_alpha;
	mlFloat		m_biasAlpha;
	mlFloat		m_momentum;
	mlFloat		m_decayRate;
	mlFloat		m_sigmoidK;
	mlFloat		m_dampingFactor;

			 aiNeuralNetworkSystem();
	virtual	~aiNeuralNetworkSystem();

	static aiNeuralNetworkSystem * s_instance;
};

inline mlFloat aiNeuralNetworkSystem::GetAlpha(void)
{
	return m_alpha;
}

inline void aiNeuralNetworkSystem::SetAlpha(const mlFloat & alpha)
{
	m_alpha = alpha;
}

inline mlFloat aiNeuralNetworkSystem::GetBiasAlpha(void)
{
	return m_biasAlpha;
}

inline void aiNeuralNetworkSystem::SetBiasAlpha(const mlFloat & biasAlpha)
{
	m_biasAlpha = biasAlpha;
}

inline mlFloat aiNeuralNetworkSystem::GetMomentum(void)
{
	return m_momentum;
}

inline void aiNeuralNetworkSystem::SetMomentum(const mlFloat & momentum)
{
	m_momentum = momentum;
}

inline mlFloat aiNeuralNetworkSystem::GetDecayRate(void)
{
	return m_decayRate;
}

inline void aiNeuralNetworkSystem::SetDecayRate(const mlFloat & decayRate)
{
	m_decayRate = decayRate;
}

inline mlFloat aiNeuralNetworkSystem::GetSigmoidK(void)
{
	return m_sigmoidK;
}

inline void aiNeuralNetworkSystem::SetSigmoidK(const mlFloat & sigmoidK)
{
	m_sigmoidK = sigmoidK;
}

inline mlFloat aiNeuralNetworkSystem::GetDamping(void)
{
	return m_dampingFactor;
}

inline void aiNeuralNetworkSystem::SetDamping(const mlFloat & dampingFactor)
{
	m_dampingFactor = dampingFactor;
}

#endif // AI_NEURALNETWORKSYSTEM_H
