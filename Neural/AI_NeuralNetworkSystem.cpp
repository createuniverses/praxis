#include "AI_NeuralNetworkSystem.h"

aiNeuralNetworkSystem * aiNeuralNetworkSystem::s_instance = NULL;

aiNeuralNetworkSystem::aiNeuralNetworkSystem()
{
	ASSERT(s_instance == NULL);

	s_instance = this;

	m_alpha				= 4.5f;
	m_biasAlpha			= 0.0f;
	m_momentum			= 0.0f;
	m_decayRate			= 0.0f;
	m_sigmoidK			= 1.0f;
	m_dampingFactor		= 1.0f;	
}

aiNeuralNetworkSystem::~aiNeuralNetworkSystem()
{
	ASSERT(s_instance == this);

	s_instance = NULL;
}

aiNeuralNetworkSystem & aiNeuralNetworkSystem::GetInstance()
{
	ASSERT(s_instance);

	return *s_instance;
}

void aiNeuralNetworkSystem::Startup()
{
	ASSERT(s_instance == NULL);

	new aiNeuralNetworkSystem;
}

void aiNeuralNetworkSystem::Shutdown()
{
	ASSERT(s_instance != NULL);

	delete s_instance;
}

bool aiNeuralNetworkSystem::IsRunning()
{
	return (s_instance != NULL);
}
