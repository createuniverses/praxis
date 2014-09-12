
// Author: Greg Santucci, 2008 and 2009
// Email: thecodewitch@gmail.com
// Web: http://createuniverses.blogspot.com/

#ifndef SINGLEWORLDCONFIGURATION_H
#define SINGLEWORLDCONFIGURATION_H

class World;

void RunSingleWorldConfiguration(
	World * pYourWorld,
	char * sName,
	int argc, char** argv);

World * GetSingleWorldConfigurationWorld();

void SingleWorld_SetInitialWindowDimensions(int nTop, int nLeft, int nWidth, int nHeight);

void UseMainWindowContext();
void UseOffscreenContext();

#endif // SINGLEWORLDCONFIGURATION_H

