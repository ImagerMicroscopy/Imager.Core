#ifndef IMAGERPLUGIN_H
#define IMAGERPLUGIN_H

// http://www.flounder.com/ultimateheaderfile.htm

#include <cstdint>

#ifdef COMPILING_IMAGERPLUGIN  // set this define when compiling a plugin
    #ifdef __linux__
        #define LIBSPEC __attribute__((visibility("default")))
    #else
        #define LIBSPEC __declspec(dllexport)
    #endif
#else
    #ifdef __linux__
        #define LIBSPEC 	// https://stackoverflow.com/questions/2164827/explicitly-exporting-shared-library-functions-in-linux
    #else
        #define LIBSPEC __declspec(dllimport)
    #endif
#endif

#define IMAGER_API_VERSION 1

#ifdef __cplusplus
extern "C" {
#endif
    LIBSPEC int InitImagerPlugin(void(*printer)(const char*));
    LIBSPEC void ShutdownImagerPlugin();
	LIBSPEC int ImagerPluginAPIVersion(int* version);
	LIBSPEC int EquipmentName(char* name, int maxNBytesPerName);

    LIBSPEC int ListAvailableLightSources(char** namesPtr, int nNames, int maxNBytesPerName, int* nNamesReturned);
    LIBSPEC int ListAvailableChannels(char* lightSourceName, char** namesPtr, int nNames, int maxNBytesPerName, int* nNamesReturned, int* canControlPower, int* allowMultipleChannelsAtOnce);
    LIBSPEC int ActivateLightSource(char* lightSourceName, char** channelNames, double* illuminationPowers, int nChannels);
    LIBSPEC int DeactivateLightSource();

	LIBSPEC int ListDiscreteMovableComponents(char** namesPtr, int nNames, int maxNBytesPerName, int* nNamesReturned);
    LIBSPEC int ListContinuouslyMovableComponents(char** namesPtr, int nNames, int maxNBytesPerName, int* nNamesReturned);
    LIBSPEC int ListDiscreteMovableComponentSettings(char* discreteComponentName, char** namesPtr, 
                                                     int nNames, int maxNBytesPerName, int* nNamesReturned);
    LIBSPEC int ListContinuouslyMovableComponentRange(char* discreteComponentName, double* minValue, double* maxValue, double* increment);
    LIBSPEC int SetMovableComponents(int nDiscreteComponentNames, char** discreteComponentNames, char** discreteSettings,
                                     int nContinuousComponentNames, char** continuousComponentNames, double* continuousSettings);

    LIBSPEC int HasMotorizedStage(int* hasIt);
	LIBSPEC int MotorizedStageName(char* name, int maxNBytesPerName);
    LIBSPEC int SupportedStageAxes(int* x, int* y, int* z);
    LIBSPEC int GetStagePosition(double* x, double* y, double* z, int* usingHardwareAF, int* afOffset);
    LIBSPEC int SetStagePosition(double x, double y, double z, int usingHardwareAF, int afOffset);
#ifdef __cplusplus
}
#endif

#endif
