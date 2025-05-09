#ifndef MICROSCOPECONTROLDLL_H
#define MICROSCOPECONTROLDLL_H

// http://www.flounder.com/ultimateheaderfile.htm

#include <cstdint>

#ifdef COMPILING_MICROSCOPEDLL_H
#define LIBSPEC __declspec(dllexport)
#else
#define LIBSPEC __declspec(dllimport)
#endif

#ifdef __cplusplus
extern "C" {
#endif
    LIBSPEC int InitImagerPlugin(void(*printer)(const char*));
    LIBSPEC void ShutdownImagerPlugin();
	LIBSPEC int ImagerPluginAPIVersion(int* version);
	LIBSPEC int EquipmentName(char* name, int maxNBytesPerName);

    LIBSPEC int ListAvailableLightSources(char** namesPtr, int nNames, int maxNBytesPerName, int* nNamesReturned);
    LIBSPEC int ListAvailableChannels(char* lightSourceName, char** namesPtr, int nNames, int maxNBytesPerName, int* nNamesReturned, int* canControlPower, int* channelsAreExclusive);
    LIBSPEC int ActivateLightSource(char* lightSourceName, char** channelNames, double* illuminationPowers, int nChannels);
    LIBSPEC int DeactivateLightSource();

	LIBSPEC int ListAvailableFilterWheels(char** namesPtr, int nNames, int maxNBytesPerName, int* nNamesReturned);
    LIBSPEC int ListAvailableFilters(char* filterWheelName, char** namesPtr, int nNames, int maxNBytesPerName, int* nNamesReturned);
    LIBSPEC int SetFilter(char* filterWheelName, char* filterName);

    LIBSPEC int HasMotorizedStage(int* hasIt);
	LIBSPEC int MotorizedStageName(char* name, int maxNBytesPerName);
    LIBSPEC int SupportedStageAxes(int* x, int* y, int* z);
    LIBSPEC int GetStagePosition(double* x, double* y, double* z, int* usingHardwareAF, int* afOffset);
    LIBSPEC int SetStagePosition(double x, double y, double z, int usingHardwareAF, int afOffset);
#ifdef __cplusplus
}
#endif

#endif
