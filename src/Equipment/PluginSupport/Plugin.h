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

#define IMAGER_API_VERSION 2

#ifdef __cplusplus
extern "C" {
#endif
    // Global settings
    LIBSPEC int InitImagerPlugin(char* configurationDirPath, void(*printer)(const char*));
    LIBSPEC void ShutdownImagerPlugin();
	LIBSPEC int ImagerPluginAPIVersion(int* version);
	LIBSPEC int EquipmentName(char* name, int maxNBytesPerName);

    // Light sources
    LIBSPEC int ListAvailableLightSources(char** namesPtr, int nNames, int maxNBytesPerName, int* nNamesReturned);
    LIBSPEC int ListAvailableChannels(char* lightSourceName, char** namesPtr, int nNames, int maxNBytesPerName, int* nNamesReturned, int* canControlPower, int* allowMultipleChannelsAtOnce);
    LIBSPEC int ActivateLightSource(char* lightSourceName, char** channelNames, double* illuminationPowers, int nChannels);
    LIBSPEC int DeactivateLightSource();

    // Movable components
	LIBSPEC int ListDiscreteMovableComponents(char** namesPtr, int nNames, int maxNBytesPerName, int* nNamesReturned);
    LIBSPEC int ListContinuouslyMovableComponents(char** namesPtr, int nNames, int maxNBytesPerName, int* nNamesReturned);
    LIBSPEC int ListDiscreteMovableComponentSettings(char* discreteComponentName, char** namesPtr, 
                                                     int nNames, int maxNBytesPerName, int* nNamesReturned);
    LIBSPEC int ListContinuouslyMovableComponentRange(char* discreteComponentName, double* minValue, double* maxValue, double* increment);
    LIBSPEC int SetMovableComponents(int nDiscreteComponentNames, char** discreteComponentNames, char** discreteSettings,
                                     int nContinuousComponentNames, char** continuousComponentNames, double* continuousSettings);

    // Motorized stages
    LIBSPEC int HasMotorizedStage(int* hasIt);
	LIBSPEC int MotorizedStageName(char* name, int maxNBytesPerName);
    LIBSPEC int SupportedStageAxes(int* x, int* y, int* z);
    LIBSPEC int GetStagePosition(double* x, double* y, double* z, int* usingHardwareAF, int* afOffset);
    LIBSPEC int SetStagePosition(double x, double y, double z, int usingHardwareAF, int afOffset);

    // Robots
    LIBSPEC int ListRobots(char** namesPtr, int nNames, int maxNBytesPerName, int* nNamesReturned);
    LIBSPEC int ListRobotPrograms(char* robotName, char** encodedProgramsInfoPtr);
    LIBSPEC void ReleaseRobotProgramsInfo(char* info);
    LIBSPEC int ExecuteRobotProgram(char* robotName, char* encodedProgramCallParams);
    LIBSPEC int RobotIsExecuting(char* robotName, int* isExecuting, char* possibleErrorMessage, int maxNBytesForErrorMessage);
    LIBSPEC int StopRobots();

    // Cameras
    LIBSPEC int ListConnectedCameraNames(char **namesPtr, int nNames, int maxNBytesPerName, int *nNamesReturned);
    LIBSPEC int GetCameraOptions(char* cameraName, char** encodedOptionsPtr);
    LIBSPEC void ReleaseOptionsData(char* data);
    LIBSPEC int SetCameraOption(char* cameraName, char* encodedOption);
    LIBSPEC int GetFrameRate(char* cameraName, double* frameRate);
    LIBSPEC int IsConfiguredForHardwareTriggering(char* cameraName, int* isConfiguredForHardwareTriggering);
    
    LIBSPEC int AcquireSingleImage(char* cameraName, uint16_t** imagePtr, int* nRows, int* nCols);

    LIBSPEC int StartAsyncAcquisition(char* cameraName);
    LIBSPEC int StartBoundedAsyncAcquisition(char* cameraName, uint64_t nImagesToAcquire);
    LIBSPEC int GetOldestImageAsyncAcquired(char* cameraName, uint32_t timeoutMillis, uint16_t** imagePtr, int* nRows, int* nCols, double* timeStamp);
    LIBSPEC void ReleaseImageData(uint16_t* imagePtr);
    LIBSPEC int AbortAsyncAcquisition(char* cameraName);

    LIBSPEC void GetLastSCCamError(char* msgBuf, size_t bufSize);
#ifdef __cplusplus
}
#endif

#endif
