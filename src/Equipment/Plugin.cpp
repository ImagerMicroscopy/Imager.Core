#define COMPILING_IMAGERPLUGIN

#include "Plugin.h"

#include <cstdio>
#include <functional>
#include <stdexcept>
#include <string>
#include <vector>

const char* gEquipmentName = "MyEquipment";        // adjust to the name of your equipment

// Use this function pointer to print output in the Imager console window
std::function<void(const char*)> gPrinter;

// A suggested utility function that provides a wrapper between C++ exceptions
// and the return-code based C interface. Use it like so:
// return HandleExceptions([&]() {
//     <your code>
// });
int HandleExceptions(const std::function<void()>& func) {
    try {
        func();
    }
    catch (const std::exception& e) {
		gPrinter(e.what());
        return -1;
    }
    catch (...) {
		gPrinter("unknown exception");
        return -2;
    }
    return 0;
}

// A utility function to store a list of strings in buffers passed by Imager. Returns the number of items actually stored.
int StoreStringListInBuffers(const std::vector<std::string>& stringList, char** stringBuffers, int nBuffers, int maxNBytesPerName);

int InitImagerPlugin(char* configurationDirPath, void(*printer)(const char*)) {
    // configurationDirPath is a path to a folder where you can read or write configuration data
    // use the name of your plugin as the base name (without extension) of the config file.
    // printer is a function pointer that you can use to print output in the main program window.
    return HandleExceptions([&]() {
        gPrinter = printer;

        // any initialization your plugin needs

        printf("Successfully initialized\n");
    });
}

void ShutdownImagerPlugin() {
    // any cleanup your plugin needs
}

int ImagerPluginAPIVersion(int* version) {
    *version = IMAGER_API_VERSION;
    return 0;
}

int EquipmentName(char* name, int maxNBytesPerName) {
    return HandleExceptions([&]() {
        if (strlen(gEquipmentName) > maxNBytesPerName - 1) {
            throw std::runtime_error("buffer too small to store the EquipmentName");
        }
        snprintf(name, maxNBytesPerName, "%s", gEquipmentName);
    });
}

int ListAvailableLightSources(char **namesPtr, int nNames, int maxNBytesPerName, int *nNamesReturned) {
    return HandleExceptions([&]() {
        std::vector<std::string> lsNames; // = <a call to a function you created>
        *nNamesReturned = StoreStringListInBuffers(lsNames, namesPtr, nNames, maxNBytesPerName);
        if (*nNamesReturned != lsNames.size()) {
            throw std::runtime_error("Could not return all available light sources");
        }
    });
}

int ListAvailableChannels(char *lightSourceName, char **namesPtr, int nNames, int maxNBytesPerName,
                          int *nNamesReturned, int *canControlPower, int *allowMultipleChannelsAtOnce) {
    return HandleExceptions([&]() {
        std::vector<std::string> channels; // = <a call to a function you created>
        *nNamesReturned = StoreStringListInBuffers(channels, namesPtr, nNames, maxNBytesPerName);
        if (*nNamesReturned != channels.size()) {
            throw std::runtime_error(std::string("Could not return all available channels for light source ") + lightSourceName);
        }
        *canControlPower = 1;   // set to 1 if the power can be controller, 0 otherwise
        *allowMultipleChannelsAtOnce = 1;   // set to 1 if multiple channels can be active simultaneously, 0 otherwise
    });
}

int ActivateLightSource(char *lightSourceName, char **channelNames, double *illuminationPowers, int nChannels) {
    return HandleExceptions([&]() {
        std::vector<std::string> channelNamesV;
        std::vector<double> illuminationPowersV;
        for (int i = 0; i < nChannels; i++) {
            channelNamesV.emplace_back(lightSourceName);
            illuminationPowersV.push_back(illuminationPowers[i]);
        }
        // call a function created passing channelNamesV and illuminationPowersV
    });
}

int DeactivateLightSource() {
    return HandleExceptions([&]() {
        // call a function you created
    });
}

int ListDiscreteMovableComponents(char **namesPtr, int nNames, int maxNBytesPerName, int *nNamesReturned) {
    return HandleExceptions([&]() {
        std::vector<std::string> components; // = <a call to a function you created>
        *nNamesReturned = StoreStringListInBuffers(components, namesPtr, nNames, maxNBytesPerName);
        if (*nNamesReturned != components.size()) {
            throw std::runtime_error("Could not return all available discrete movable components");
        }
    });
}

int ListContinuouslyMovableComponents(char **namesPtr, int nNames, int maxNBytesPerName, int *nNamesReturned) {
    return HandleExceptions([&]() {
        std::vector<std::string> components; // = <a call to a function you created>
        *nNamesReturned = StoreStringListInBuffers(components, namesPtr, nNames, maxNBytesPerName);
        if (*nNamesReturned != components.size()) {
            throw std::runtime_error("Could not return all available continously movable components");
        }
    });
}

int ListDiscreteMovableComponentSettings(char *discreteComponentName, char **namesPtr, int nNames,
                                         int maxNBytesPerName, int *nNamesReturned) {
    return HandleExceptions([&]() {
        std::vector<std::string> settings; // = <a call to a function you created>
        *nNamesReturned = StoreStringListInBuffers(settings, namesPtr, nNames, maxNBytesPerName);
        if (*nNamesReturned != settings.size()) {
            throw std::runtime_error("Could not return all available continously movable components");
        }
    });
}

int ListContinuouslyMovableComponentRange(char *discreteComponentName, double *minValue, double *maxValue,
    double *increment) {
        // set minValue, maxValue, and increment as appropriate
    return 0;
}

int SetMovableComponents(int nDiscreteComponentNames, char **discreteComponentNames, char **discreteSettings,
                         int nContinuousComponentNames, char **continuousComponentNames, double *continuousSettings) {
    return HandleExceptions([&]() {
        // adjust components as appropriate
    });
}

int HasMotorizedStage(int *hasIt) {
    return HandleExceptions([&] {
        *hasIt = 0;     // set to 1 as needed
    });
}

int MotorizedStageName(char *name, int maxNBytesPerName) {
    return HandleExceptions([&] {
        std::string stageName; // <fill in or get from a call to your function
        if (stageName.size() > maxNBytesPerName - 1) {
            throw std::runtime_error("Unable to fit stage name in buffer");
        }
        snprintf(name, maxNBytesPerName, "%s", stageName.c_str());
    });
}

int SupportedStageAxes(int *x, int *y, int *z) {
    return HandleExceptions([&] {
        *x = 0; *y = 0; *z = 0;
        // set these to 1 if the stage axis is supported
    });
}

int GetStagePosition(double *x, double *y, double *z, int *usingHardwareAF, int *afOffset) {
    return HandleExceptions([&] {
        // replace these values with those of your hardware
        *x = -1.0; *y = -1.0; *z = -1.0;
        *usingHardwareAF = 0;
        *afOffset = 0;
    });
}

int SetStagePosition(double x, double y, double z, int usingHardwareAF, int afOffset) {
    return HandleExceptions([&] {
        // set your hardware to these parameters
    });
}

int ListConnectedCameraNames(char **namesPtr, int nNames, int maxNBytesPerName, int *nNamesReturned) {
    return HandleExceptions([&]() {
        *nNamesReturned = 0;
    });
}

int GetCameraOptions(char* cameraName, char** encodedOptionsPtr) {
    return HandleExceptions([&]() {
        
    });
}

void ReleaseOptionsData(char* data) {

}

int SetCameraOption(char* cameraName, char* encodedOption) {
    return HandleExceptions([&]() {
        
    });
}

int GetFrameRate(char* cameraName, double* frameRate) {
    return -1;
}

int IsConfiguredForHardwareTriggering(char* cameraName, int* isConfiguredForHardwareTriggering) {
    return -1;
}

int AcquireSingleImage(char* cameraName, uint16_t** imagePtr, int* nRows, int* nCols) {
    return -1;
}

int StartAsyncAcquisition(char* cameraName) {
    return -1;
}

int StartBoundedAsyncAcquisition(char* cameraName, uint64_t nImagesToAcquire) {
    return -1;
}

int GetOldestImageAsyncAcquired(char* cameraName, uint32_t timeoutMillis, uint16_t** imagePtr, int* nRows, int* nCols, double* timeStamp) {
    return -1;
}

void ReleaseImageData(uint16_t* imagePtr) {
    
}

int AbortAsyncAcquisition(char* cameraName) {
    return -1;
}

void GetLastSCCamError(char* msgBuf, size_t bufSize) {
    msgBuf[0] = 0;
}

// A utility function to store a list of strings in buffers passed by Imager. Returns the number of items actually stored.
int StoreStringListInBuffers(const std::vector<std::string>& stringList, char** stringBuffers, int nBuffers, int maxNBytesPerName) {
    int nStrings = (int)stringList.size();
    int nItemsToStore = std::min(nStrings, nBuffers);
    for (int i = 0; i < nItemsToStore; ++i) {
        const std::string& item = stringList.at(i);
        if (item.size() > maxNBytesPerName - 1) {   // '-1' so the trailing nil can be stored
            throw std::runtime_error("buffer too small to store item \"" + item + "\"");
        }
        snprintf(stringBuffers[i], maxNBytesPerName, "%s", item.c_str());
    }
    return nItemsToStore;
}
