#define COMPILING_IMAGERPLUGIN

#include "ImagerPlugin.h"

#include <algorithm>
#include <cstdio>
#include <functional>
#include <stdexcept>
#include <string>
#include <vector>

#include "RobotProgramArguments.h"
#include "CameraPropertiesEncoding.h"
#include "PluginManager.h"

const char* gEquipmentName = IMAGER_EQUIPMENT_NAME;   // set in the build configuration file.

// Forward declarations of functions that must be
// defined in the actual plugin implementation.
void InitPlugin();
void ShutdownPlugin();

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
		PluginManager::Manager().Print(e.what());
        return -1;
    }
    catch (...) {
		PluginManager::Manager().Print("unknown exception");
        return -2;
    }
    return 0;
}

template<typename T>
T Limit(T a, T b, T c) {
    return std::max(a, std::min(b, c));
}

// A utility function to store a list of strings in buffers passed by Imager. Returns the number of items actually stored.
int StoreStringListInBuffers(const std::vector<std::string>& stringList, char** stringBuffers, int nBuffers, int maxNBytesPerName);

int InitImagerPlugin(char* configurationDirPath, void(*printer)(const char*)) {
    // configurationDirPath is a path to a folder where you can read or write configuration data.
    // use the name of your plugin as the base name (without extension) of the config file.
    // printer is a function pointer that you can use to print output in the main program window.
    return HandleExceptions([&]() {
        PluginManager::Manager().setPrinter(printer);

        InitPlugin();

        PluginManager::Manager().Print("Successfully initialized\n");
    });
}

void ShutdownImagerPlugin() {
    ShutdownPlugin();
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
        PluginManager& manager = PluginManager::Manager();
        auto lightSources = manager.getAvailableLightSources();
        std::vector<std::string> lsNames;
        for (const auto& ls : lightSources) {
            lsNames.push_back(ls->getName());
        }
        
        *nNamesReturned = StoreStringListInBuffers(lsNames, namesPtr, nNames, maxNBytesPerName);
        if (*nNamesReturned != lsNames.size()) {
            throw std::runtime_error("Could not return all available light sources");
        }
    });
}

int ListAvailableChannels(char *lightSourceName, char **namesPtr, int nNames, int maxNBytesPerName,
                          int *nNamesReturned, int *canControlPower, int *allowMultipleChannelsAtOnce) {
    return HandleExceptions([&]() {
        PluginManager& manager = PluginManager::Manager();
        auto lightSource = manager.getLightSourceByName(lightSourceName);

        std::vector<std::string> channels = lightSource->getChannels();
        *nNamesReturned = StoreStringListInBuffers(channels, namesPtr, nNames, maxNBytesPerName);
        if (*nNamesReturned != channels.size()) {
            throw std::runtime_error(std::string("Could not return all available channels for light source ") + lightSourceName);
        }
        *canControlPower = lightSource->canControlPower() ? 1 : 0; 
        *allowMultipleChannelsAtOnce = lightSource->allowMultipleChannelsAtOnce() ? 1 : 0;
    });
}

int ActivateLightSource(char *lightSourceName, char **channelNames, double *illuminationPowers, int nChannels) {
    return HandleExceptions([&]() {
        PluginManager& manager = PluginManager::Manager();
        auto lightSource = manager.getLightSourceByName(lightSourceName);

        std::vector<LightSource::ChannelSetting> channelSettings;
        for (int i = 0; i < nChannels; i++) {
            if (channelNames[i] == nullptr) {
                throw std::runtime_error("Null channel name provided");
            }
            channelSettings.emplace_back(channelNames[i], Limit(illuminationPowers[i], 0.0, 100.0));
        }

        lightSource->activate(channelSettings);
    });
}

int DeactivateLightSource() {
    return HandleExceptions([&]() {
        PluginManager& manager = PluginManager::Manager();
        auto lightSources = manager.getAvailableLightSources();
        for (const auto& ls : lightSources) {
            ls->deactivate();
        }
    });
}

int ListDiscreteMovableComponents(char **namesPtr, int nNames, int maxNBytesPerName, int *nNamesReturned) {
    return HandleExceptions([&]() {
        PluginManager& manager = PluginManager::Manager();
        auto components = manager.getAvailableDiscreteMovableComponents();
        std::vector<std::string> componentNames;
        for (const auto& comp : components) {
            componentNames.push_back(comp->getName());
        }
        *nNamesReturned = StoreStringListInBuffers(componentNames, namesPtr, nNames, maxNBytesPerName);
        if (*nNamesReturned != componentNames.size()) {
            throw std::runtime_error("Could not return all available discrete movable components");
        }
    });
}

int ListContinuouslyMovableComponents(char **namesPtr, int nNames, int maxNBytesPerName, int *nNamesReturned) {
    return HandleExceptions([&]() {
        PluginManager& manager = PluginManager::Manager();
        auto components = manager.getAvailableContinuouslyMovableComponents();
        std::vector<std::string> componentNames;
        for (const auto& comp : components) {
            componentNames.push_back(comp->getName());
        }
        *nNamesReturned = StoreStringListInBuffers(componentNames, namesPtr, nNames, maxNBytesPerName);
        if (*nNamesReturned != componentNames.size()) {
            throw std::runtime_error("Could not return all available continously movable components");
        }
    });
}

int ListDiscreteMovableComponentSettings(char *discreteComponentName, char **namesPtr, int nNames,
                                         int maxNBytesPerName, int *nNamesReturned) {
    return HandleExceptions([&]() {
        PluginManager& manager = PluginManager::Manager();
        auto component = manager.getDiscreteMovableComponentByName(discreteComponentName);
        std::vector<std::string> settings = component->getSettings();
        *nNamesReturned = StoreStringListInBuffers(settings, namesPtr, nNames, maxNBytesPerName);
        if (*nNamesReturned != settings.size()) {
            throw std::runtime_error("Could not return all available continously movable components");
        }
    });
}

int ListContinuouslyMovableComponentRange(char *discreteComponentName, double *minValue, double *maxValue,
    double *increment) {
    return HandleExceptions([&]() {
        PluginManager& manager = PluginManager::Manager();
        auto component = manager.getContinuouslyMovableComponentByName(discreteComponentName);
        *minValue = component->getMinValue();
        *maxValue = component->getMaxValue();
        *increment = component->getIncrement();
    });
}

int SetMovableComponents(int nDiscreteComponentNames, char **discreteComponentNames, char **discreteSettings,
                         int nContinuousComponentNames, char **continuousComponentNames, double *continuousSettings) {
    return HandleExceptions([&]() {
        PluginManager& manager = PluginManager::Manager();

        for (int i = 0; i < nDiscreteComponentNames; i++) {
            if (discreteComponentNames[i] == nullptr || discreteSettings[i] == nullptr) {
                throw std::runtime_error("Null discrete component name or setting provided");
            }
            auto component = manager.getDiscreteMovableComponentByName(discreteComponentNames[i]);
            component->setTo(discreteSettings[i]);
        }

        for (int i = 0; i < nContinuousComponentNames; i++) {
            if (continuousComponentNames[i] == nullptr) {
                throw std::runtime_error("Null continuous component name provided");
            }
            auto component = manager.getContinuouslyMovableComponentByName(continuousComponentNames[i]);
            component->setTo(continuousSettings[i]);
        }
    });
}

int HasMotorizedStage(int *hasIt) {
    return HandleExceptions([&] {
        *hasIt = (PluginManager::Manager().getAvailableMotorizedStages().empty() ? 0 : 1);
    });
}

int MotorizedStageName(char *name, int maxNBytesPerName) {
    return HandleExceptions([&] {
        PluginManager& manager = PluginManager::Manager();
        auto stages = manager.getAvailableMotorizedStages();
        std::string stageName;
        if (!stages.empty()) {
            stageName = stages.front()->getName();
        }
        snprintf(name, maxNBytesPerName, "%s", stageName.c_str());
    });
}

int SupportedStageAxes(int *x, int *y, int *z) {
    return HandleExceptions([&] {
        PluginManager& manager = PluginManager::Manager();
        auto stages = manager.getAvailableMotorizedStages();
        if (!stages.empty()) {
            *x = stages.front()->supportsX() ? 1 : 0;
            *y = stages.front()->supportsY() ? 1 : 0;
            *z = stages.front()->supportsZ() ? 1 : 0;
        }
        else {
            *x = *y = *z = 0;
        }
    });
}

int GetStagePosition(double *x, double *y, double *z, int *usingHardwareAF, int *afOffset) {
    return HandleExceptions([&] {
        PluginManager& manager = PluginManager::Manager();
        auto stages = manager.getAvailableMotorizedStages();
        if (!stages.empty()) {
            std::tie(*x, *y, *z, *usingHardwareAF, *afOffset)= stages.front()->getPosition();
        } else {
            *x = -1.0; *y = -1.0; *z = -1.0; *usingHardwareAF = 0; *afOffset = 0;
        }
    });
}

int SetStagePosition(double x, double y, double z, int usingHardwareAF, int afOffset) {
    return HandleExceptions([&] {
        PluginManager& manager = PluginManager::Manager();
        auto stages = manager.getAvailableMotorizedStages();
        if (!stages.empty()) {
            stages.front()->setPosition({x, y, z, usingHardwareAF != 0, afOffset});
        } else {
            throw std::runtime_error("No motorized stage available");
        }
    });
}

int ListRobots(char** namesPtr, int nNames, int maxNBytesPerName, int* nNamesReturned) {
    return HandleExceptions([&] {
        PluginManager& manager = PluginManager::Manager();
        auto robots = manager.getAvailableRobots();

        std::vector<std::string> robotNames;
        for (const auto& robot : robots) {
            robotNames.push_back(robot->getName());
        }
        *nNamesReturned = StoreStringListInBuffers(robotNames, namesPtr, nNames, maxNBytesPerName);
        if (*nNamesReturned != robotNames.size()) {
            throw std::runtime_error("Could not return all available robots");
        }
    });
}

int ListRobotPrograms(char* robotName, char** encodedProgramsInfoPtr) {
    return HandleExceptions([&] {
        PluginManager& manager = PluginManager::Manager();
        auto robot = manager.getRobotByName(robotName);
        std::vector<RobotProgramDescription> programDescriptions = robot->getProgramDescriptions();
        std::string encoded = EncodeRobotProgramsAsJSON(programDescriptions);
        *encodedProgramsInfoPtr = new char[encoded.size() + 1];
        memcpy(*encodedProgramsInfoPtr, encoded.data(), encoded.size() + 1);
    });
}

void ReleaseRobotProgramsInfo(char* info) {
    delete[] info;
}

int ExecuteRobotProgram(char* robotName, char* encodedProgramCallParams) {
    return HandleExceptions([&]() {
        PluginManager& manager = PluginManager::Manager();
        auto robot = manager.getRobotByName(robotName);
        RobotProgramExecutionParams callParams(encodedProgramCallParams);
        robot->executeProgram(callParams);
    });
}

int RobotIsExecuting(char* robotName, int* isExecuting, char* possibleErrorMessage, int maxNBytesForErrorMessage) {
    return HandleExceptions([&]() {
        // set *isExecuting to 1 if the robot is currently executing a program, 0 otherwise.
        // possibleErrorMessage may contain an error message, in which case the measurement program will be aborted.
        possibleErrorMessage[0] = 0;   // set to an error message if needed, making sure not to exceed maxNBytesForErrorMessage
        PluginManager& manager = PluginManager::Manager();
        auto robot = manager.getRobotByName(robotName);
        auto executingStatus = robot->isExecuting();
        if (std::holds_alternative<bool>(executingStatus)) {
            *isExecuting = std::get<bool>(executingStatus) ? 1 : 0;
        }
        else {
            std::string errorMessage = std::get<std::string>(executingStatus);
            snprintf(possibleErrorMessage, maxNBytesForErrorMessage, "%s", errorMessage.c_str());
        }
    });
}

int StopRobots() {
    return HandleExceptions([&]() {
        PluginManager& manager = PluginManager::Manager();
        auto robots = manager.getAvailableRobots();
        for (const auto& robot : robots) {
            robot->stop();
        }
    });
}

int ListConnectedCameraNames(char **namesPtr, int nNames, int maxNBytesPerName, int *nNamesReturned) {
    return HandleExceptions([&]() {
        *nNamesReturned = 0;
    });
}

int GetCameraOptions(char* cameraName, char** encodedOptionsPtr) {
    return HandleExceptions([&]() {
        // The camera options need to be encoded as JSON. See CameraPropertiesEncoding.cpp/.h.
        // Essence of a possible implementation:
        // std::vector<CameraProperty> properties = camPtr->getCameraProperties();
        // std::vector<nlohmann::json> encodedProps;
        // for (const auto& p : properties) {
        //     encodedProps.push_back(p.encodeAsJSONObject());
        // }
        // nlohmann::json object;
        // object["properties"] = encodedProps;
        // std::string serialized = object.dump();
        // *encodedOptionsPtr = new char[serialized.size() + 1];
        // memcpy(*encodedOptionsPtr, serialized.data(), serialized.size() + 1);
    });
}

void ReleaseOptionsData(char* data) {
    // Release the data the plugin returned in GetCameraOptions(). This function will be called automatically when Imager
    // has received the data. Possible implementation:
    // delete[] data;
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
