#ifndef PLUGIN_MANAGER_H
#define PLUGIN_MANAGER_H

#include <memory>
#include <stdexcept>
#include <string>

#include "DeviceTemplates.h"
#include "BaseCameraClass.h"

class PluginManager {
public:
    static PluginManager& Manager() {
        static PluginManager instance;
        return instance;
    }

    PluginManager(const PluginManager&) = delete;
    PluginManager& operator=(const PluginManager&) = delete;

    void addLightSource(std::shared_ptr<LightSource> lightSource) {
        _availableLightSources.push_back(lightSource);
    }
    void addDiscreteMovableComponent(std::shared_ptr<DiscreteMovableComponent> component) {
        _availableDiscreteMovableComponents.push_back(component);
    }
    void addContinuouslyMovableComponent(std::shared_ptr<ContinuouslyMovableComponent> component) {
        _availableContinuouslyMovableComponents.push_back(component);
    }
    void addMotorizedStage(std::shared_ptr<MotorizedStage> stage) {
        _availableMotorizedStages.push_back(stage);
    }
    void addRobot(std::shared_ptr<Robot> robot) {
        _availableRobots.push_back(robot);
    }

    void addCamera(std::shared_ptr<BaseCameraClass> camera) {
        _availableCameras.push_back(camera);
    }

    std::vector<std::shared_ptr<LightSource>> getAvailableLightSources() {return _availableLightSources;}
    std::shared_ptr<LightSource> getLightSourceByName(const std::string& name);

    std::vector<std::shared_ptr<DiscreteMovableComponent>> getAvailableDiscreteMovableComponents() {return _availableDiscreteMovableComponents;}
    std::shared_ptr<DiscreteMovableComponent> getDiscreteMovableComponentByName(const std::string& name);

    std::vector<std::shared_ptr<ContinuouslyMovableComponent>> getAvailableContinuouslyMovableComponents() {return _availableContinuouslyMovableComponents;}
    std::shared_ptr<ContinuouslyMovableComponent> getContinuouslyMovableComponentByName(const std::string& name);

    std::vector<std::shared_ptr<MotorizedStage>> getAvailableMotorizedStages() {return _availableMotorizedStages;}
    std::shared_ptr<MotorizedStage> getMotorizedStageByName(const std::string& name);

    std::vector<std::shared_ptr<Robot>> getAvailableRobots() {return _availableRobots;}
    std::shared_ptr<Robot> getRobotByName(const std::string& name);

    std::vector<std::shared_ptr<BaseCameraClass>> getAvailableCameras() {return _availableCameras;}
    std::shared_ptr<BaseCameraClass> getCameraByName(const std::string& name);

    void setPrinter(void (*printer)(const char*)) {
        if (_printer != nullptr) {
            throw std::logic_error("Plugin printer function already set");
        }
        _printer = printer;
    }

    void Print(const std::string& message) {
        if (_printer) {
            _printer(message.c_str());
        } else {
            throw std::logic_error("Plugin printer function not set");
        }
    }
private:
    PluginManager() = default;
    ~PluginManager() = default;

    std::vector<std::shared_ptr<LightSource>> _availableLightSources;
    std::vector<std::shared_ptr<DiscreteMovableComponent>> _availableDiscreteMovableComponents;
    std::vector<std::shared_ptr<ContinuouslyMovableComponent>> _availableContinuouslyMovableComponents;
    std::vector<std::shared_ptr<MotorizedStage>> _availableMotorizedStages;
    std::vector<std::shared_ptr<Robot>> _availableRobots;

    std::vector<std::shared_ptr<BaseCameraClass>> _availableCameras;

    void (*_printer)(const char*) = nullptr;
};

#endif // PLUGIN_MANAGER_H