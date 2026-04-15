#include "PluginManager.h"
#include <algorithm>
#include <stdexcept>

std::shared_ptr<LightSource> PluginManager::getLightSourceByName(const std::string& name) {
    auto it = std::find_if(_availableLightSources.begin(), _availableLightSources.end(),
        [&](const std::shared_ptr<LightSource>& ls) { return ls->getName() == name; });
    if (it == _availableLightSources.end()) {
        throw std::runtime_error("Light source not found: " + name);
    }
    return *it;
}

std::shared_ptr<DiscreteMovableComponent> PluginManager::getDiscreteMovableComponentByName(const std::string& name) {
    auto it = std::find_if(_availableDiscreteMovableComponents.begin(), _availableDiscreteMovableComponents.end(),
        [&](const std::shared_ptr<DiscreteMovableComponent>& comp) { return comp->getName() == name; });
    if (it == _availableDiscreteMovableComponents.end()) {
        throw std::runtime_error("Discrete movable component not found: " + name);
    }
    return *it;
}

std::shared_ptr<ContinuouslyMovableComponent> PluginManager::getContinuouslyMovableComponentByName(const std::string& name) {
    auto it = std::find_if(_availableContinuouslyMovableComponents.begin(), _availableContinuouslyMovableComponents.end(),
        [&](const std::shared_ptr<ContinuouslyMovableComponent>& comp) { return comp->getName() == name; });
    if (it == _availableContinuouslyMovableComponents.end()) {
        throw std::runtime_error("Continuously movable component not found: " + name);
    }
    return *it;
}

std::shared_ptr<MotorizedStage> PluginManager::getMotorizedStageByName(const std::string& name) {
    auto it = std::find_if(_availableMotorizedStages.begin(), _availableMotorizedStages.end(),
        [&](const std::shared_ptr<MotorizedStage>& stage) { return stage->getName() == name; });
    if (it == _availableMotorizedStages.end()) {
        throw std::runtime_error("Motorized stage not found: " + name);
    }
    return *it;
}

std::shared_ptr<Robot> PluginManager::getRobotByName(const std::string& name) {
    auto it = std::find_if(_availableRobots.begin(), _availableRobots.end(),
        [&](const std::shared_ptr<Robot>& robot) { return robot->getName() == name; });
    if (it == _availableRobots.end()) {
        throw std::runtime_error("Robot not found: " + name);
    }
    return *it;
}