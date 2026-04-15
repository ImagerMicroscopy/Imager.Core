#ifndef PLUGINWRAPPER_H
#define PLUGINWRAPPER_H

#include <string>
#include <variant>
#include <vector>

#include "RobotProgramArguments.h"

class LightSource {
public:
    LightSource() = default;
    virtual ~LightSource() = default;
    LightSource(const LightSource&) = delete;
    LightSource& operator=(const LightSource&) = delete;
    
    virtual std::string getName() const = 0;
    virtual std::vector<std::string> getChannels() const = 0;
    virtual bool canControlPower() const = 0;
    virtual bool allowMultipleChannelsAtOnce() const = 0;

    using ChannelSetting = std::pair<std::string, double>;   // channel name, illumination power
    virtual void activate(const std::vector<ChannelSetting>& channelSettings) = 0;
    virtual void deactivate() = 0;
};

class DiscreteMovableComponent {
public:
    DiscreteMovableComponent() = default;
    virtual ~DiscreteMovableComponent() = default;
    DiscreteMovableComponent(const DiscreteMovableComponent&) = delete;
    DiscreteMovableComponent& operator=(const DiscreteMovableComponent&) = delete;

    virtual std::string getName() const = 0;
    virtual std::vector<std::string> getSettings() const = 0;

    virtual void setTo(const std::string& setting) = 0;
};

class ContinuouslyMovableComponent {
public:
    ContinuouslyMovableComponent() = default;
    virtual ~ContinuouslyMovableComponent() = default;
    ContinuouslyMovableComponent(const ContinuouslyMovableComponent&) = delete;
    ContinuouslyMovableComponent& operator=(const ContinuouslyMovableComponent&) = delete;

    virtual std::string getName() const = 0;
    virtual double getMinValue() const = 0;
    virtual double getMaxValue() const = 0;
    virtual double getIncrement() const = 0;

    virtual void setTo(double value) = 0;
};

class MotorizedStage {
public:
    MotorizedStage() = default;
    virtual ~MotorizedStage() = default;
    MotorizedStage(const MotorizedStage&) = delete;
    MotorizedStage& operator=(const MotorizedStage&) = delete;

    virtual std::string getName() const = 0;
    virtual bool supportsX() const = 0;
    virtual bool supportsY() const = 0;
    virtual bool supportsZ() const = 0;

    using Position = std::tuple<double, double, double, bool, int>;   // x, y, z, usingHardwareAF, afOffset
    virtual Position getPosition() = 0;
    virtual void setPosition(Position position) = 0;
};

class Robot {
public:
    Robot() = default;
    virtual ~Robot() = default;
    Robot(const Robot&) = delete;
    Robot& operator=(const Robot&) = delete;

    virtual std::string getName() const = 0;
    virtual std::vector<RobotProgramDescription> getProgramDescriptions() const = 0;
    virtual void executeProgram(const RobotProgramExecutionParams& programParams) = 0;
    using ErrorMessage = std::string;
    virtual std::variant<bool, ErrorMessage> isExecuting() const = 0;
    virtual void stop() = 0;
};


#endif // PLUGINWRAPPER_H