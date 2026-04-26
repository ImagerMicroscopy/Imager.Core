#ifndef DEVICETEMPLATES_H
#define DEVICETEMPLATES_H

#include <functional>
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

class LightSourceFunctor : public LightSource {
public:
    LightSourceFunctor(const std::string& name, const std::vector<std::string>& channels, bool canControlPower, bool allowMultipleChannelsAtOnce,
                       std::function<void(const std::vector<ChannelSetting>&)> activateFunc,
                       std::function<void()> deactivateFunc) :
        _name(name), _channels(channels), _canControlPower(canControlPower), _allowMultipleChannelsAtOnce(allowMultipleChannelsAtOnce),
        _activateFunc(activateFunc), _deactivateFunc(deactivateFunc)
    {}

    std::string getName() const override { return _name; }
    std::vector<std::string> getChannels() const override { return _channels; }
    bool canControlPower() const override { return _canControlPower; }
    bool allowMultipleChannelsAtOnce() const override { return _allowMultipleChannelsAtOnce; }
    void activate(const std::vector<ChannelSetting>& channelSettings) override { _activateFunc(channelSettings); }
    void deactivate() override { _deactivateFunc(); }

private:
    std::string _name;
    std::vector<std::string> _channels;
    bool _canControlPower;
    bool _allowMultipleChannelsAtOnce;
    std::function<void(const std::vector<ChannelSetting>&)> _activateFunc;
    std::function<void()> _deactivateFunc;
};

class DiscreteMovableComponentFunctor : public DiscreteMovableComponent {
public:
    DiscreteMovableComponentFunctor(const std::string& name, const std::vector<std::string>& settings,
                                    std::function<void(const std::string&)> setToFunc)
        : _name(name), _settings(settings), _setToFunc(setToFunc) {}

    std::string getName() const override { return _name; }
    std::vector<std::string> getSettings() const override { return _settings; }
    void setTo(const std::string& setting) override { _setToFunc(setting); }

private:
    std::string _name;
    std::vector<std::string> _settings;
    std::function<void(const std::string&)> _setToFunc;
};

class ContinuouslyMovableComponentFunctor : public ContinuouslyMovableComponent {
public:
    ContinuouslyMovableComponentFunctor(const std::string& name, double minValue, double maxValue, double increment,
                                        std::function<void(double)> setToFunc)
        : _name(name), _minValue(minValue), _maxValue(maxValue), _increment(increment), _setToFunc(setToFunc) {}

    std::string getName() const override { return _name; }
    double getMinValue() const override { return _minValue; }
    double getMaxValue() const override { return _maxValue; }
    double getIncrement() const override { return _increment; }
    void setTo(double value) override { _setToFunc(value); }

private:
    std::string _name;
    double _minValue, _maxValue, _increment;
    std::function<void(double)> _setToFunc;
};

class MotorizedStageFunctor : public MotorizedStage {
public:
    MotorizedStageFunctor(const std::string& name, bool supportsX, bool supportsY, bool supportsZ,
                          std::function<Position()> getPositionFunc,
                          std::function<void(Position)> setPositionFunc)
        : _name(name), _supportsX(supportsX), _supportsY(supportsY), _supportsZ(supportsZ),
          _getPositionFunc(getPositionFunc), _setPositionFunc(setPositionFunc) {}

    std::string getName() const override { return _name; }
    bool supportsX() const override { return _supportsX; }
    bool supportsY() const override { return _supportsY; }
    bool supportsZ() const override { return _supportsZ; }
    Position getPosition() override { return _getPositionFunc(); }
    void setPosition(Position position) override { _setPositionFunc(position); }

private:
    std::string _name;
    bool _supportsX, _supportsY, _supportsZ;
    std::function<Position()> _getPositionFunc;
    std::function<void(Position)> _setPositionFunc;
};

class RobotFunctor : public Robot {
public:
    RobotFunctor(const std::string& name, const std::vector<RobotProgramDescription>& programDescriptions,
                 std::function<void(const RobotProgramExecutionParams&)> executeProgramFunc,
                 std::function<std::variant<bool, ErrorMessage>()> isExecutingFunc,
                 std::function<void()> stopFunc)
        : _name(name), _programDescriptions(programDescriptions),
          _executeProgramFunc(executeProgramFunc), _isExecutingFunc(isExecutingFunc), _stopFunc(stopFunc) {}

    std::string getName() const override { return _name; }
    std::vector<RobotProgramDescription> getProgramDescriptions() const override { return _programDescriptions; }
    void executeProgram(const RobotProgramExecutionParams& programParams) override { _executeProgramFunc(programParams); }
    std::variant<bool, ErrorMessage> isExecuting() const override { return _isExecutingFunc(); }
    void stop() override { _stopFunc(); }

private:
    std::string _name;
    std::vector<RobotProgramDescription> _programDescriptions;
    std::function<void(const RobotProgramExecutionParams&)> _executeProgramFunc;
    std::function<std::variant<bool, ErrorMessage>()> _isExecutingFunc;
    std::function<void()> _stopFunc;
};


#endif // DEVICETEMPLATES_H
