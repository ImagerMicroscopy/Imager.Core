#ifndef ROBOTPROGRAMARGUMENTS_H
#define ROBOTPROGRAMARGUMENTS_H

#include <map>
#include <string>
#include <variant>
#include <vector>

#include "nlohmann/json.hpp"

class RobotProgramDiscreteArgumentInfo {
public:
    RobotProgramDiscreteArgumentInfo(const std::string& name, const std::vector<std::string>& possibleValues)
        : _name(name), _possibleValues(possibleValues) {}
    ~RobotProgramDiscreteArgumentInfo() = default;

    nlohmann::json encodeAsJSONObject() const;

private:
    std::string _name;
    std::vector<std::string> _possibleValues;
};

class RobotProgramContinuousArgumentInfo {
public:
    RobotProgramContinuousArgumentInfo(const std::string& name, double minValue, double maxValue, double increment)
        : _name(name), _minValue(minValue), _maxValue(maxValue), _increment(increment) {}
    ~RobotProgramContinuousArgumentInfo() = default;

    nlohmann::json encodeAsJSONObject() const;

private:
    std::string _name;
    double _minValue;
    double _maxValue;
    double _increment;
};

using RobotProgramArgumentInfo = std::variant<RobotProgramDiscreteArgumentInfo, RobotProgramContinuousArgumentInfo>;

class RobotProgramDescription {
public:
    RobotProgramDescription(const std::string& programName) : _programName(programName) {
        if (programName.empty()) {
            throw std::invalid_argument("Program name cannot be empty");
        }
        _programName = programName;
    }
    ~RobotProgramDescription() = default;

    void addArgument(const RobotProgramArgumentInfo& argumentInfo) {
        _argumentsInfo.push_back(argumentInfo);
    }

    nlohmann::json encodeAsJSON() const;
private:
    std::string _programName;
    std::vector<RobotProgramArgumentInfo> _argumentsInfo;
};

std::string EncodeRobotProgramsAsJSON(const std::vector<RobotProgramDescription>& programsInfo);

class RobotProgramExecutionDiscreteArgument {
public:
    RobotProgramExecutionDiscreteArgument(const std::string& argumentName, const std::string& value)
        : _argumentName(argumentName), _value(value) {}
    ~RobotProgramExecutionDiscreteArgument() = default;

private:
    std::string _argumentName;
    std::string _value;
};

class RobotProgramExecutionContinuousArgument {
public:
    RobotProgramExecutionContinuousArgument(const std::string& argumentName, double value)
        : _argumentName(argumentName), _value(value) {}
    ~RobotProgramExecutionContinuousArgument() = default;

    private:
    std::string _argumentName;
    double _value;
};

using RobotProgramExecutionArgument = std::variant<RobotProgramExecutionDiscreteArgument, RobotProgramExecutionContinuousArgument>;

class RobotProgramExecutionParams {
public:
    RobotProgramExecutionParams(const std::string& encodedExectionParams);
    ~RobotProgramExecutionParams() = default;

    const std::string& programName() const { return _programName; }
    const std::map<std::string, RobotProgramExecutionArgument>& arguments() const { return _arguments; }

private:
    std::string _programName;
    std::map<std::string, RobotProgramExecutionArgument> _arguments;
};
#endif // ROBOTPROGRAMARGUMENTS_H