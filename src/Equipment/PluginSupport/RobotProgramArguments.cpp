#include "RobotProgramArguments.h"

nlohmann::json RobotProgramDiscreteArgumentInfo::encodeAsJSONObject() const {
    return {
        {"type", "discreteargument"},
        {"programargumentname", _name},
        {"permissiblevalues", _possibleValues}
    };
}

nlohmann::json RobotProgramContinuousArgumentInfo::encodeAsJSONObject() const {
    return {
        {"type", "continuousargument"},
        {"programargumentname", _name},
        {"minvalue", _minValue},
        {"maxvalue", _maxValue},
        {"increment", _increment}
    };
}

nlohmann::json RobotProgramDescription::encodeAsJSON() const {
    nlohmann::json json;
    json["programname"] = _programName;
    json["programarguments"] = nlohmann::json::array();
    for (const auto& argumentInfo : _argumentsInfo) {
        std::visit([&json](const auto& argInfo) {
            json["programarguments"].push_back(argInfo.encodeAsJSONObject());
        }, argumentInfo);
    }
    return json;
}

std::string EncodeRobotProgramsAsJSON(const std::vector<RobotProgramDescription>& programsInfo) {
    nlohmann::json json = nlohmann::json::array();
    for (const auto& programInfo : programsInfo) {
        json.push_back(programInfo.encodeAsJSON());
    }
    return json.dump();
}

RobotProgramExecutionParams::RobotProgramExecutionParams(const std::string& encodedExecutionParams) {
    if (encodedExecutionParams.empty()) {
        throw std::invalid_argument("Encoded execution parameters cannot be empty");
    }

    try {
        auto json = nlohmann::json::parse(encodedExecutionParams);

        if (json.contains("programname") && json["programname"].is_string()) {
            _programName = json["programname"].get<std::string>();
        }

        if (json.contains("arguments") && json["arguments"].is_array()) {
            for (const auto& arg : json["arguments"]) {
                if (!arg.contains("robotprogramargumenttype") || !arg.contains("argumentname") || !arg.contains("argument")) {
                    continue;
                }

                std::string type = arg["robotprogramargumenttype"].get<std::string>();
                std::string name = arg["argumentname"].get<std::string>();

                if (type == "discrete" && arg["argument"].is_string()) {
                    std::string value = arg["argument"].get<std::string>();
                    _arguments.emplace(name, RobotProgramExecutionDiscreteArgument(name, value));
                } else if (type == "continuous" && arg["argument"].is_number()) {
                    double value = arg["argument"].get<double>();
                    _arguments.emplace(name, RobotProgramExecutionContinuousArgument(name, value));
                }
            }
        }
    } catch (const nlohmann::json::exception& e) {
        // Handle parse errors (e.g., log or throw a custom exception)
        throw std::invalid_argument("Failed to parse encoded execution parameters");
    }
}
