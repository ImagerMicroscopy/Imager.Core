#include "CameraPropertiesEncoding.h"

void CameraProperty::setNumeric(const double val) {
    if (_propertyType != CameraProperty::PropertyInvalid) {
        throw std::runtime_error("trying to reset encoded camera property");
    }
    _propertyType = CameraProperty::PropertyNumeric;
    _val = val;
}

void CameraProperty::setDiscrete(const std::string& currentOption, const std::vector<std::string>& availableOptions) {
    if (_propertyType != CameraProperty::PropertyInvalid) {
        throw std::runtime_error("trying to reset encoded camera property");
    }
    _propertyType = CameraProperty::PropertyDiscrete;
    _currentOption = currentOption;
    _availableOptions = availableOptions;
}

double CameraProperty::getValue() const {
    if (_propertyType != PropertyNumeric) {
        throw std::runtime_error("fetching numeric from discrete");
    }
    return _val;
}

std::string CameraProperty::getCurrentOption() const {
    if (_propertyType != PropertyDiscrete) {
        throw std::runtime_error("fetching discrete from numeric");
    }
    return _currentOption;
}

const std::vector<std::string>& CameraProperty::getAvailableOptions() const {
    if (_propertyType != PropertyDiscrete) {
        throw std::runtime_error("fetching discrete from numeric");
    }
    return _availableOptions;
}

nlohmann::json CameraProperty::encodeAsJSONObject() const {
    nlohmann::json encoded;
    encoded["propertycode"] = _propertyCode;
    encoded["descriptor"] = _descriptor;
    if (_propertyType == CameraProperty::PropertyNumeric) {
        encoded["kind"] = "numeric";
        encoded["value"] = _val;
    } else if (_propertyType == CameraProperty::PropertyDiscrete) {
        encoded["kind"] = "discrete";
        encoded["current"] = _currentOption;
        encoded["availableoptions"] = _availableOptions;
    } else {
        throw std::runtime_error("trying to encode unknown camera property type");
    }
    return encoded;
}

CameraProperty CameraProperty::decodeFromJSONObject(const nlohmann::json& encoded) {
    int propertyCode = encoded["propertycode"];
    std::string descriptor = encoded["descriptor"];
    CameraProperty cameraProperty(propertyCode, descriptor);
    std::string kind = encoded["kind"];
    if (kind == "numeric") {
        cameraProperty.setNumeric(encoded["value"]);
    } else if (kind == "discrete") {
        cameraProperty.setDiscrete(encoded["current"], encoded["availableoptions"]);
    } else {
        throw std::runtime_error("decoding camera property from incorrect object");
    }
    return cameraProperty;
}
