#ifndef CAMERAPROPERTIESENCODING_H
#define CAMERAPROPERTIESENCODING_H

#include <optional>
#include <string>
#include <vector>

#include <nlohmann/json.hpp>

class CameraProperty {
public:
    enum PropertyType {
        PropertyInvalid,
        PropertyNumeric,
        PropertyDiscrete
    };

    enum StandardCameraPropertyIDs {
        ReqPropExposureTime,
        ReqPropCroppingDim1,
        ReqPropCroppingDim2,
        ReqPropBinning,
        FirstAvailablePropertyID
    };

    CameraProperty(const int propertyCode, const std::string& descriptor) :
        _propertyCode(propertyCode),
        _descriptor(descriptor),
        _propertyType(PropertyInvalid)
    {}
    ~CameraProperty() { ; }

    void setNumeric(const double val);
    void setDiscrete(const std::string& currentOption, const std::vector<std::string>& availableOptions);

    PropertyType getPropertyType() const { return _propertyType; }
    int getPropertyCode() const { return _propertyCode; }
    double getValue() const;
    std::string getCurrentOption() const;
    const std::vector<std::string>& getAvailableOptions() const;

    nlohmann::json encodeAsJSONObject() const;
    static CameraProperty decodeFromJSONObject(const nlohmann::json& encoded);

private:
    int _propertyCode;
    std::string _descriptor;
    PropertyType _propertyType;

    double _val;
    std::string _currentOption;
    std::vector<std::string> _availableOptions;
};

#endif
