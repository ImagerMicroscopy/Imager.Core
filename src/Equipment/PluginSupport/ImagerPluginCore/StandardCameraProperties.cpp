#include "StandardCameraProperties.h"

#include <algorithm>
#include <cstdio>

std::vector<CameraProperty> GetStandardProperties(double currentExposureTime, const std::pair<int, int>& currentCrop,
                                                  const std::vector<int>& allowableCropping1,
                                                  const std::vector<int>& allowableCropping2,
                                                  int currentBinning, const std::vector<int>& allowableBinning) {
    std::vector<CameraProperty> properties;
    // exposure time
    {
        CameraProperty prop(CameraProperty::ReqPropExposureTime, "Exposure time");
        prop.setNumeric(currentExposureTime);
        properties.push_back(prop);
    }
    // cropping
    {
        int first, second;
        std::vector<std::string> strCropSizes1, strCropSizes2;
        for (const int size : allowableCropping1) {
            strCropSizes1.push_back(std::to_string(size));
        }
        for (const int size : allowableCropping2) {
            strCropSizes2.push_back(std::to_string(size));
        }

        std::tie(first, second) = currentCrop;
        CameraProperty prop1(CameraProperty::ReqPropCroppingDim1, "Sensor cropping 1");
        prop1.setDiscrete(std::to_string(first), strCropSizes1);
        properties.push_back(prop1);
        CameraProperty prop2(CameraProperty::ReqPropCroppingDim2, "Sensor cropping 2");
        prop2.setDiscrete(std::to_string(second), strCropSizes2);
        properties.push_back(prop2);
    }
    // binning
    {
        std::vector<std::string> binningStrs;
        std::string currentBinningStr;
        char buf[32];
        sprintf(buf, "%d", currentBinning);
        currentBinningStr = buf;
        for (const int b : allowableBinning) {
            sprintf(buf, "%d", b);
            binningStrs.push_back(buf);
        }
        CameraProperty prop(CameraProperty::ReqPropBinning, "Binning");
        prop.setDiscrete(currentBinningStr, binningStrs);
        properties.push_back(prop);
    }

    return properties;
}

DecodedStandardProperties DecodeAndRemoveStandardProperties(std::vector<CameraProperty>& properties) {
    DecodedStandardProperties decodedProperties;
    for (int i = properties.size() - 1; i >= 0; i -= 1) {
        const CameraProperty& prop = properties.at(i);
        int propertyCode = prop.getPropertyCode();

        switch (propertyCode) {
            case CameraProperty::ReqPropExposureTime:
                decodedProperties.exposureTime = prop.getValue();
                break;
            case CameraProperty::ReqPropCroppingDim1:
                decodedProperties.crop1 = std::stoi(prop.getCurrentOption());
                break;
            case CameraProperty::ReqPropCroppingDim2:
                decodedProperties.crop2 = std::stoi(prop.getCurrentOption());
                break;
            case CameraProperty::ReqPropBinning:
                decodedProperties.binningFactor = std::stoi(prop.getCurrentOption());
                break;
            default:
                continue;
        }
        properties.erase(properties.begin() + i);
    }

    return decodedProperties;
}

std::vector<int> StandardCroppingOptions(int uncroppedDimension) {
    int cropDimensions[] = { 16,32,64,128,256,512,1024,1280,1536,2048,3072,4096 };
    std::vector<int> result;
    std::copy_if(std::cbegin(cropDimensions), std::cend(cropDimensions), std::back_inserter(result),
                 [=](int i) -> bool {
                     return (i < uncroppedDimension);
                 });
    result.push_back(uncroppedDimension);
    return result;
}

std::vector<int> StandardBinningOptions() {
    return { 1, 2, 4 };
}
