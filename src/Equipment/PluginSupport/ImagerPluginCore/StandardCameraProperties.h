#ifndef SRC_STANDARDCAMERAPROPERTIES_H
#define SRC_STANDARDCAMERAPROPERTIES_H

#include <optional>
#include <string>
#include <vector>

#include <nlohmann/json.hpp>

// Include the core CameraProperty class from ImagerPluginCore
#include "ImagerPluginCore/CameraPropertiesEncoding.h"

// Extended functionality for standard camera properties
class DecodedStandardProperties {
public:
    std::optional<double> exposureTime;
    std::optional<int> crop1;
    std::optional<int> crop2;
    std::optional<int> binningFactor;
};

// Utility functions for standard camera properties
std::vector<CameraProperty> GetStandardProperties(double currentExposureTime, const std::pair<int, int>& currentCrop,
                                                  const std::vector<int>& allowableCropping1,
                                                  const std::vector<int>& allowableCropping2,
                                                  int currentBinning, const std::vector<int>& allowableBinning);

DecodedStandardProperties DecodeAndRemoveStandardProperties(std::vector<CameraProperty>& properties);

std::vector<int> StandardCroppingOptions(int uncroppedDimension);
std::vector<int> StandardBinningOptions();

#endif
