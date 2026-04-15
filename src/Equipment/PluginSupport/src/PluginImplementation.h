#ifndef PLUGINIMPLEMENTATION_H
#define PLUGINIMPLEMENTATION_H

#include <filesystem>
#include <memory>
#include <string>
#include <vector>

#include "DeviceTemplates.h"

void InitPlugin(const std::filesystem::path& configDirPath);

void ShutdownPlugin();

#endif // PLUGINIMPLEMENTATION_H