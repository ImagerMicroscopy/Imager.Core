#include "PluginImplementation.h"

#include "ImagerPluginCore/PluginManager.h"

void InitPlugin() {
    // Imager is starting up. Create all objects and perform all work
    // needed to start operation.

    PluginManager& manager = PluginManager::Manager();
}

void ShutdownPlugin() {
    // Imager is closing.
    // Perform any necessary cleanup here.
}
