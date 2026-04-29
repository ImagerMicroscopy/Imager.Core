#include "ConfigManager.h"
#include <fstream>
#include <iostream>

#include <toml++/toml.hpp>

class ConfigManager::Impl {
public:
    toml::table table;
};

ConfigManager::ConfigManager(const std::filesystem::path& configFilePath)
    : _configFilePath(configFilePath),
      _impl(std::make_unique<Impl>()) {

    if (std::filesystem::exists(_configFilePath)) {
        try {
            _impl->table = toml::parse_file(_configFilePath.string());
        } catch (const toml::parse_error& err) {
            std::cerr << "Parsing failed: " << err << "\n";
            // Handled as empty config, or you could throw an exception
        }
    }
}

ConfigManager::~ConfigManager() {
    save();
}

void ConfigManager::save() {
    if (_configFilePath.empty()) return;

    std::ofstream outStream(_configFilePath);
    
    // toml++ knows how to serialize itself to std::ostream naturally
    outStream << _impl->table;
}

void ConfigManager::storeSetting(const ConfigPath& configPath, const std::string& value) {
    const auto& keys = configPath.getKeys();
    if (keys.empty()) return;

    toml::table* currentTable = &(_impl->table);

    // Navigate to the correct nested table, creating any missing sections on the fly
    for (size_t i = 0; i < keys.size() - 1; ++i) {
        const auto& key = keys[i];
        
        // If the table doesn't exist at this key, insert a new one
        if (!currentTable->contains(key) || !currentTable->at(key).is_table()) {
            currentTable->insert_or_assign(key, toml::table{});
        }
        
        currentTable = currentTable->at(key).as_table();
    }

    // Now insert the value at the final key location
    const std::string& finalKey = keys.back();
    
    // Insert or replace the value
    currentTable->insert_or_assign(finalKey, value);
}

std::pair<std::string, bool> ConfigManager::getSettingOrDefault(const ConfigPath& configPath, const std::string& defaultValue) {
    const auto& keys = configPath.getKeys();
    if (keys.empty()) {
        return {defaultValue, false};
    }

    toml::table* root = &(_impl->table);
    toml::node* currentNode = root;

    for (const auto& key : keys) {
        if (!currentNode || !currentNode->is_table()) {
            currentNode = nullptr;
            break;
        }
        currentNode = currentNode->as_table()->get(key);
    }

    if (currentNode) {
        if (currentNode->is_string()) {
            return {currentNode->as_string()->get(), true};
        } 
        
        // Attempt to extract the value as a std::string if it's not directly a string (e.g. conversion)
        if (auto val = currentNode->value<std::string>()) {
            return {*val, true};
        }
    }

    // The key was not found or was of an incompatible type.
    // Store the default value so it gets saved.
    storeSetting(configPath, defaultValue);

    return {defaultValue, false};
}
