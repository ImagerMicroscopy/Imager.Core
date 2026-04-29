#ifndef CONFIGMANAGER_H
#define CONFIGMANAGER_H

#include <filesystem>
#include <string>
#include <vector>
#include <utility>

class ConfigPath {
public:
    // Allow implicit conversion from single strings
    ConfigPath(std::string key) : _keys{std::move(key)} {}
    ConfigPath(const char* key) : _keys{key} {}
    ConfigPath() = default;

    // Append operator
    ConfigPath& operator/=(const ConfigPath& rhs) {
        _keys.insert(_keys.end(), rhs._keys.begin(), rhs._keys.end());
        return *this;
    }

    const std::vector<std::string>& getKeys() const { return _keys; }

private:
    std::vector<std::string> _keys;
};

// Global operator/ allows joining paths safely
inline ConfigPath operator/(ConfigPath lhs, const ConfigPath& rhs) {
    lhs /= rhs;
    return lhs;
}

class ConfigManager {
public:
    ConfigManager(const std::filesystem::path& configFilePath);
    ~ConfigManager(); // Will save the config on destruction, or manually
    ConfigManager(const ConfigManager&) = delete;
    ConfigManager& operator=(const ConfigManager&) = delete;

    void storeSetting(const ConfigPath& configPath, const std::string& value);
    std::pair<std::string, bool> getSettingOrDefault(const ConfigPath& configPath, const std::string& defaultValue);

    void save(); // Trigger a manual save

private:
    std::filesystem::path _configFilePath;

    class Impl;    // forward declaration of a custom implementation so we don't need to include toml headers.
    std::unique_ptr<Impl> _impl;
};

#endif // CONFIGMANAGER_H
