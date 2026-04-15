#ifndef CAMERAUTILS_H
#define CAMERAUTILS_H

#include <cstdint>
#include <functional>
#include <memory>
#include <string>
#include <mutex>
#include <utility>

template <typename T>
T clamp(const T& a, const T& min, const T& max) {
        return std::min(max, std::max(a, min));
}

class CleanupRunner {
public:
    CleanupRunner(std::function<void()> func) : _func(func) {}
    ~CleanupRunner() { _func(); }

private:
    std::function<void()> _func;
};

class AtomicString {
public:
    AtomicString() {;}

    void set(const std::string& val);
    std::string get();
    void clear();
    bool empty();
private:
    std::string _theString;
    std::mutex _mutex;
};

std::string wcharStringToUtf8(const std::wstring& str);
std::wstring utf8StringToWChar(const std::string& str);

std::shared_ptr<std::uint16_t> NewRecycledImage(std::pair<size_t, size_t> size);

#endif // CAMERAUTILS_H
