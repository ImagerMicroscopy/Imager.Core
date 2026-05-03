#ifndef CAMERAUTILS_H
#define CAMERAUTILS_H

#include <cstdint>
#include <functional>
#include <memory>
#include <string>
#include <mutex>
#include <utility>

class AcquiredImage {
public:
    enum PixelFormat {
        Mono8 = 2,
        Mono16 = 0,
        Float64 = 1
    };

    AcquiredImage();
    AcquiredImage(PixelFormat pixelFormat, int nRows, int nCols, double timestamp, std::shared_ptr<std::uint8_t[]> data);
    AcquiredImage(PixelFormat pixelFormat, int nRows, int nCols, double timestamp);
    ~AcquiredImage() = default;

    std::shared_ptr<std::uint8_t[]> getData() const { return _data; }
    PixelFormat getPixelFormat() const { return _pixelFormat; }
    int getNRows() const { return _nRows; }
    int getNCols() const { return _nCols; }
    double getTimestamp() const { return _timestamp; }
    void setTimestamp(double timestamp) { _timestamp = timestamp; }

    static size_t BytesPerPixelForPixelFormat(PixelFormat pixelFormat);

private:

    std::shared_ptr<std::uint8_t[]> _data;
    int _nRows = 0;
    int _nCols = 0;
    double _timestamp = 0.0;
    PixelFormat _pixelFormat = PixelFormat::Mono16;
};

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

AcquiredImage NewRecycledImage(AcquiredImage::PixelFormat pixelFormat, int nRows, int nCols, double timestamp = 0.0);

#endif // CAMERAUTILS_H
