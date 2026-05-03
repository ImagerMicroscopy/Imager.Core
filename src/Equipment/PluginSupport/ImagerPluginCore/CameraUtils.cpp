#include "CameraUtils.h"

#include <concurrentqueue.h>
#include <unordered_map>
#include <mutex>

#include <cwchar>

#if defined(_WIN64)
    #include "Windows.h"
#endif

size_t AcquiredImage::BytesPerPixelForPixelFormat(PixelFormat pixelFormat) {
    switch (pixelFormat) {
        case Mono8:
            return 1;
        case Mono16:
            return 2;
        case Float64:
            return 8;
    }
    throw std::invalid_argument("unknown pixel format");
}

AcquiredImage::AcquiredImage() {
    _data.reset(new std::uint8_t[0], std::default_delete<std::uint8_t[]>());
}

AcquiredImage::AcquiredImage(PixelFormat pixelFormat, int nRows, int nCols, double timestamp, std::shared_ptr<std::uint8_t[]> data) {
    _pixelFormat = pixelFormat;
    _nRows = nRows;
    _nCols = nCols;
    _timestamp = timestamp;
    _data = std::move(data);
}

AcquiredImage::AcquiredImage(PixelFormat pixelFormat, int nRows, int nCols, double timestamp) {
    _pixelFormat = pixelFormat;
    _nRows = nRows;
    _nCols = nCols;
    _timestamp = timestamp;
    _data.reset(new std::uint8_t[nRows * nCols * BytesPerPixelForPixelFormat(pixelFormat)], std::default_delete<std::uint8_t[]>());
}

class ImageRecycler {
public:
    ImageRecycler() = default;

    ~ImageRecycler() {
        std::lock_guard<std::mutex> lock(_mutex);
        for (auto& [key, queue] : _pools) {
            std::uint8_t* ptr = nullptr;
            // Drain the queue and free the memory
            while (queue->try_dequeue(ptr)) {
                delete[] ptr;
            }
            delete queue;
        }
    }

    std::shared_ptr<std::uint8_t[]> newRecycledImage(size_t sizeInBytes) {
        auto* q = getQueueForSize(sizeInBytes);
        std::uint8_t* ptr = nullptr;

        // Try to pull an existing un-used buffer from the lock-free queue
        if (!q->try_dequeue(ptr)) {
            // If the queue is empty, allocate a new buffer
            ptr = new std::uint8_t[sizeInBytes];
        }

        // Define a maximum number of idle buffers to hold onto per dimension
        // (e.g., 10 frames = ~80MB for typical 2048x2048 16-bit cameras)
        constexpr size_t MAX_IDLE_BUFFERS = 10;

        // Return a shared_ptr with a lambda deleter
        return std::shared_ptr<std::uint8_t[]>(ptr, [q](std::uint8_t* p) {
            // size_approx() is fast and lock-free. It may not be perfect under extreme thread contention, 
            // but it is an excellent heuristic for high-water marks.
            if (q->size_approx() < MAX_IDLE_BUFFERS) {
                q->enqueue(p); // Keep it in the pool
            } else {
                delete[] p;    // Pool is full, return memory to the OS
            }
        });
    }

private:
    moodycamel::ConcurrentQueue<std::uint8_t*>* getQueueForSize(size_t sizeInBytes) {
        std::lock_guard<std::mutex> lock(_mutex);
        auto it = _pools.find(sizeInBytes);
        if (it == _pools.end()) {
            auto* newQueue = new moodycamel::ConcurrentQueue<std::uint8_t*>();
            _pools[sizeInBytes] = newQueue;
            return newQueue;
        }
        return it->second;
    }

    std::mutex _mutex; 
    std::unordered_map<size_t, moodycamel::ConcurrentQueue<std::uint8_t*>*> _pools;
};

// Global static instance
static ImageRecycler gImageRecycler;

AcquiredImage NewRecycledImage(AcquiredImage::PixelFormat pixelFormat, int nRows, int nCols, double timestamp) {
    // Determine the true size of the underlying buffer
    size_t sizeInBytes = nRows * nCols * AcquiredImage::BytesPerPixelForPixelFormat(pixelFormat);
    auto data = gImageRecycler.newRecycledImage(sizeInBytes);
    return AcquiredImage(pixelFormat, nRows, nCols, timestamp, data);
}

void AtomicString::set(const std::string& val) {
    std::lock_guard<std::mutex> lock(_mutex);
    _theString = val;
}
std::string AtomicString::get() {
    std::lock_guard<std::mutex> lock(_mutex);
    return _theString;
}
void AtomicString::clear() {
    std::lock_guard<std::mutex> lock(_mutex);
    _theString.clear();
}

bool AtomicString::empty() {
    std::lock_guard<std::mutex> lock(_mutex);
    return _theString.empty();
}

std::string wcharStringToUtf8(const std::wstring& str) {
    size_t strlen = wcslen(str.c_str());

    #if defined(_WIN64)
        int outLength = WideCharToMultiByte(CP_UTF8, 0, str.c_str(), str.size(), 0, 0, nullptr, nullptr);
        if (outLength < 0) {
            throw std::runtime_error("can't convert wide string to utf8");
        }
    
        std::string utf8Encoded(outLength, 0);
        outLength = WideCharToMultiByte(CP_UTF8, 0, str.c_str(), strlen, utf8Encoded.data(), outLength, nullptr, nullptr);
        if (outLength < 0) {
            throw std::runtime_error("can't convert wide string to utf8");
        }

        return utf8Encoded;
    #else
        throw std::logic_error("wcharStringToUtf8() not implemented");
    #endif
}

std::wstring utf8StringToWChar(const std::string& str) {
    #if defined(_WIN64)
        int convertResult = MultiByteToWideChar(CP_UTF8, 0, str.c_str(), str.size(), NULL, 0);
        if (convertResult < 0) {
            throw std::runtime_error("can't convert utf8 to wide string");
        }

        std::wstring wstring(convertResult, '0');
        convertResult = MultiByteToWideChar(CP_UTF8, 0, str.c_str(), str.size(), wstring.data(), wstring.size());
        if (convertResult <= 0) {
            throw std::runtime_error("can't convert utf8 to wide string");
        }

        return wstring;
    #else
        throw std::logic_error("wcharStringToUtf8() not implemented");
    #endif
}
