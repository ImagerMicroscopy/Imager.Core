#ifndef BASECAMERACLASS_H
#define BASECAMERACLASS_H

#include <string>
#include <vector>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <thread>
#include <mutex>
#include <future>
#include <optional>
#include <vector>

#include "blockingconcurrentqueue.h"

#include "CameraPropertiesEncoding.h"
#include "ImageProcessingUtils.h"
#include "CameraUtils.h"

class BaseCameraClass {
public:
    enum AcquisitionMode {
        AcqFreeRunMode,
        AcqFillAndStop
    };

    enum NewImageResult {
        NewImageCopied,
        NoImageBeforeTimeout
    };

    BaseCameraClass() : _asyncWantAbort(false), _asyncNImagesStored(0) { ; }

    virtual ~BaseCameraClass();

    virtual std::string getIdentifierStr() = 0;

    std::vector<CameraProperty> getCameraProperties();
    void setCameraProperties(const std::vector<CameraProperty>& properties);

    virtual double getFrameRate() = 0;
    bool isConfiguredForHardwareTriggering();
    virtual void setImageOrientationOps(const std::vector<std::shared_ptr<ImageProcessingDescriptor>> &ops);

    std::tuple<std::shared_ptr<uint16_t>, int, int> acquireSingleImage();

    int startAsyncAcquisition(AcquisitionMode acqMode, std::uint64_t nImagesToAcquire);
    bool isAsyncAcquisitionRunning() const;
    void abortAsyncAcquisitionIfRunning();
    std::uint64_t getNImagesAsyncAcquired() const;
    std::tuple<std::shared_ptr<std::uint16_t>, int, int, double> getOldestImageAsyncAcquired();
    std::optional<std::tuple<std::shared_ptr<std::uint16_t>, int, int, double>> getOldestImageAsyncAcquiredWithTimeout(const std::uint32_t timeoutMillis);


private:
    virtual std::vector<CameraProperty> _derivedGetCameraProperties() = 0;
    virtual void _derivedSetCameraProperties(const std::vector<CameraProperty>& properties) = 0;

    virtual bool _derivedIsConfiguredForHardwareTriggering() { return false; }

    virtual std::pair<int, int> _getSizeOfRawImages() = 0;
    virtual bool _hasCustomAcquireSingleImage() const { return false; }
    virtual void _derivedAcquireSingleImage(std::uint16_t* bufferForThisImage, int nBytes) { throw std::logic_error("custom single acquire but not implemented"); }

    void _asyncAcquisitionWorker(AcquisitionMode acqMode, std::uint64_t nImagesToAcquire, const std::shared_ptr<moodycamel::BlockingConcurrentQueue<int>>& startedNotificationQueue);
    void _clearAvailableImagesQueue();
    
    virtual void _derivedStartUnboundedAsyncAcquisition() = 0;
    virtual bool _derivedHaveBoundedAsyncAcquisition() {return false;}
    virtual void _derivedStartBoundedAsyncAcquisition(std::uint64_t nImagesToAcquire) {throw std::logic_error("_derivedStartBoundedAsyncAcquisition() not implemented");}
    virtual void _derivedAbortAsyncAcquisition() = 0;
    virtual NewImageResult _waitForNewImageWithTimeout(int timeoutMillis, std::uint16_t* bufferForThisImage, int nBytes) = 0;

    std::vector<std::shared_ptr<ImageProcessingDescriptor>> _getImageProcessingDescriptors();
    virtual std::vector<std::shared_ptr<ImageProcessingDescriptor>> _derivedGetAdditionalImageProcessingDescriptors() { return {}; }
    void _imageProcessingWorker(const size_t nRows, const size_t nCols, const std::vector<std::shared_ptr<ImageProcessingDescriptor>> &
                                processingDescriptors,
                                moodycamel::BlockingConcurrentQueue<std::pair<std::shared_ptr<std::uint16_t>, double>> &incomingImagesQueue,
                                moodycamel::BlockingConcurrentQueue<std::tuple<std::shared_ptr<std::uint16_t>, int, int, double>>& outgoingImagesQueue,
                                AtomicString& errorString);

    std::vector<std::shared_ptr<ImageProcessingDescriptor>> _imageOrientationOps;

    std::chrono::steady_clock::time_point _acquisitionStartTimeStamp;
    AtomicString _asyncAcquisitionErrorStr;
    volatile bool _asyncWantAbort;
    std::uint64_t _asyncNImagesStored;
    moodycamel::BlockingConcurrentQueue<std::tuple<std::shared_ptr<std::uint16_t>, int, int, double>> _availableImagesQueue;
    std::future<void> _asyncWorkerFuture;
};

#endif
