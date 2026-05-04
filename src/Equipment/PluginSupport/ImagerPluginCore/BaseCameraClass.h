#ifndef BASECAMERACLASS_H
#define BASECAMERACLASS_H

#include <atomic>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <future>
#include <limits>
#include <mutex>
#include <optional>
#include <string>
#include <thread>
#include <vector>

#include "blockingconcurrentqueue.h"

#include "CameraPropertiesEncoding.h"
#include "ImageProcessingUtils.h"
#include "CameraUtils.h"

class BaseCameraClass {
public:
    BaseCameraClass() = default;
    virtual ~BaseCameraClass();

    BaseCameraClass(const BaseCameraClass&) = delete;
    BaseCameraClass& operator=(const BaseCameraClass&) = delete;

    virtual std::string getIdentifierStr() = 0;

    std::vector<CameraProperty> getCameraProperties();
    void setCameraProperties(const std::vector<CameraProperty>& properties);

    virtual double getFrameRate() = 0;
    bool isConfiguredForHardwareTriggering();
    void setImageProcessingOps(const std::vector<std::shared_ptr<ImageProcessingDescriptor>> &ops);

    AcquiredImage acquireSingleImage();

    int startAsyncAcquisition(std::uint64_t nImagesToAcquire);
    bool isAsyncAcquisitionRunning() const;
    void abortAsyncAcquisitionIfRunning();
    AcquiredImage getOldestImageAsyncAcquired();
    std::optional<AcquiredImage> getOldestImageAsyncAcquiredWithTimeout(const std::uint32_t timeoutMillis);


private:
    virtual std::vector<CameraProperty> _derivedGetCameraProperties() = 0;
    virtual void _derivedSetCameraProperties(const std::vector<CameraProperty>& properties) = 0;

    virtual bool _derivedIsConfiguredForHardwareTriggering() { return false; }

    virtual AcquiredImage _derivedAcquireSingleImage();

    void _asyncAcquisitionWorker(std::uint64_t nImagesToAcquire, const std::shared_ptr<moodycamel::BlockingConcurrentQueue<int>>& startedNotificationQueue);
    void _clearAvailableImagesQueue();
    
    virtual void _derivedStartBoundedAsyncAcquisition(std::uint64_t nImagesToAcquire);
    virtual void _derivedAbortAsyncAcquisition();
    virtual std::optional<AcquiredImage> _waitForNewImageWithTimeout(int timeoutMillis);

    std::vector<std::shared_ptr<ImageProcessingDescriptor>> _getImageProcessingDescriptors();
    virtual std::vector<std::shared_ptr<ImageProcessingDescriptor>> _derivedGetAdditionalImageProcessingDescriptors() { return {}; }
    void _imageProcessingWorker(const std::vector<std::shared_ptr<ImageProcessingDescriptor>> &processingDescriptors,
                                moodycamel::BlockingConcurrentQueue<AcquiredImage> &incomingImagesQueue,
                                moodycamel::BlockingConcurrentQueue<AcquiredImage>& outgoingImagesQueue,
                                AtomicString& errorString);

    std::vector<std::shared_ptr<ImageProcessingDescriptor>> _imageOrientationOps;

    std::chrono::steady_clock::time_point _acquisitionStartTimeStamp;
    AtomicString _asyncAcquisitionErrorStr;
    std::atomic_bool _asyncWantAbort = false;
    moodycamel::BlockingConcurrentQueue<AcquiredImage> _availableImagesQueue;
    std::future<void> _asyncAcquisitionWorkerFuture;
    std::mutex _abortWorkerMutex;

    moodycamel::BlockingConcurrentQueue<AcquiredImage> _asyncFromSingleImageAcquisitionQueue;
    std::atomic_bool _asyncFromSingleImageAcquisitionWantAbort = false;
    AtomicString _asyncFromSingleImageAcquisitionErrorStr;
    std::future<void> _asyncFromSingleImageAcquisitionFuture;
    std::mutex _abortSingleImageAcquisitionMutex;
};

#endif
