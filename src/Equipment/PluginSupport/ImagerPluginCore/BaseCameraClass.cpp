
#include "BaseCameraClass.h"

template <typename T>
class ScopedSetter {
public:
    ScopedSetter(T* valToModify, T value) :
        _valToModify(valToModify),
        _value(value) {
    }
    ~ScopedSetter() {
        *_valToModify = _value;
    }
private:
    T* _valToModify;
    T _value;
};

BaseCameraClass::~BaseCameraClass() {
    abortAsyncAcquisitionIfRunning();
}

std::vector<CameraProperty> BaseCameraClass::getCameraProperties() {
    return _derivedGetCameraProperties();
}

void BaseCameraClass::setCameraProperties(const std::vector<CameraProperty>& properties) {
    //if (isAsyncAcquisitionRunning()) {
    //	throw std::runtime_error("BaseCameraClass::setCameraProperties() but acquisition running");
    //}
    _derivedSetCameraProperties(properties);
}

bool BaseCameraClass::isConfiguredForHardwareTriggering() {
    return _derivedIsConfiguredForHardwareTriggering();
}

void BaseCameraClass::setImageOrientationOps(const std::vector<std::shared_ptr<ImageProcessingDescriptor>> &ops) {
    _imageOrientationOps = ops;
}

AcquiredImage BaseCameraClass::acquireSingleImage() {
    abortAsyncAcquisitionIfRunning();
    AcquiredImage acquiredImage = _derivedAcquireSingleImage();
    std::vector<std::shared_ptr<ImageProcessingDescriptor>> imageProcessingDescriptors = _getImageProcessingDescriptors();
    return ProcessImage(acquiredImage, imageProcessingDescriptors);
}

int BaseCameraClass::startAsyncAcquisition(AcquisitionMode acqMode, std::uint64_t nImagesToAcquire) {
    _acquisitionStartTimeStamp = std::chrono::steady_clock::now();
    
    abortAsyncAcquisitionIfRunning();
    _asyncAcquisitionErrorStr.clear();
    _asyncWantAbort = false;
    _asyncNImagesStored = 0;
    _clearAvailableImagesQueue();
    std::shared_ptr<moodycamel::BlockingConcurrentQueue<int>> startedNotificationQueue(new moodycamel::BlockingConcurrentQueue<int>());

    _asyncAcquisitionWorkerFuture = std::async(std::launch::async, [=]() {
        _asyncAcquisitionWorker(acqMode, nImagesToAcquire, startedNotificationQueue);
    });

    int dummy = -1;
    bool hadValue = startedNotificationQueue->wait_dequeue_timed(dummy, std::chrono::seconds(5));	// wait until acquisition has really started
    if (!hadValue) {
        throw std::runtime_error("Waiting excessively long on camera acquisition start");
    }

    return 0;
}

bool BaseCameraClass::isAsyncAcquisitionRunning() const {
    if (!_asyncAcquisitionWorkerFuture.valid()) {
        return false;
    }
    std::future_status status = _asyncAcquisitionWorkerFuture.wait_for(std::chrono::seconds(0));
    return (status != std::future_status::ready);
}

void BaseCameraClass::abortAsyncAcquisitionIfRunning() {
    if (isAsyncAcquisitionRunning()) {
        _asyncWantAbort = true;
        _asyncAcquisitionWorkerFuture.wait();
        _asyncAcquisitionWorkerFuture.get();
    }
}

std::uint64_t BaseCameraClass::getNImagesAsyncAcquired() const {
    return _asyncNImagesStored;
}

AcquiredImage BaseCameraClass::getOldestImageAsyncAcquired() {
    auto result = getOldestImageAsyncAcquiredWithTimeout(std::numeric_limits<uint32_t>::max());
    return std::move(result.value());
}

std::optional<AcquiredImage> BaseCameraClass::getOldestImageAsyncAcquiredWithTimeout(const std::uint32_t timeoutMillis) {
    std::uint32_t maybeCorrectedTimeoutMillis = std::max(timeoutMillis, (std::uint32_t)1);

    std::chrono::time_point<std::chrono::high_resolution_clock> start(std::chrono::high_resolution_clock::now());
    std::chrono::time_point<std::chrono::high_resolution_clock> end = start + std::chrono::milliseconds(maybeCorrectedTimeoutMillis);
    std::chrono::milliseconds singleWaitDuration(std::min(maybeCorrectedTimeoutMillis, (std::uint32_t)250));

   AcquiredImage imageData;

    for ( ; ; ) {
        if (!isAsyncAcquisitionRunning()) {
            if (!_asyncAcquisitionErrorStr.empty()) {
                throw std::runtime_error(std::string("async worker found error: ") + _asyncAcquisitionErrorStr.get());
            } else {
                throw std::runtime_error("waiting for new image but no acquisition running");
            }
        }

        if (std::chrono::high_resolution_clock::now() > end) {
            // timeout
            return std::optional<AcquiredImage>();
        }

        bool haveImage = _availableImagesQueue.wait_dequeue_timed(imageData, std::chrono::milliseconds(singleWaitDuration));
        if (haveImage) {
            return std::optional<AcquiredImage>(imageData);
        }
    }
}

AcquiredImage BaseCameraClass::_derivedAcquireSingleImage() {
    if (isAsyncAcquisitionRunning()) {
        throw std::logic_error("Camera plugin implemented neither async nor single acquisition modes!");
    }
    startAsyncAcquisition(AcqFillAndStop, 1);
    AcquiredImage acquiredImage = getOldestImageAsyncAcquired();
    abortAsyncAcquisitionIfRunning();
    return acquiredImage;
}

void BaseCameraClass::_asyncAcquisitionWorker(AcquisitionMode acqMode, std::uint64_t nImagesToAcquire, const std::shared_ptr<moodycamel::BlockingConcurrentQueue<int>>& startedNotificationQueue) {
    auto actualImageSize = _getSizeOfRawImages();

    try {
        std::vector<std::shared_ptr<ImageProcessingDescriptor>> imageProcessingDescriptors = _getImageProcessingDescriptors();

        moodycamel::BlockingConcurrentQueue<AcquiredImage> processingQueue;
        AtomicString _asyncProcessingErrorStr;
        _asyncProcessingErrorStr.clear();
        std::future<void> imageProcessingFuture = std::async(std::launch::async, [&]() {
            _imageProcessingWorker(imageProcessingDescriptors,
                                   processingQueue, _availableImagesQueue, _asyncProcessingErrorStr);
        });
        CleanupRunner ipRunner([&]() {
            processingQueue.enqueue(AcquiredImage());
            imageProcessingFuture.wait();
            imageProcessingFuture.get();
        });

        if (_derivedHaveBoundedAsyncAcquisition()) {
            _derivedStartBoundedAsyncAcquisition(nImagesToAcquire);
        } else {
            _derivedStartUnboundedAsyncAcquisition();
        }
        CleanupRunner runner([&]() {
            this->_derivedAbortAsyncAcquisition();
        });

        startedNotificationQueue->enqueue(0);

        for ( ; ;) {
            std::shared_ptr<std::uint16_t[]> theImage = NewRecycledImage(actualImageSize);
            for ( ; ; ) {
                if (_asyncWantAbort) {
                    return;
                }
                if (!_asyncProcessingErrorStr.empty()) {
                    _asyncAcquisitionErrorStr.set("_imageProcessingWorker had error:" + _asyncProcessingErrorStr.get());
                    return;
                }
                NewImageResult result = _waitForNewImageWithTimeout(250, theImage.get(), actualImageSize.first * actualImageSize.second * sizeof(std::uint16_t));
                if (result == NewImageCopied) {
                    auto duration = std::chrono::duration<double>(std::chrono::steady_clock::now() - _acquisitionStartTimeStamp);
                    double acqTimeStamp = duration.count();
                    processingQueue.enqueue(AcquiredImage(actualImageSize.first, actualImageSize.second, acqTimeStamp, theImage));
                    _asyncNImagesStored += 1;
                    
                    if ((acqMode == AcqFillAndStop) && (_asyncNImagesStored == nImagesToAcquire)) {
                        return;
                    }
                }
            }
        }
    }
    catch (const std::exception& e) {
        _asyncAcquisitionErrorStr.set(e.what());
        return;
    }
    catch (...) {
        _asyncAcquisitionErrorStr.set("unknown exception in _asyncAcquisitionWorker");
        return;
    }
}

void BaseCameraClass::_clearAvailableImagesQueue() {
    AcquiredImage dummy;
    while (_availableImagesQueue.try_dequeue(dummy)) {
        ;
    }
}

void BaseCameraClass::_derivedStartUnboundedAsyncAcquisition() {
    // default implementation based on acquireSingleImage()
    AcquiredImage dummy;
    while (_asyncFromSingleImageAcquisitionQueue.try_dequeue(dummy)) {
        ;
    }

    _asyncFromSingleImageAcquisitionWantAbort = false;
    _asyncFromSingleImageAcquisitionErrorStr.clear();
    _asyncFromSingleImageAcquisitionFuture = std::async(std::launch::async, [&]() {
        try {
            while (true) {
                if (_asyncFromSingleImageAcquisitionWantAbort) {
                    return;
                }
                AcquiredImage acquiredImage = _derivedAcquireSingleImage();
                _asyncFromSingleImageAcquisitionQueue.enqueue(acquiredImage);
            }
        }
        catch (const std::exception& e) {
            _asyncFromSingleImageAcquisitionErrorStr.set(e.what());
            return;
        }
        catch (...) {
            _asyncFromSingleImageAcquisitionErrorStr.set("unknown exception in default _derivedStartUnboundedAsyncAcquisition");
            return;
        }
    });
}

void BaseCameraClass::_derivedAbortAsyncAcquisition() {
    // default implementation based on acquireSingleImage()
    _asyncFromSingleImageAcquisitionWantAbort = true;
    if (_asyncFromSingleImageAcquisitionFuture.valid()) {
        _asyncFromSingleImageAcquisitionFuture.wait();
        _asyncFromSingleImageAcquisitionFuture.get();
    }
}

BaseCameraClass::NewImageResult BaseCameraClass::_waitForNewImageWithTimeout(int timeoutMillis, std::uint16_t* bufferForThisImage, int nBytes) {
    // default implementation based on acquireSingleImage()
    AcquiredImage acquiredImage;
    bool haveImage = _asyncFromSingleImageAcquisitionQueue.wait_dequeue_timed(acquiredImage, std::chrono::milliseconds(timeoutMillis));
    if (haveImage) {
        auto data = acquiredImage.getData();
        std::memcpy(bufferForThisImage, data.get(), nBytes);
        return NewImageCopied;
    } else {
        return NoImageBeforeTimeout;
    }
}

std::vector<std::shared_ptr<ImageProcessingDescriptor>> BaseCameraClass::_getImageProcessingDescriptors() {
    std::vector<std::shared_ptr<ImageProcessingDescriptor>> imageProcessingDescriptors;
    imageProcessingDescriptors = _derivedGetAdditionalImageProcessingDescriptors();
    for (const auto& pd : _imageOrientationOps) {
        imageProcessingDescriptors.push_back(pd);
    }
    return imageProcessingDescriptors;
}

void BaseCameraClass::_imageProcessingWorker(const std::vector<std::shared_ptr<ImageProcessingDescriptor>> &processingDescriptors,
                                             moodycamel::BlockingConcurrentQueue<AcquiredImage> &incomingImagesQueue,
                                             moodycamel::BlockingConcurrentQueue<AcquiredImage>& outgoingImagesQueue,
                                             AtomicString& errorString) {
    try {
        for (; ; ) {
            AcquiredImage inputImage;
            incomingImagesQueue.wait_dequeue(inputImage);
            if (inputImage.getData() == nullptr) {
                // Signals that this worker should stop.
                return;
            }

            AcquiredImage outputImage = ProcessImage(inputImage, processingDescriptors);
            outgoingImagesQueue.enqueue(outputImage);
        }
    }
    catch (std::exception& e) {
        errorString.set(e.what());
        return;
    }
    catch (...) {
        errorString.set("unknown error in _imageProcessingWorker()");
        return;
    }
}
