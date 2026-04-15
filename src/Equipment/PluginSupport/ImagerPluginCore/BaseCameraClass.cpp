
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

std::tuple<std::shared_ptr<uint16_t>, int, int> BaseCameraClass::acquireSingleImage() {
    abortAsyncAcquisitionIfRunning();
    if (!_hasCustomAcquireSingleImage()) {
        std::shared_ptr<std::uint16_t> imageData;
        int nRows, nCols;
        double timeStamp;

        startAsyncAcquisition(AcqFillAndStop, 1);
        std::tie(imageData, nRows, nCols, timeStamp) = getOldestImageAsyncAcquired();
        abortAsyncAcquisitionIfRunning();

        return std::tuple<std::shared_ptr<uint16_t>, int, int>(imageData, nRows, nCols);
    } else {
        std::pair<int, int> imageSize = _getSizeOfRawImages();
        int nPixels = imageSize.first * imageSize.second;
        std::shared_ptr<std::uint16_t> imageData(new std::uint16_t[nPixels], [](const std::uint16_t* ptr) {delete[] ptr; });
        std::vector<std::shared_ptr<ImageProcessingDescriptor>> imageProcessingDescriptors = _getImageProcessingDescriptors();
        _derivedAcquireSingleImage(imageData.get(), nPixels * sizeof(std::uint16_t));

        size_t nOutputRows, nOutputCols;
        imageData = ProcessImage(imageSize.first, imageSize.second, imageData, imageProcessingDescriptors, nOutputRows, nOutputCols);
        return std::tuple<std::shared_ptr<uint16_t>, int, int>(imageData, (int)nOutputRows, (int)nOutputCols);
    }
}

int BaseCameraClass::startAsyncAcquisition(AcquisitionMode acqMode, std::uint64_t nImagesToAcquire) {
    _acquisitionStartTimeStamp = std::chrono::steady_clock::now();
    
    abortAsyncAcquisitionIfRunning();
    _asyncAcquisitionErrorStr.clear();
    _asyncWantAbort = false;
    _asyncNImagesStored = 0;
    _clearAvailableImagesQueue();
    std::shared_ptr<moodycamel::BlockingConcurrentQueue<int>> startedNotificationQueue(new moodycamel::BlockingConcurrentQueue<int>());

    _asyncWorkerFuture = std::async(std::launch::async, [=]() {
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
    if (!_asyncWorkerFuture.valid()) {
        return false;
    }
    std::future_status status = _asyncWorkerFuture.wait_for(std::chrono::seconds(0));
    return (status != std::future_status::ready);
}

void BaseCameraClass::abortAsyncAcquisitionIfRunning() {
    if (isAsyncAcquisitionRunning()) {
        _asyncWantAbort = true;
        _asyncWorkerFuture.wait();
        _asyncWorkerFuture.get();
    }
}

std::uint64_t BaseCameraClass::getNImagesAsyncAcquired() const {
    return _asyncNImagesStored;
}

std::tuple<std::shared_ptr<std::uint16_t>, int, int, double> BaseCameraClass::getOldestImageAsyncAcquired() {
    auto result = getOldestImageAsyncAcquiredWithTimeout(std::numeric_limits<uint32_t>::max());
    return result.value();
}

std::optional<std::tuple<std::shared_ptr<std::uint16_t>, int, int, double>> BaseCameraClass::getOldestImageAsyncAcquiredWithTimeout(const std::uint32_t timeoutMillis) {
    std::uint32_t maybeCorrectedTimeoutMillis = std::max(timeoutMillis, (std::uint32_t)1);

    std::chrono::time_point<std::chrono::high_resolution_clock> start(std::chrono::high_resolution_clock::now());
    std::chrono::time_point<std::chrono::high_resolution_clock> end = start + std::chrono::milliseconds(maybeCorrectedTimeoutMillis);
    std::chrono::milliseconds singleWaitDuration(std::min(maybeCorrectedTimeoutMillis, (std::uint32_t)250));

    std::tuple<std::shared_ptr<std::uint16_t>, int, int, double> imageData;

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
            return std::optional<std::tuple<std::shared_ptr<std::uint16_t>, int, int, double>>();
        }

        bool haveImage = _availableImagesQueue.wait_dequeue_timed(imageData, std::chrono::milliseconds(singleWaitDuration));
        if (haveImage) {
            return std::optional<std::tuple<std::shared_ptr<std::uint16_t>, int, int, double>>(imageData);
        }
    }
}

void BaseCameraClass::_asyncAcquisitionWorker(AcquisitionMode acqMode, std::uint64_t nImagesToAcquire, const std::shared_ptr<moodycamel::BlockingConcurrentQueue<int>>& startedNotificationQueue) {
    auto actualImageSize = _getSizeOfRawImages();

    try {
        std::vector<std::shared_ptr<ImageProcessingDescriptor>> imageProcessingDescriptors = _getImageProcessingDescriptors();

        moodycamel::BlockingConcurrentQueue<std::pair<std::shared_ptr<std::uint16_t>, double>> processingQueue;
        AtomicString _asyncProcessingErrorStr;
        _asyncProcessingErrorStr.clear();
        std::future<void> imageProcessingFuture = std::async(std::launch::async, [&]() {
            _imageProcessingWorker(actualImageSize.first, actualImageSize.second, imageProcessingDescriptors,
                                   processingQueue, _availableImagesQueue, _asyncProcessingErrorStr);
        });
        CleanupRunner ipRunner([&]() {
            processingQueue.enqueue(std::pair<std::shared_ptr<std::uint16_t>, double>());
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
            std::shared_ptr<std::uint16_t> theImage = NewRecycledImage(actualImageSize);
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
                    processingQueue.enqueue(std::pair<std::shared_ptr<std::uint16_t>, double>(theImage, acqTimeStamp));
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
    std::tuple<std::shared_ptr<std::uint16_t>, int, int, double> dummy;
    bool hadValue = false;
    do {
        hadValue = _availableImagesQueue.try_dequeue(dummy);
    } while (hadValue);
}

std::vector<std::shared_ptr<ImageProcessingDescriptor>> BaseCameraClass::_getImageProcessingDescriptors() {
    std::vector<std::shared_ptr<ImageProcessingDescriptor>> imageProcessingDescriptors;
    imageProcessingDescriptors = _derivedGetAdditionalImageProcessingDescriptors();
    for (const auto& pd : _imageOrientationOps) {
        imageProcessingDescriptors.push_back(pd);
    }
    return imageProcessingDescriptors;
}

void BaseCameraClass::_imageProcessingWorker(const size_t nRows, const size_t nCols, const std::vector<std::shared_ptr<ImageProcessingDescriptor>> &
                                             processingDescriptors,
                                             moodycamel::BlockingConcurrentQueue<std::pair<std::shared_ptr<std::uint16_t>, double>>& incomingImagesQueue,
                                             moodycamel::BlockingConcurrentQueue<std::tuple<std::shared_ptr<std::uint16_t>, int, int, double>>& outgoingImagesQueue,
                                             AtomicString& errorString) {
    try {
        std::shared_ptr<std::uint16_t> inputImage;
        double timeStamp;
        std::pair<std::shared_ptr<std::uint16_t>, double> queuedData;

        for (; ; ) {
            incomingImagesQueue.wait_dequeue(queuedData);
            std::tie(inputImage, timeStamp) = queuedData;
            if (inputImage == nullptr) {
                return;
            }

            size_t nInputRows = nRows, nInputCols = nCols;
            size_t nOutputRows = nRows, nOutputCols = nCols;
            std::shared_ptr<std::uint16_t> outputImage = ProcessImage(nInputRows, nInputCols, inputImage, processingDescriptors, nOutputRows, nOutputCols);
            std::tuple<std::shared_ptr<std::uint16_t>, int, int, double> imageData(outputImage, nOutputRows, nOutputCols, timeStamp);
            outgoingImagesQueue.enqueue(imageData);
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
