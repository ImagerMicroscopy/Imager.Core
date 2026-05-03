#include "ImageProcessingUtils.h"

#include <algorithm>
#include <cstring>
#include <limits>
#include <string>
#include <stdexcept>

#include "CameraUtils.h"

enum class ImageProcessingTypes {
    kRotateCW,
    kRotateCCW,
    kFlipHorizontal,
    kFlipVertical,
    kCrop,
    kBin
};

ImageProcessingTypes IPDRotateCW::getType() const {
    return ImageProcessingTypes::kRotateCW;
}

ImageProcessingTypes IPDRotateCCW::getType() const {
    return ImageProcessingTypes::kRotateCCW;
}

ImageProcessingTypes IPDFlipHorizontal::getType() const {
    return ImageProcessingTypes::kFlipHorizontal;
}

ImageProcessingTypes IPDFlipVertical::getType() const {
    return ImageProcessingTypes::kFlipVertical;
}

ImageProcessingTypes IPDBin::getType() const {
    return ImageProcessingTypes::kBin;
}

ImageProcessingTypes IPDCrop::getType() const {
    return ImageProcessingTypes::kCrop;
}

AcquiredImage DoProcessingStep(std::shared_ptr<ImageProcessingDescriptor> descriptor, const AcquiredImage& inputImage);

AcquiredImage ProcessImage(const AcquiredImage& inputImage, const std::vector<std::shared_ptr<ImageProcessingDescriptor>> & processingDescriptors) {
    if (processingDescriptors.empty()) {
        return inputImage;
    }

    AcquiredImage currentImage = inputImage;

    for (const auto& pd : processingDescriptors) {
        currentImage = DoProcessingStep(pd, currentImage);
    }
    
    return currentImage;
}

template <typename Func>
decltype(auto) DispatchPixelType(AcquiredImage::PixelFormat format, Func&& func) {
    switch (format) {
        case AcquiredImage::PixelFormat::Mono8:
            return std::forward<Func>(func)(static_cast<std::uint8_t*>(nullptr));
        case AcquiredImage::PixelFormat::Mono16:
            return std::forward<Func>(func)(static_cast<std::uint16_t*>(nullptr));
        case AcquiredImage::PixelFormat::Float64:
            return std::forward<Func>(func)(static_cast<double*>(nullptr));
    }
    throw std::logic_error("unsupported pixel format");
}

AcquiredImage DoProcessingStep(std::shared_ptr<ImageProcessingDescriptor> descriptor, const AcquiredImage& inputImage) {
    return DispatchPixelType(inputImage.getPixelFormat(), [&](auto dummy_ptr) {
        using PixelType = std::remove_pointer_t<decltype(dummy_ptr)>;
        
        ImageProcessingTypes processingType = descriptor->getType();
        AcquiredImage::PixelFormat pixelFormat = inputImage.getPixelFormat();
        int nRowsInput = inputImage.getNRows();
        int nColsInput = inputImage.getNCols();
        double timestamp = inputImage.getTimestamp();
        int nRowsOutput;
        int nColsOutput;

        const PixelType* inputPtr = reinterpret_cast<const PixelType*>(inputImage.getData().get());

        switch (processingType) {
            case ImageProcessingTypes::kRotateCW:
            case ImageProcessingTypes::kRotateCCW:
            {
                nRowsOutput = nColsInput;
                nColsOutput = nRowsInput;
                AcquiredImage outputImage = NewRecycledImage(pixelFormat, nRowsOutput, nColsOutput, timestamp);
                PixelType* outputPtr = reinterpret_cast<PixelType*>(outputImage.getData().get());
                if (processingType == ImageProcessingTypes::kRotateCW) {
                    RotateCW(inputPtr, nRowsInput, nColsInput, outputPtr);
                } else {
                    RotateCCW(inputPtr, nRowsInput, nColsInput, outputPtr);
                }
                return outputImage;
            }
            case ImageProcessingTypes::kFlipHorizontal:
            case ImageProcessingTypes::kFlipVertical:
            {
                nRowsOutput = nRowsInput;
                nColsOutput = nColsInput;
                AcquiredImage outputImage = NewRecycledImage(pixelFormat, nRowsOutput, nColsOutput, timestamp);
                PixelType* outputPtr = reinterpret_cast<PixelType*>(outputImage.getData().get());
                if (processingType == ImageProcessingTypes::kFlipHorizontal) {      
                    FlipHorizontal(inputPtr, nRowsInput, nColsInput, outputPtr);
                } else {
                    FlipVertical(inputPtr, nRowsInput, nColsInput, outputPtr);
                }
                return outputImage;
            }
            case ImageProcessingTypes::kCrop:
            {
                IPDCrop* cropObj = reinterpret_cast<IPDCrop*>(descriptor.get());
                nRowsOutput = cropObj->nRows;
                nColsOutput = cropObj->nCols;
                AcquiredImage outputImage = NewRecycledImage(pixelFormat, nRowsOutput, nColsOutput, timestamp);
                PixelType* outputPtr = reinterpret_cast<PixelType*>(outputImage.getData().get());
                CropImage(inputPtr, nRowsInput, nColsInput, nRowsOutput, nColsOutput, outputPtr);
                return outputImage;
            }
            case ImageProcessingTypes::kBin:
            {
                IPDBin* binObj = reinterpret_cast<IPDBin*>(descriptor.get());
                int binFactor = binObj->binFactor;
                nRowsOutput = nRowsInput / binFactor;
                nColsOutput = nColsInput / binFactor;
                AcquiredImage outputImage = NewRecycledImage(pixelFormat, nRowsOutput, nColsOutput, timestamp);
                PixelType* outputPtr = reinterpret_cast<PixelType*>(outputImage.getData().get());
                BinImage(inputPtr, nRowsInput, nColsInput, outputPtr, binFactor);
                return outputImage;
            }
            default:
                throw std::logic_error("no processing in _doProcessingStep()");
        }
    });
}
