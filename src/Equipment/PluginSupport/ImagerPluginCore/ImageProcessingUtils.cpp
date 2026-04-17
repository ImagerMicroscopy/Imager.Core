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

AcquiredImage DoProcessingStep(std::shared_ptr<ImageProcessingDescriptor> descriptor, const AcquiredImage& inputImage) {
    ImageProcessingTypes processingType = descriptor->getType();
    size_t nRowsInput = inputImage.getNRows();
    size_t nColsInput = inputImage.getNCols();
    double timestamp = inputImage.getTimestamp();
    size_t nRowsOutput;
    size_t nColsOutput;

    switch (processingType) {
        case ImageProcessingTypes::kRotateCW:
        case ImageProcessingTypes::kRotateCCW:
        {
            nRowsOutput = nColsInput;
            nColsOutput = nRowsInput;
            AcquiredImage outputImage = NewRecycledImage((int)nRowsOutput, (int)nColsOutput, timestamp);
            if (processingType == ImageProcessingTypes::kRotateCW) {
                RotateCW(inputImage.getData().get(), nRowsInput, nColsInput, outputImage.getData().get());
            } else {
                RotateCCW(inputImage.getData().get(), nRowsInput, nColsInput, outputImage.getData().get());
            }
            return outputImage;
            break;
        }
        case ImageProcessingTypes::kFlipHorizontal:
        case ImageProcessingTypes::kFlipVertical:
        {
            nRowsOutput = nRowsInput;
            nColsOutput = nColsInput;
            AcquiredImage outputImage = NewRecycledImage((int)nRowsOutput, (int)nColsOutput, timestamp);
            if (processingType == ImageProcessingTypes::kFlipHorizontal) {      
                FlipHorizontal(inputImage.getData().get(), nRowsInput, nColsInput, outputImage.getData().get());
            } else {
                FlipVertical(inputImage.getData().get(), nRowsInput, nColsInput, outputImage.getData().get());
            }
            return outputImage;
            break;
        }
        case ImageProcessingTypes::kCrop:
        {
            IPDCrop* cropObj = reinterpret_cast<IPDCrop*>(descriptor.get());
            nRowsOutput = cropObj->nRows;
            nColsOutput = cropObj->nCols;
            AcquiredImage outputImage = NewRecycledImage((int)nRowsOutput, (int)nColsOutput, timestamp);
            CropImage(inputImage.getData().get(), nRowsInput, nColsInput, nRowsOutput, nColsOutput, outputImage.getData().get());
            return outputImage;
            break;
        }
        case ImageProcessingTypes::kBin:
        {
            IPDBin* binObj = reinterpret_cast<IPDBin*>(descriptor.get());
            int binFactor = binObj->binFactor;
            nRowsOutput = nRowsInput / binFactor;
            nColsOutput = nColsInput / binFactor;
            AcquiredImage outputImage = NewRecycledImage((int)nRowsOutput, (int)nColsOutput, timestamp);
            BinImage(inputImage.getData().get(), nRowsInput, nColsInput, outputImage.getData().get(), binFactor);
            return outputImage;
            break;
        }
        default:
            throw std::logic_error("no processing in _doProcessingStep()");
            break;
    }
}

void RotateCW(const std::uint16_t* image, size_t nRows, size_t nCols, std::uint16_t* rotatedImage) {
    // Assuming column-major storage: index = col * nRows + row
    for (size_t c = 0; c < nCols; ++c) {
        for (size_t r = 0; r < nRows; ++r) {
            size_t new_r = c;
            size_t new_c = nRows - 1 - r;
            rotatedImage[new_c * nCols + new_r] = image[c * nRows + r];
        }
    }
}

void RotateCCW(const std::uint16_t* image, size_t nRows, size_t nCols, std::uint16_t* rotatedImage) {
    // Assuming column-major storage: index = col * nRows + row
    for (size_t c = 0; c < nCols; ++c) {
        for (size_t r = 0; r < nRows; ++r) {
            size_t new_r = nCols - 1 - c;
            size_t new_c = r;
            rotatedImage[new_c * nCols + new_r] = image[c * nRows + r];
        }
    }
}

void FlipHorizontal(const std::uint16_t* image, size_t nRows, size_t nCols, std::uint16_t* flippedImage) {
    for (size_t c = 0; c < nCols; ++c) {
        size_t flipped_c = nCols - 1 - c;
        for (size_t r = 0; r < nRows; ++r) {
            flippedImage[flipped_c * nRows + r] = image[c * nRows + r];
        }
    }
}

void FlipVertical(const std::uint16_t* image, size_t nRows, size_t nCols, std::uint16_t* flippedImage) {
    for (size_t c = 0; c < nCols; ++c) {
        for (size_t r = 0; r < nRows; ++r) {
            size_t flipped_r = nRows - 1 - r;
            flippedImage[c * nRows + flipped_r] = image[c * nRows + r];
        }
    }
}

void CropImage(const std::uint16_t* image, size_t nRows, size_t nCols, size_t outputNRows, size_t outputNCols, std::uint16_t* croppedImage) {
    size_t rowOffset = (nRows - outputNRows) / 2;
    size_t colOffset = (nCols - outputNCols) / 2;
    
    for (size_t c = 0; c < outputNCols; ++c) {
        for (size_t r = 0; r < outputNRows; ++r) {
            croppedImage[c * outputNRows + r] = image[(c + colOffset) * nRows + (r + rowOffset)];
        }
    }
}

void BinImage(const std::uint16_t* image, size_t nRows, size_t nCols, std::uint16_t* binnedImage, const int binFactor) {
    if (binFactor > 32) {
        throw std::logic_error(std::string("unsupported binning factor"));
    }
    if (binFactor == 1) {
        memcpy(binnedImage, image, nRows * nCols * sizeof(std::uint16_t));
        return;
    }

    int nRowsOutput = nRows / binFactor;
    int nColsOutput = nCols / binFactor;
    int nPixelsOutput = nRowsOutput * nColsOutput;

    const std::uint16_t* inputPtr = image;
    std::uint16_t* outputPtr = binnedImage;

    for (int col = 0; col < nColsOutput; col += 1) {
        for (int row = 0; row < nRowsOutput; row += 1) {
            std::uint32_t accum = 0;
            for (int bCol = 0; bCol < binFactor; bCol += 1) {
                for (int bRow = 0; bRow < binFactor; bRow += 1) {
                    accum += *(inputPtr + bRow + bCol * nRows);
                }
            }
            *outputPtr = (accum > 65535) ? 65535 : *outputPtr;
            outputPtr += 1;
            inputPtr += binFactor;
        }
        inputPtr += nRows * (binFactor - 1);
    }
}
