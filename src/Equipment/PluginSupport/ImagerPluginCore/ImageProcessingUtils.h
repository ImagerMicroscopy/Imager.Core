#ifndef IMAGEPROCESSINGUTILS_H
#define IMAGEPROCESSINGUTILS_H

#include <cstddef>
#include <cstdint>
#include <memory>
#include <vector>
#include <cstring>
#include <type_traits>
#include <limits>
#include <stdexcept>
#include <string>

enum class ImageProcessingTypes;

class ImageProcessingDescriptor {
public:
    virtual ~ImageProcessingDescriptor() {;}
    virtual ImageProcessingTypes getType() const = 0;
};

class IPDRotateCW : public ImageProcessingDescriptor {
public:
    ImageProcessingTypes getType() const override;
};

class IPDRotateCCW : public ImageProcessingDescriptor {
public:
    ImageProcessingTypes getType() const override;
};

class IPDFlipHorizontal : public ImageProcessingDescriptor {
public:
    ImageProcessingTypes getType() const override;
};

class IPDFlipVertical : public ImageProcessingDescriptor {
public:
    ImageProcessingTypes getType() const override;
};

class IPDBin : public ImageProcessingDescriptor {
public:
    IPDBin(int binFactorRHS) : binFactor(binFactorRHS) {}
    ImageProcessingTypes getType() const override;
    int binFactor;
};

class IPDCrop : public ImageProcessingDescriptor {
public:
    IPDCrop(size_t nRowsRHS, size_t nColsRHS) : nRows(nRowsRHS), nCols(nColsRHS) {}
    ImageProcessingTypes getType() const override;
    size_t nRows, nCols;
};

class AcquiredImage; // Forward declaration

AcquiredImage ProcessImage(const AcquiredImage& inputImage, const std::vector<std::shared_ptr<ImageProcessingDescriptor>>& processingDescriptors);

template <typename T>
void RotateCW(const T* image, size_t nRows, size_t nCols, T* rotatedImage) {
    // Assuming column-major storage: index = col * nRows + row
    for (size_t c = 0; c < nCols; ++c) {
        for (size_t r = 0; r < nRows; ++r) {
            size_t new_r = c;
            size_t new_c = nRows - 1 - r;
            rotatedImage[new_c * nCols + new_r] = image[c * nRows + r];
        }
    }
}

template <typename T>
void RotateCCW(const T* image, size_t nRows, size_t nCols, T* rotatedImage) {
    // Assuming column-major storage: index = col * nRows + row
    for (size_t c = 0; c < nCols; ++c) {
        for (size_t r = 0; r < nRows; ++r) {
            size_t new_r = nCols - 1 - c;
            size_t new_c = r;
            rotatedImage[new_c * nCols + new_r] = image[c * nRows + r];
        }
    }
}

template <typename T>
void FlipHorizontal(const T* image, size_t nRows, size_t nCols, T* flippedImage) {
    for (size_t c = 0; c < nCols; ++c) {
        size_t flipped_c = nCols - 1 - c;
        for (size_t r = 0; r < nRows; ++r) {
            flippedImage[flipped_c * nRows + r] = image[c * nRows + r];
        }
    }
}

template <typename T>
void FlipVertical(const T* image, size_t nRows, size_t nCols, T* flippedImage) {
    for (size_t c = 0; c < nCols; ++c) {
        for (size_t r = 0; r < nRows; ++r) {
            size_t flipped_r = nRows - 1 - r;
            flippedImage[c * nRows + flipped_r] = image[c * nRows + r];
        }
    }
}

template <typename T>
void CropImage(const T* image, size_t nRows, size_t nCols, size_t outputNRows, size_t outputNCols, T* croppedImage) {
    size_t rowOffset = (nRows - outputNRows) / 2;
    size_t colOffset = (nCols - outputNCols) / 2;
    
    for (size_t c = 0; c < outputNCols; ++c) {
        for (size_t r = 0; r < outputNRows; ++r) {
            croppedImage[c * outputNRows + r] = image[(c + colOffset) * nRows + (r + rowOffset)];
        }
    }
}

template <typename T>
void BinImage(const T* image, size_t nRows, size_t nCols, T* binnedImage, const int binFactor) {
    if (binFactor > 32) {
        throw std::logic_error(std::string("unsupported binning factor"));
    }
    if (binFactor == 1) {
        std::memcpy(binnedImage, image, nRows * nCols * sizeof(T));
        return;
    }

    int nRowsOutput = nRows / binFactor;
    int nColsOutput = nCols / binFactor;

    const T* inputPtr = image;
    T* outputPtr = binnedImage;

    using AccumType = std::conditional_t<std::is_floating_point_v<T>, double, std::uint32_t>;
    AccumType maxVal = static_cast<AccumType>(std::numeric_limits<T>::max());

    for (int col = 0; col < nColsOutput; col += 1) {
        for (int row = 0; row < nRowsOutput; row += 1) {
            AccumType accum = 0;
            for (int bCol = 0; bCol < binFactor; bCol += 1) {
                for (int bRow = 0; bRow < binFactor; bRow += 1) {
                    accum += static_cast<AccumType>(*(inputPtr + bRow + bCol * nRows));
                }
            }
            if constexpr (std::is_integral_v<T>) {
                *outputPtr = static_cast<T>((accum > maxVal) ? maxVal : accum);
            } else {
                *outputPtr = static_cast<T>(accum);
            }
            outputPtr += 1;
            inputPtr += binFactor;
        }
        inputPtr += nRows * (binFactor - 1);
    }
}

#endif
