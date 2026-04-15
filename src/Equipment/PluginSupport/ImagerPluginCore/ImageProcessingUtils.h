#ifndef IMAGEPROCESSINGUTILS_H
#define IMAGEPROCESSINGUTILS_H

#include <cstddef>
#include <cstdint>
#include <memory>
#include <vector>

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

std::shared_ptr<std::uint16_t> ProcessImage(const size_t nRows, const size_t nCols, std::shared_ptr<std::uint16_t> inputImage,
                                            const std::vector<std::shared_ptr<ImageProcessingDescriptor>> &
                                            processingDescriptors,
                                            size_t& nOutputRows, size_t& nOutputCols);

void RotateCW(const std::uint16_t* image, size_t nRows, size_t nCols, std::uint16_t* rotatedImage);
void RotateCCW(const std::uint16_t* image, size_t nRows, size_t nCols, std::uint16_t* rotatedImage);

void FlipHorizontal(const std::uint16_t* image, size_t nRows, size_t nCols, std::uint16_t* flippedImage);
void FlipVertical(const std::uint16_t* image, size_t nRows, size_t nCols, std::uint16_t* flippedImage);

void CropImage(const std::uint16_t* image, size_t nRows, size_t nCols, size_t outputNRows, size_t outputNCols, std::uint16_t* croppedImage);
void BinImage(const std::uint16_t* image, size_t nRows, size_t nCols, std::uint16_t* binnedImage, const int binFactor);

#endif
