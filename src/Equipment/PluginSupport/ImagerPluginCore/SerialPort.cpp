#include "SerialPort.h"

#include <format>

#include "PluginManager.h"

void SerialPort::open(const std::string& portName, std::uint32_t baudRate, std::uint32_t timeoutMillis, SerialFormat format) {
    if (_serial.isOpen()) {
        std::string errorMsg = std::format("SerialPort::open({}) but already open on {}", portName, _serial.getPort());
        throw std::runtime_error(errorMsg);
    }

    _serial.setPort(portName);
    _serial.setBaudrate(baudRate);
    _serial.setBytesize(serial_cpp::eightbits);
    _serial.setParity(serial_cpp::parity_t::parity_none);

    switch (format) {
        case SerialFormat::f8N1:
            _serial.setStopbits(serial_cpp::stopbits_t::stopbits_one);
            break;
        case SerialFormat::f8N2:
            _serial.setStopbits(serial_cpp::stopbits_t::stopbits_two);
            break;
    }

    serial_cpp::Timeout timeout(50,             // inter_byte_timeout
                            timeoutMillis,  // read_timeout_constant
                            0,              // read_timeout_multiplier
                            timeoutMillis,  // write_timeout_constant
                            0);             // write_timeout_multiplier
    _serial.setTimeout(timeout);

    _serial.open();
    if (!_serial.isOpen()) {
        std::string errorMsg = std::format("SerialPort::open({}) failed to open", portName);
        throw std::runtime_error(errorMsg);
    }
}

void SerialPort::close() {
    if (_serial.isOpen()) {
        _serial.close();
    }
}

void SerialPort::write(const std::string& data) {
    size_t nBytesWrittenTotal = 0;
    while (nBytesWrittenTotal < data.size()) {
        size_t nBytesWritten = _serial.write(reinterpret_cast<const uint8_t*>(data.data()) + nBytesWrittenTotal, 
                                             data.size() - nBytesWrittenTotal);
        if (nBytesWritten == 0) {
            std::string errMsg = std::format("SerialPort::write timed out or failed to write to {}", _serial.getPort());
            throw std::runtime_error(errMsg);
        }
        
        nBytesWrittenTotal += nBytesWritten;
    }

    if (_printCommunication) {
        std::string msg = std::format("{} wrote: {}\n", _serial.getPort(), data);
        PluginManager::Manager().Print(msg);
    }
}

std::string SerialPort::read() {
    return read(65535);
}

std::string SerialPort::read(size_t maxNBytesToRead) {
    std::string response = _serial.read(maxNBytesToRead);
    if (_printCommunication) {
        std::string msg = std::format("{} read: {}\n", _serial.getPort(), response);
        PluginManager::Manager().Print(msg);
    }
    return response;
}

std::uint8_t SerialPort::writeByteAndReadByte(const std::uint8_t byte) {
    std::string byteStr(1, static_cast<char>(byte));
    write(byteStr);
    std::string response = read(1);
    if (response.empty()) {
        std::string errMsg = std::format("Expected to read 1 byte from {} but read nothing", _serial.getPort());
        throw std::runtime_error(errMsg);
    }

    return static_cast<std::uint8_t>(response[0]);
}

std::string SerialPort::writeAndReadUntilString(const std::string& dataToWrite, const std::string& terminatorString) {
    write(dataToWrite);

    std::string response = _serial.readline(65536, terminatorString);
    if (_printCommunication) {
        std::string msg = std::format("{} read: {}\n", _serial.getPort(), response);
        PluginManager::Manager().Print(msg);
    }
    
    return response;
}

void SerialPort::clearBuffers() {
    _serial.flush();
}
