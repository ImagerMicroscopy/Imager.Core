#include "SerialPort.h"

void SerialPort::open(const std::string& portName, std::uint32_t baudRate, std::uint32_t timeoutMillis) {
    _serial.setPort(portName);
    _serial.setBaudrate(baudRate);
    _serial.setBytesize(serial_cpp::eightbits);
    _serial.setParity(serial_cpp::parity_t::parity_none);
    _serial.setStopbits(serial_cpp::stopbits_t::stopbits_one);

    serial_cpp::Timeout timeout(50,             // inter_byte_timeout
                            timeoutMillis,  // read_timeout_constant
                            0,              // read_timeout_multiplier
                            timeoutMillis,  // write_timeout_constant
                            0);             // write_timeout_multiplier
    _serial.setTimeout(timeout);

    _serial.open();
}

void SerialPort::write(const std::string& data) {
    size_t nBytesWrittenTotal = 0;
    for ( ; ; ) {
        size_t nBytesWritten = _serial.write(data.substr(nBytesWrittenTotal));
        nBytesWrittenTotal += nBytesWritten;
        if (nBytesWrittenTotal >= data.size()) {
            break;
        }
    }
}

std::uint8_t SerialPort::writeByteAndReadByte(const std::uint8_t byte) {
    std::uint8_t responseByte = 0;
    _serial.write(&byte, 1);
    _serial.read(&responseByte, 1);
    return responseByte;
}

std::string SerialPort::writeAndReadUntilString(const std::string& dataToWrite, const std::string& terminatorString) {
    std::string response;
    std::string remaining = dataToWrite;
    while (!remaining.empty()) {
        size_t nBytesWritten = _serial.write(remaining);
        remaining = remaining.substr(nBytesWritten);
    }

    std::string accum;
    for ( ; ; ) {
        std::string read;
        size_t nBytesRead = _serial.read(read);
        accum += read;

        size_t pos = accum.rfind(terminatorString);
        if ((pos != std::string::npos) && (pos + terminatorString.size() == accum.size())) {
            return accum;
        }
    }
}

void SerialPort::clearBuffers() {
    _serial.flush();
}
