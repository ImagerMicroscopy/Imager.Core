#ifndef SERIALPORT_H
#define SERIALPORT_H

#include <cstdint>
#include <string>

#include "serial_cpp/serial.h"

class SerialPort {
public:
    SerialPort() = default;
    ~SerialPort() = default;
    SerialPort(const SerialPort&) = delete;
    SerialPort& operator=(const SerialPort&) = delete;

    void open(const std::string& portName, std::uint32_t baudRate, std::uint32_t timeoutMillis);

    void write(const std::string& data);
    std::uint8_t writeByteAndReadByte(const std::uint8_t byte);
    std::string writeAndReadUntilString(const std::string& dataToWrite, const std::string& terminatorString);

    void clearBuffers();

private:
    serial_cpp::Serial _serial;
};

#endif // SERIALPORT_H
