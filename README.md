## Imager.Core


```text
            ██╗███╗   ███╗ █████╗  ██████╗ ███████╗██████╗
            ██║████╗ ████║██╔══██╗██╔════╝ ██╔════╝██╔══██╗
            ██║██╔████╔██║███████║██║  ███╗█████╗  ██████╔╝
            ██║██║╚██╔╝██║██╔══██║██║   ██║██╔══╝  ██╔══██╗
            ██║██║ ╚═╝ ██║██║  ██║╚██████╔╝███████╗██║  ██║
            ╚═╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝ ╚══════╝╚═╝  ╚═╝
```
This repository contains documentation and code for running the Imager Haskell backend that is responsible for hardware interaction and
Imager DSL definition.



## Table of Contents
- [Installation](#Installation)
- [Camera plugin installation](#Camera-installation)
- [Hardware plugin installation](#Hardware-installation)
- [License](#license)

## Installation (Windows)

### Building from source

Imager utilizes [stack](https://docs.haskellstack.org/en/stable/) for building the Haskell backend. 

#### Step 1: Install stack

Download the stack installer for Windows at the following link: [windows-x86_64](https://get.haskellstack.org/stable/windows-x86_64-installer.exe). Follow the installation instructions.

#### Step 2: Clone the repository

You can clone the repository, including the submodules as follows: 

First, go to the directory where you want to clone Imager to. 
Open the terminal or powershell and copy paste the following:

```bash
git clone --recurse-submodules https://github.com/ImagerMicroscopy/Imager.Core

cd Imager.Core
```

#### Step 3: Build Haskell backend

Build the backend by running the following command from within the directory:

```bash
stack build .
```

#### Step 4: Copy build artifacts

We now can copy all of the binaries into our build folder:

```bash
cp -r "$(stack path --local-install-root)\bin\*" ./build/   
```
This will copy the binaries into the build folder

The final folder structure looks as follows:

```bash

Imager.Core/build/
├─ PluginConfigurations/ -> Configurations for each plugin
├─ Plugins/              -> Hardware plugins in .imagerplugin format
├─ SmartProgramPython/   -> Python backend
├─ config.json           -> Config file
├─ equipment.txt         -> Hardware plugins built into imager
└─ Imager.exe            -> Main binary
```

#### Step 5: Edit the config file

The config.json contains configuration for running the python backend
> [!IMPORTANT]
> Configuring the Python interpreter path is required to enable smart imaging capabilities.
> If the path is not configured correctly, Imager will run without the Python backend and
> smart imaging capabilities will be disabled.

Follow the installation instructions to install Python virtual environment for smart imaging backend from the repository, as found here:
[Smart imaging repository](https://github.com/ImagerMicroscopy/Imager.Smart)

Once the virtual environment is set up, you can copy the interpreter path (typically it is  located at: ```PATHTOENVIRONMENT\Scripts\python.exe```)


> [!IMPORTANT]
> Make sure the path contains backslashes (/) instead of forward slashes 


Copy the path to the corresponding field in the config.json, f.e.:

```json
{
    "pythonpath":  "C:/path/to/venv/Scripts/python.exe"
}
```


## Camera plugin installation

To install the camera plugin, follow the instructions to compile the camera plugins at: [Camera plugin repository](https://github.com/ImagerMicroscopy/Imager.Cameras)


#### Step 1: Copy the camera plugin
Copy the SCCamera.imagerplugin into the ```Imager.Core/build/Plugins/```

Now Imager will be able to utilize the cameras provided by the camera plugin

## Hardware plugin installation

You can install various hardware plugins in two ways:

### Installation with .imagerplugin

The standard way to install hardware support for imager is through our own defined ```.imagerplugin``` system.
We provide an API for integrating hardware into the Imager. Description of how to write your own plugins can be found here:
[Hardware integration](https://github.com/ImagerMicroscopy/Imager.Plugins)

To use the hardware plugins that are built into ```.imagerplugin```, you can simply copy the corresponding plugin into the ```Imager.Core/build/Plugins/```
directory. If the plugin depends on external libraries, you can copy them into ```Imager.Core/build``` folder. 


### Installation with equipment file (legacy)

Some hardware support is implemented directly inside of Imager. These can be enabled by modifying the ```Imager.Core/build/equipment.txt``` file. 
The plugins supported in this form can be found in the [Hardware integration](https://github.com/ImagerMicroscopy/Imager.Plugins) as well. 

To enable these plugins, user needs to modify ```Imager.Core/build/equipment.txt``` by adding fields corresponding for each hardware equipment (see examples in the repository). For example, the following equipment file enables 2 dummy filter wheels (fw1, and fw2), one dummy stage and one dummy light source:


```equipment.txt
[
    DummyFilterWheelDesc {dfwDescName = "fw1", dfwDescFilters = [("DAPI",0), ("GFP",1), ("YFP",2), ("RFP",3), ("640",4)]},
    DummyFilterWheelDesc {dfwDescName = "fw2", dfwDescFilters = [("DAPI/GFP/RFP/640",0), ("CFP/YFP/RFP",1)]},
    DummyLightSourceDesc {dlsdName = "hello"},
    DummyStageDesc {dsName = "dStage"}
]
```
> [!IMPORTANT]
> Modifying equipment.txt file is considered legacy and will be slowly phased out in favor of the .imagerplugin system


### License

The software is licensed under BSD-3-Clause license

