project('ImagerPlugin', 'cpp',
  version : '0.1',
  default_options : ['warning_level=3', 'cpp_std=c++20']
)

# --- USER SETTINGS ---
# Change this variable to customize your plugin's name
plugin_name = 'ImagerPlugin'
# ---------------------

# Enforce 64-bit build
if host_machine.cpu_family() != 'x86_64'
  error('This plugin must be built for 64-bit architecture. Please configure your build system for x64.')
endif

# Fetch dependencies
json_dep = dependency('nlohmann_json')
queue_dep = dependency('concurrentqueue')

# Add an include dir so that we can use ImagerPluginCore/ syntax to access its headers
core_inc = include_directories('.')

# Define the shared library (DLL) target for the plugin
shared_library(plugin_name,
  # Source files for the plugin implementation
  # It should not be necessary to edit or modify these
  'ImagerPluginCore/BaseCameraClass.cpp',
  'ImagerPluginCore/CameraUtils.cpp',
  'ImagerPluginCore/ImageProcessingUtils.cpp',
  'ImagerPluginCore/ImagerPlugin.cpp',
  'ImagerPluginCore/CameraPropertiesEncoding.cpp',
  'ImagerPluginCore/PluginManager.cpp',
  'ImagerPluginCore/RobotProgramArguments.cpp',
  'ImagerPluginCore/StandardCameraProperties.cpp',
  
  # Add your plugin source files here
  'src/PluginImplementation.cpp',
  
  include_directories : core_inc,
  #link_with : [core_lib],
  dependencies : [json_dep, queue_dep],
  name_suffix : 'imagerplugin', # Change the output file extension
  cpp_args : ['-DNOMINMAX', '-DIMAGER_EQUIPMENT_NAME="' + plugin_name + '"'],
  )
