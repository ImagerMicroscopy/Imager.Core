project('ImagerPlugin', 'cpp',
  version : '0.1',
  default_options : ['warning_level=3', 'cpp_std=c++17']
)

# --- USER SETTINGS ---
# Change this variable to customize your plugin's name
plugin_name = 'ImagerPlugin'
# ---------------------

# Enforce 64-bit build
if host_machine.cpu_family() != 'x86_64'
  error('This plugin must be built for 64-bit architecture. Please configure your build system for x64.')
endif

# Fetch nlohmann_json dependency 
# (Automatically resolved by subprojects/nlohmann_json.wrap)
json_dep = dependency('nlohmann_json')

# Define the shared library (DLL) target for the plugin
shared_library(plugin_name,
  'Plugin.cpp',
  'CameraPropertiesEncoding.cpp',
  'RobotProgramArguments.cpp',

  # --- USER SETTINGS ---
  # Add your own source files here
  # ---------------------

  dependencies : json_dep,
  name_suffix : 'imagerplugin', # Change the output file extension
  cpp_args : ['-DNOMINMAX', '-DIMAGER_EQUIPMENT_NAME="' + plugin_name + '"'],
)