import sys
import os
import importlib.util
from flask import Flask, request
from http import HTTPStatus
import json
import msgpack
import numpy as np
import random

class ImagerImage:
    def __init__(self, detection_index, image, timestamp, acquisition_name, detector_name, x, y, z):
        self.detection_index = detection_index
        self.image = image  # numpy array
        self.timestamp = timestamp  # double
        self.acquisition_name = acquisition_name  # string
        self.detector_name = detector_name # string
        self.x = x
        self.y = y
        self.z = z

def RandomNamedStagePosition():
    position = {
        "name": "posName", "coordinates": {
            "x": random.random(), "y": random.random(), "z": random.random(),
            "usinghardwareautofocus": False, "hardwareautofocusoffset": 0
        }
    }
    return position

def RelativeStageLoopParameters():
    dx = 50; dy=50; dz=50
    nNegX = 1; nNegY = 1; nNegZ = 1
    nPosX = 1; nPosY = 1; nPosZ = 1
    params = {"deltax" : dx, "deltay": dy, "deltaz": dz,
               "additionalplanesx":[nNegX, nPosX], "additionalplanesy": [nNegY, nPosY],
               "additionalplanesz": [nNegZ, nPosZ], "returntostartingposition": True}
    return params

def SuccessResponse():
    return {"type": "status", "status": "success"}
def ErrorResponse(what):
    return {"type": "status", "status": "error", "what": what}
def NoDecisionResponse():
    return {"type": "nodecision"}
def DoTimesDecisionResponse(n_times):
    return {"type": "dotimesdecision", "ntotal": n_times}
def StageLoopDecisionResponse():
    return {"type": "stageloopdecision", "positions": [RandomNamedStagePosition(), RandomNamedStagePosition()]}
def RelativeStageLoopDecisionResponse():
    return {"type": "relativestageloopdecision", "params": RelativeStageLoopParameters}
def TimeLapseDecisionResponse():
    return {"type": "timelapsedecision", "nototal": 3, "timedelta": 1}

# --- Plugin Loading Function ---
def load_plugin(plugin_path, main_module):
    """
    Dynamically loads a Python module and provides it a reference to the main module.
    """
    if not os.path.exists(plugin_path):
        print(f"Error: Plugin file not found at {plugin_path}")
        return None
    
    module_name = "user_plugin"
    spec = importlib.util.spec_from_file_location(module_name, plugin_path)
    if spec is None:
        print(f"Error: Could not load spec from {plugin_path}")
        return None
        
    user_module = importlib.util.module_from_spec(spec)
    
    # Pass the main module reference to the plugin
    user_module.ProgramRunner = main_module
    
    sys.modules[module_name] = user_module
    spec.loader.exec_module(user_module)
    return user_module

# --- Main Server Logic ---
def create_app(plugin):
    app = Flask(__name__)
    app.plugin = plugin

    @app.route('/data', methods=['POST'])
    def OnDataReceived():
        if not hasattr(app.plugin, 'OnImagesReceived'):
            return json.dumps({"error": "'OnImagesReceived' not found in plugin"}), HTTPStatus.NOT_IMPLEMENTED

        try:
            print("received image")
            # Get the binary data from the request body and decode it
            binary_data = request.data
            decoded_data = msgpack.unpackb(binary_data, raw=False)

            # create an ImagerImage object
            data_dict = decoded_data['message']['data']
            binary_data = data_dict['imagedata']
            nrows = data_dict['nrows']
            ncols = data_dict['ncols']
            image_1d = np.frombuffer(binary_data, dtype=np.uint16)
            image_2d = image_1d.reshape((nrows, ncols))
            timestamp = data_dict['timestamp']
            detectorname = data_dict['detectorname']

            metadata_dict = decoded_data['message']['metadata']
            acquisitiontype = metadata_dict['acquisitiontype']
            detectionindex = metadata_dict['detectionindex']

            # 3. Access the 'stageposition' members
            stage_position_dict = metadata_dict['stageposition']
            stage_x = stage_position_dict['x']
            stage_y = stage_position_dict['y']
            stage_z = stage_position_dict['z']

            imagerImage = ImagerImage(detection_index=detectionindex, image=image_2d, timestamp=timestamp,
                                      acquisition_name=acquisitiontype, detector_name=detectorname,
                                      x=stage_x, y=stage_y, z=stage_z)
            
            # Call the user-defined function from the plugin
            # The plugin will now have access to the main module via its 'ProgramRunner' attribute
            result = app.plugin.OnImagesReceived([imagerImage])

            return json.dumps(result), HTTPStatus.OK
        except Exception as e:
            return json.dumps({"error": f"An error occurred: {str(e)}"}), HTTPStatus.INTERNAL_SERVER_ERROR
    
    @app.route('/dotimesdecision', methods=['GET'])
    def OnDoTimesDecisionRequested():
        if not hasattr(app.plugin, 'OnDoTimesDecisionRequested'):
            return json.dumps({"error": "'OnDoTimesDecisionRequested' not found in plugin"}), HTTPStatus.NOT_IMPLEMENTED

        try:
            print("received do times decision request")
            result = app.plugin.OnDoTimesDecisionRequested()
            return json.dumps(result), HTTPStatus.OK
        except Exception as e:
            return json.dumps({"error": f"An error occurred: {str(e)}"}), HTTPStatus.INTERNAL_SERVER_ERROR
        
    @app.route('/stageloopdecision', methods=['GET'])
    def onStageLoopDecisionRequested():
        if not hasattr(app.plugin, 'OnStageLoopDecisionRequested'):
            return json.dumps({"error": "'OnStageLoopDecisionRequested' not found in plugin"}), HTTPStatus.NOT_IMPLEMENTED

        try:
            result = app.plugin.OnStageLoopDecisionRequested()
            return json.dumps(result), HTTPStatus.OK
        except Exception as e:
            return json.dumps({"error": f"An error occurred: {str(e)}"}), HTTPStatus.INTERNAL_SERVER_ERROR
    
    @app.route('/relativestageloopdecision', methods=['GET'])
    def OnRelativeStageLoopDecisionRequested():
        if not hasattr(app.plugin, 'OnRelativeStageLoopDecisionRequested'):
            return json.dumps({"error": "'OnRelativeStageLoopDecisionRequested' not found in plugin"}), HTTPStatus.NOT_IMPLEMENTED

        try:
            result = app.plugin.OnRelativeStageLoopDecisionRequested()
            return json.dumps(result), HTTPStatus.OK
        except Exception as e:
            return json.dumps({"error": f"An error occurred: {str(e)}"}), HTTPStatus.INTERNAL_SERVER_ERROR
    
    @app.route('/timelapsedecision', methods=['GET'])
    def OnTimeLapseDecisionRequested():
        if not hasattr(app.plugin, 'OnTimeLapseDecisionRequested'):
            return json.dumps({"error": "'OnTimeLapseDecisionRequested' not found in plugin"}), HTTPStatus.NOT_IMPLEMENTED

        try:
            result = app.plugin.OnTimeLapseDecisionRequested()
            return json.dumps(result), HTTPStatus.OK
        except Exception as e:
            return json.dumps({"error": f"An error occurred: {str(e)}"}), HTTPStatus.INTERNAL_SERVER_ERROR

    return app

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print(f"Usage: {os.path.basename(__file__)} <port_number> <plugin_file_path.py>")
        sys.exit(1)

    try:
        port = int(sys.argv[1])
        plugin_file_path = sys.argv[2]
        
        # Pass a reference to the current module to the plugin loader
        user_plugin = load_plugin(plugin_file_path, sys.modules[__name__])
        if user_plugin is None:
            sys.exit(1)
            
        app = create_app(user_plugin)
        
        # Run the server
        app.run(port=port, debug=False, threaded=False)
        
    except ValueError:
        print("Error: Port number must be an integer.")
        sys.exit(1)