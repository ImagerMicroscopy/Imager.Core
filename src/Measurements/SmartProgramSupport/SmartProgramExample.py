# The 'ProgramRunner' object is automatically made available by the main program
# when the plugin is loaded.

def OnSmartProgramStart():
    """
    This function will be called when the smart program starts
    """

def OnImagesReceived(imagerImages):
    """
    This function will be called when an acquired image is received.
    The input is a list of ImagerImage objects, each acquired from the same detection event.
    Each ImagerImage object has the following members:
    """

    nImages = len(imagerImages)
    print(f'received {nImages} images')
    for imagerImage in imagerImages:
        image = imagerImage.image
        detection_index = imagerImage.detection_index
        timestamp = imagerImage.timestamp
        acquisition_name = imagerImage.acquisition_name
        detector_name = imagerImage.detector_name
        x = imagerImage.x
        y = imagerImage.y
        z = imagerImage.z
        stage_position_name = imagerImage.stage_position_name
        print(f'received image with detection index {detection_index} and timestamp {timestamp}')

    return ProgramRunner.SuccessResponse()
    
def OnDoTimesDecisionRequested():
    """
    This function will be called when we need to decide how many
    times a do times loop should be run
    """
    print("Received do times decision request")

    ntotal = 5          # number of times to repeat in total
    return ProgramRunner.DoTimesDecisionResponse(ntotal, "")

def OnStageLoopDecisionRequested():
    """
    This function will be called when we need to decide how many
    times a stage loop should be run
    """

    # coordinates are in micrometer
    x = 0.0; y = 0.0; z = 0.0
    positions = [ProgramRunner.StagePosition("ExamplePositionName1", x, y, z),
                 ProgramRunner.StagePosition("ExamplePositionName2", x + 1.0, y + 1.0, z + 1.0)]

    # in this example code we return no positions since we could otherwise have
    # accidental stage movement!
    #return ProgramRunner.StageLoopDecisionResponse(positions)
    return ProgramRunner.StageLoopDecisionResponse([], "")

def OnRelativeStageLoopDecisionRequested():
    """
    This function will be called when we need to decide how many
    times a relative stage loop should be run
    """

    # coordinates are in micrometer
    dx = 0.0; dy = 0.0; dz = 0.0        # the step size - distance between the positions
    nNegX = 0; nNegY = 0; nNegZ = 0     # number of additional positions in the negative direction
    nPosX = 0; nPosY = 0; nPosZ = 0     # number of additional positions in the positive direction
    returnToStartingPosition = True      # whether to return to the starting position after the loop

    # the total number of acquisitions will be (nNegX + nPosX + 1) * (nNegY + nPosY + 1) * (nNegZ + nPosZ + 1)
    parameters = ProgramRunner.RelativeStageLoopParameters(dx, dy, dz, nNegX, nNegY, nNegZ, nPosX, nPosY, nPosZ, returnToStartingPosition)
    return ProgramRunner.RelativeStageLoopDecisionResponse(parameters, "")

def OnTimeLapseDecisionRequested():
    """
    This function will be called when we need to decide how many
    times a time lapse loop should be run
    """
    ntotal = 5          # number of times to repeat in total
    timedelta = 1.0     # time between each iteration in seconds
    return ProgramRunner.TimeLapseDecisionResponse(ntotal, timedelta, "")

def OnSmartProgramEnd():
    """
    This function will be called when the smart program ends
    """
    return ProgramRunner.SuccessResponse()
