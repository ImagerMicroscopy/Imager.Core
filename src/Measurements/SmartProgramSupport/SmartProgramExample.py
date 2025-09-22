# The 'ProgramRunner' object is automatically made available by the main program
# when the plugin is loaded.

def OnImagesReceived(imagerImages):
    """
    This function will be called when an acquired image is received
    """

    nImages = len(imagerImages)
    for imagerImage in imagerImages:
        image = imagerImage.image
        timestamp = imagerImage.timestamp
        acquisition_name = imagerImage.acquisition_name
        detector_name = imagerImage.detector_name
        x = imagerImage.x
        y = imagerImage.y
        z = imagerImage.z

    print("Plugin is processing the incoming binary data.")

    return ProgramRunner.SuccessResponse()
    
def OnDoTimesDecisionRequested():
    print("Received do times decision request")

    return ProgramRunner.DoTimesDecisionResponse(10)

def OnStageLoopDecisionRequested():
    return ProgramRunner.ErrorResponse("not yet implemented")

def OnRelativeStageLoopDecisionRequested():
    return ProgramRunner.ErrorResponse("not yet implemented")

def OnTimeLapseDecisionRequested():
    return ProgramRunner.ErrorResponse("not yet implemented")
