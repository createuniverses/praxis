
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/


#ifndef WORLD_H
#define WORLD_H

#include "GL/openglut.h"

#include "ML_Transform.h"
#include "GLEditor.h"

#include "RtMidi.h"

class World
{
public:
	World();
	virtual ~World();

    void Init();

    void Update();
    void Render();

    void ResetCounts()   { m_nRenderCount = 0; m_nUpdateCount = 0; }

    int GetRenderCount() { return m_nRenderCount; }
    int GetUpdateCount() { return m_nUpdateCount; }

    void OnMouseMove(int nX, int nY);

    void OnDoubleClick();

    void OnLButtonDown(int nX, int nY);
    void OnLButtonUp(int nX, int nY);

    void OnRButtonDown(int nX, int nY);
    void OnRButtonUp(int nX, int nY);

    void OnWheelDown(int nX, int nY);
    void OnWheelUp(int nX, int nY);

    void OnKeyDown(unsigned char nKey, int nX, int nY);
    void OnKeyUp(unsigned char nKey, int nX, int nY);

    void OnKeyDownSpecial(unsigned char nKey, int nX, int nY);
    void OnKeyUpSpecial(unsigned char nKey, int nX, int nY);

    void UpdateMousePos(int nX, int nY);

    // Camera

    mlTransform * GetCameraTransform();
    mlTransform * GetCameraTransformBase();

    mlVector3D GetBaseUp()      { return m_trnCameraBase.GetMatrix().J; }
    mlVector3D GetBaseForward() { return m_trnCameraBase.GetMatrix().K; }
    mlVector3D GetBaseSide()    { return m_trnCameraBase.GetMatrix().I; }

    void PanCamera(float fX, float fY);
    void RotateCamera(float fHeading, float fPitch);
    void PositionPreservingOrbitCamera(mlVector3D & vCenter, float fHeading, float fPitch);
    void OrientationPreservingOrbitCamera(mlVector3D & vCenter, float fHeading, float fPitch);
    void LookAt(mlVector3D vFocalPoint);
    void LookDown();

    void EnableStdMouseCam()  { m_bMouseMovesCamera = true;  }
    void DisableStdMouseCam() { m_bMouseMovesCamera = false; }
    bool StdMouseCamEnabled() { return m_bMouseMovesCamera;  }

    mlVector3D GetMousePickPosition() { return m_vMousePickPosition; }
    mlVector3D GetMousePosition() { return m_vMouse2DPosition; }
    mlVector3D GetMouseDelta() { return m_vMouse2DPositionDelta; }

    bool Pick(mlVector3D & pos2d, mlVector3D & pos3d);

    bool m_bUpdatePickPosition;
    bool m_bRenderFloorPlane;
    bool m_bRenderFloorGrid;
    bool m_bRenderProbesHUD;
    bool m_bRenderMousePickSphere;

    //GLEditor * m_pCurrentBuffer;
    int m_nCurrentBuffer;
    // Need several editors so we can have multiple buffers
    //
    std::vector<GLEditor *> m_buffers;
    GLEditor * GetEditor() { return m_buffers[m_nCurrentBuffer]; }

    void NewEditor();
    void SwitchToEditor(int n);
    void SwitchToEditor(const std::string & sName);
    void NextEditor();
    void PreviousEditor();
    void CloseEditor();

    void ShowOutput() { m_bRenderOutput = true;  }
    void HideOutput() { m_bRenderOutput = false; }
    bool OutputVisible() { return m_bRenderOutput; }

    void ShowError() { m_bRenderError = true;  }
    void HideError() { m_bRenderError = false; }
    bool ErrorVisible() { return m_bRenderError; }

    void ShowEditor() { m_bRenderEditor = true; glutSetKeyRepeat(GLUT_KEY_REPEAT_ON); }
    void HideEditor() { m_bRenderEditor = false; glutSetKeyRepeat(GLUT_KEY_REPEAT_OFF); }
    bool EditorVisible() { return m_bRenderEditor; }

    void Pause()     { m_bRunning = false; }
    void Continue()  { m_bRunning = true; }
    bool IsRunning() { return m_bRunning; }

    // Add options for 3D position, 2D position, 3D rotation

    void Reshape(int nWidth, int nHeight);

    static void DrawText2D(mlVector3D vPos, const std::string & sText, float fWidth = 100.0f, float fHeight = 100.0f);
    static void DrawText2DUnmapped(mlVector3D vPos, const std::string & sText);
    static void DrawText3D(mlVector3D vPos, const std::string & sText);
    static void DrawText3DStroked(mlVector3D vPos, const std::string & sText);
    static std::string SelectBeginLines(const std::string & sText, int nNumLines);
    static std::string SelectEndLines(const std::string & sText, int nNumLines);

    bool LeftMouseDown() { return m_bLeftMouseDown; }
    bool LeftMouseWentDown() { return m_bLeftMouseWentDown; }
    bool RightMouseDown() { return m_bRightMouseDown; }
    bool RightMouseWentDown() { return m_bRightMouseWentDown; }

private:

    // OpenGL state
    void BuildGLMatrices();

    // Mouse
    void RefreshPickPosition();
    void RefreshFloorPickPosition();

    // Rendering the ground and cursor
    void RenderFloorPlane();
    void RenderFloorGrid();
    void RenderMousePickSphere();

    void RenderProbes();

    // Need to expose more of these, however exposing them
    // wasn't necessary for Flashback demo

	mlTransform m_trnCamera;
    mlTransform m_trnCameraBase;
    float m_fFieldOfView;

	mlVector3D m_vMouse2DPosition;
    mlVector3D m_vMouse2DPositionDelta;

	mlVector3D m_vMousePickPosition;
    float      m_fMousePickDepth;
    mlVector3D m_vMouseFloorPickPosition;
    float      m_fMouseFloorPickDepth;

    bool m_bDoubleClick;

    bool m_bLeftMouseDown;
    bool m_bLeftMouseWentDown;
    bool m_bRightMouseDown;
    bool m_bRightMouseWentDown;

	bool m_bMouseMovesCamera;
    bool m_bUsePositionPreservingOrbitCamera;

    int m_nRenderCount;
    int m_nUpdateCount;

    bool m_bRenderOutput;
    bool m_bRenderError;
    bool m_bRenderEditor;

    bool m_bRunning;

    // Matrices for the current frame.
    GLint viewport[4];
    GLdouble modelMatrix[16];
    GLdouble projMatrix[16];

public:

    RtMidiOut* m_midiout;
    RtMidiIn*  m_midiin;
};

#endif // WORLD_H

