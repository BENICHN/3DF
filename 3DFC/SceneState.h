#include <windows.h>

struct SceneState
{
    int width;
    int height;
    int fps;
    int frameCount;
    bool lButton;
    bool mButton;
    bool rButton;
    int mouseX;
    int mouseY;
    DWORD keysPressed;
};