// inspired by https://www.daniweb.com/programming/software-development/code/241875/fast-animation-with-the-windows-gdi

#include "3DFC.h"
using namespace std;

static TCHAR szWindowClass[] = L"DesktopApp";
static TCHAR szTitle[] = L"Pipe";

int width, height;
constexpr int fps = 50;

void (*getpixs)(int, int, int, int, int, void *);
int frame = 0;
bool working, bmpChanged;
POINTS mousePos;
HINSTANCE hInst;

HBITMAP hbmp;
HANDLE hTickThread;
HWND hwnd;
HDC hdcMem;
COLORREF *pixels;

DWORD WINAPI tickThreadProc(HANDLE handle)
{
    Sleep(50);
    ShowWindow(hwnd, SW_SHOWNORMAL);

    HDC hdc = GetDC(hwnd);

    hdcMem = CreateCompatibleDC(hdc);

    int delay = 1000 / fps;

    for (;;)
    {
        if (bmpChanged)
        {
            SelectObject(hdcMem, hbmp);
            bmpChanged = false;
        }

        working = true;
        getpixs(width, height, mousePos.x, mousePos.y, frame, pixels);

        BitBlt(hdc, 0, 0, width, height, hdcMem, 0, 0, SRCCOPY);

        //cout << "frame " << frame++;
        working = false;

        Sleep(delay);
    }

    DeleteDC(hdc);
}

void MakeSurface(HWND hwnd)
{
    BITMAPINFO bmi;
    bmi.bmiHeader.biSize = sizeof(BITMAPINFO);
    bmi.bmiHeader.biWidth = width;
    bmi.bmiHeader.biHeight = -height;
    bmi.bmiHeader.biPlanes = 1;
    bmi.bmiHeader.biBitCount = 32;
    bmi.bmiHeader.biCompression = BI_RGB;
    bmi.bmiHeader.biSizeImage = 0;
    bmi.bmiHeader.biXPelsPerMeter = 0;
    bmi.bmiHeader.biYPelsPerMeter = 0;
    bmi.bmiHeader.biClrUsed = 0;
    bmi.bmiHeader.biClrImportant = 0;
    bmi.bmiColors[0].rgbBlue = 0;
    bmi.bmiColors[0].rgbGreen = 0;
    bmi.bmiColors[0].rgbRed = 0;
    bmi.bmiColors[0].rgbReserved = 0;

    HDC hdc = GetDC(hwnd);

    while (working)
    {
    }

    if (hbmp != NULL)
        DeleteObject(hbmp);
    hbmp = CreateDIBSection(hdc, &bmi, DIB_RGB_COLORS, (void **)&pixels, NULL, 0);
    bmpChanged = true;
    DeleteDC(hdc);
}

LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    switch (message)
    {
    case WM_PAINT:
    {
        PAINTSTRUCT ps;
        HDC hdc = BeginPaint(hWnd, &ps);

        if (hdcMem != NULL)
            BitBlt(hdc, 0, 0, width, height, hdcMem, 0, 0, SRCCOPY);

        EndPaint(hWnd, &ps);

        // cout << " -- painted" << endl;
    }
    break;
    case WM_CREATE:
    {
        RECT r;
        GetClientRect(hWnd, &r);
        width = r.right - r.left;
        height = r.bottom - r.top;
        MakeSurface(hWnd);
        hTickThread = CreateThread(NULL, NULL, &tickThreadProc, NULL, NULL, NULL);
    }
    break;
    case WM_SIZE:
    {
        width = LOWORD(lParam);
        height = HIWORD(lParam);
        MakeSurface(hWnd);
    }
    break;
    case WM_CLOSE:
        DestroyWindow(hwnd);
        break;
    case WM_DESTROY:
        TerminateThread(hTickThread, 0);
        PostQuitMessage(0);
        break;
    case WM_MOUSEMOVE:
        mousePos = MAKEPOINTS(lParam);
        break;
    default:
        return DefWindowProc(hWnd, message, wParam, lParam);
        break;
    }

    return 0;
}

HWND WINAPI startwin(void (*f)(int, int, int, int, int, void *))
{
    getpixs = f;
    hInst = GetModuleHandle(nullptr);

    WNDCLASSEX wc;
    MSG msg;

    wc.cbClsExtra = 0;
    wc.cbWndExtra = 0;
    wc.cbSize = sizeof(WNDCLASSEX);
    wc.hbrBackground = CreateSolidBrush(0);
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wc.hIcon = LoadIcon(NULL, IDI_APPLICATION);
    wc.hIconSm = LoadIcon(NULL, IDI_APPLICATION);
    wc.hInstance = hInst;
    wc.lpfnWndProc = WndProc;
    wc.lpszClassName = L"animation_class";
    wc.lpszMenuName = NULL;
    wc.style = 0;

    if (!RegisterClassEx(&wc))
    {
        MessageBox(NULL, L"Failed to register window class.", L"Error", MB_OK);
        return 0;
    }

    hwnd = CreateWindowEx(WS_EX_APPWINDOW, L"animation_class", L"Animation", WS_OVERLAPPEDWINDOW, 300, 200, 200, 100, NULL, NULL, hInst, NULL);

    UpdateWindow(hwnd);

    while (GetMessage(&msg, 0, 0, NULL) > 0)
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    return 0;
}