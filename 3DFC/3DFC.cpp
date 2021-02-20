#include "3DFC.h"
#define IDT_TIMER1 0001

using namespace std;

static TCHAR szWindowClass[] = L"DesktopApp";
static TCHAR szTitle[] = L"Pipe";

COLORREF *pixels;
void (*getpixs)(int, int, int, int, int, void *);
int frame = 0;

POINTS mousePos;

HINSTANCE hInst;

LPWSTR ErrorToString(DWORD errorMessageID)
{
    if (errorMessageID == 0)
        return L"";

    wchar_t buf[256];
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                  NULL, errorMessageID, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                  buf, (sizeof(buf) / sizeof(wchar_t)), NULL);

    return buf;
}

void DisplayPixArray(HDC hdc, int width, int height, COLORREF *pixs)
{
    HDC memdc = CreateCompatibleDC(hdc);
    HBITMAP hbmp = CreateBitmap(width, height, 1, 32, pixs);

    HGDIOBJ oldBitmap = SelectObject(memdc, hbmp);
    BitBlt(hdc, 0, 0, width, height, memdc, 0, 0, SRCCOPY);
    SelectObject(hdc, oldBitmap);

    DeleteDC(memdc);
    DeleteObject(hbmp);
}

pair<int, int> GetBounds(HWND hWnd)
{
    RECT ca;
    GetClientRect(hWnd, &ca);
    int width = (int)(ca.right - ca.left);
    int height = (int)(ca.bottom - ca.top);
    return pair<int, int>(width, height);
}

LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    PAINTSTRUCT ps;
    HDC hdc;
    pair<int, int> bounds;

    switch (message)
    {
    case WM_PAINT:
        bounds = GetBounds(hWnd);
        cout << bounds.first << " " << bounds.second;

        hdc = BeginPaint(hWnd, &ps);
        DisplayPixArray(hdc, bounds.first, bounds.second, pixels);
        EndPaint(hWnd, &ps);

        cout << " -- painted" << endl;
        break;
    case WM_TIMER:
        switch (wParam)
        {
        case IDT_TIMER1:
            KillTimer(hWnd, IDT_TIMER1);

            bounds = GetBounds(hWnd);
            delete[] pixels;
            pixels = new COLORREF[bounds.first * bounds.second];
            getpixs(bounds.first, bounds.second, mousePos.x, mousePos.y, frame, pixels);
            frame++;
            cout << "frame " << frame << " -- ";

            InvalidateRect(hWnd, 0, TRUE);

            SetTimer(hWnd, IDT_TIMER1, 17, NULL);
            break;
        }
        break;
    case WM_DESTROY:
        KillTimer(hWnd, IDT_TIMER1);
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
    WNDCLASSEX wcex;

    wcex.cbSize = sizeof(WNDCLASSEX);
    wcex.style = CS_HREDRAW | CS_VREDRAW;
    wcex.lpfnWndProc = WndProc;
    wcex.cbClsExtra = 0;
    wcex.cbWndExtra = 0;
    wcex.hInstance = hInst;
    wcex.hIcon = LoadIcon(hInst, IDI_APPLICATION);
    wcex.hCursor = LoadCursor(NULL, IDC_ARROW);
    wcex.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
    wcex.lpszMenuName = NULL;
    wcex.lpszClassName = szWindowClass;
    wcex.hIconSm = LoadIcon(wcex.hInstance, IDI_APPLICATION);

    if (!RegisterClassEx(&wcex))
    {
        MessageBox(NULL, L"Call to RegisterClassEx failed!", L"Pipe", NULL);
        return nullptr;
    }

    HWND hWnd = CreateWindow(szWindowClass, szTitle, WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT, 500, 100, NULL, NULL, hInst, NULL);

    if (!hWnd)
    {
        MessageBox(NULL, L"Call to CreateWindow failed!", L"Pipe", NULL);
        return nullptr;
    }

    SetTimer(hWnd, IDT_TIMER1, 17, NULL);

    ShowWindow(hWnd, SW_SHOWNORMAL);
    UpdateWindow(hWnd);

    MSG msg;
    while (GetMessage(&msg, NULL, 0, 0))
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    return hWnd;
}