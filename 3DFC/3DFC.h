#pragma once

#define STRICT
#define UNICODE
#define _UNICODE
#define WIN32
#define _WINDOWS

#include <windows.h>
#include <stdlib.h>
#include <string.h>
#include <tchar.h>
#include <iostream>

HWND WINAPI startwin(void (*)(int, int, int, void *));