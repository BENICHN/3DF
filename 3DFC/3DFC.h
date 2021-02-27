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
#include <chrono>
#include <math.h>
#include "SceneState.h"

HWND WINAPI startwin(void (*)(void *, void *));