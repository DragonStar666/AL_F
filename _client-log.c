static void vLogMsg(ULONG level, STRPTR fmt, va_list va)
{
    if (gLogLevel > level)
    {
        return;
    }

    if (NULL == app)
    {
        vprintf(fmt, va);
    }
    else
    {
        vprintf(fmt, va);
    }
    putchar('\n');
}

static void LogMsg(ULONG level, STRPTR fmt, ...)
{
    va_list va;

    va_start(va, fmt);
    vLogMsg(level, fmt, va);
    va_end(va);
}


void log_Error(STRPTR fmt, ...)
{
    va_list va;

    va_start(va, fmt);
    vLogMsg(LOG_LEVEL_ERROR, fmt, va);
    va_end(va);
}

void log_Debug(STRPTR fmt, ...)
{
    va_list va;

    va_start(va, fmt);
    vLogMsg(LOG_LEVEL_DEBUG, fmt, va);
    va_end(va);
}

void log_Warn(STRPTR fmt, ...)
{
    va_list va;

    va_start(va, fmt);
    vLogMsg(LOG_LEVEL_WARN, fmt, va);
    va_end(va);
}