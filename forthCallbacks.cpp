
#include "forthCallbacks.h"

void forthInitCallbacks()
{
#if 0
    // WikiReader assembler functions simlib.4th will expect

    ficlDictionarySetPrimitive(g_ficlDictionary, "lcd-line",        ficlCB_LCDLine,       FICL_WORD_DEFAULT);
    ficlDictionarySetPrimitive(g_ficlDictionary, "lcd-set-point",   ficlCB_LCDSetPoint,   FICL_WORD_DEFAULT);
    ficlDictionarySetPrimitive(g_ficlDictionary, "lcd-vram",        ficlCB_VRAMPtr,       FICL_WORD_DEFAULT);
    ficlDictionarySetPrimitive(g_ficlDictionary, "delay-us",        ficlCB_Delayus,       FICL_WORD_DEFAULT);

    // button?
    // button
    // button-flush
    ficlDictionarySetPrimitive(g_ficlDictionary, "button?",         ficlCB_ButtonQuery,   FICL_WORD_DEFAULT);
    ficlDictionarySetPrimitive(g_ficlDictionary, "button",          ficlCB_Button,        FICL_WORD_DEFAULT);
    ficlDictionarySetPrimitive(g_ficlDictionary, "button-flush",    ficlCB_ButtonFlush,   FICL_WORD_DEFAULT);

    // key?
    // key
    // key-flush
    ficlDictionarySetPrimitive(g_ficlDictionary, "key?",            ficlCB_KeyQuery,      FICL_WORD_DEFAULT);
    ficlDictionarySetPrimitive(g_ficlDictionary, "key",             ficlCB_Key,           FICL_WORD_DEFAULT);
    ficlDictionarySetPrimitive(g_ficlDictionary, "key-flush",       ficlCB_KeyFlush,      FICL_WORD_DEFAULT);

    // ctp-pos?
    // ctp-pos
    // ctp-flush
    ficlDictionarySetPrimitive(g_ficlDictionary, "ctp-pos?",        ficlCB_CTPPosQuery,   FICL_WORD_DEFAULT);
    ficlDictionarySetPrimitive(g_ficlDictionary, "ctp-pos",         ficlCB_CTPPos,        FICL_WORD_DEFAULT);
    ficlDictionarySetPrimitive(g_ficlDictionary, "ctp-flush",       ficlCB_CTPFlush,      FICL_WORD_DEFAULT);

    ficlDictionarySetPrimitive(g_ficlDictionary, "wait-for-event",  ficlCB_WaitForEvent,  FICL_WORD_DEFAULT);

    forthCall("load simlib.4th");
    forthCall("load simtest.4th");

    ficlDictionarySetPrimitive(g_ficlDictionary, "vm-lcd-refresh",           ficlCB_VMLCDRefresh,             FICL_WORD_DEFAULT);

    ficlDictionarySetPrimitive(g_ficlDictionary, "vm-lcd-auto-refresh?",     ficlCB_VMLCDAutoRefreshQuery,    FICL_WORD_DEFAULT);
    ficlDictionarySetPrimitive(g_ficlDictionary, "vm-lcd-auto-refresh-on",   ficlCB_VMLCDAutoRefreshOn,       FICL_WORD_DEFAULT);
    ficlDictionarySetPrimitive(g_ficlDictionary, "vm-lcd-auto-refresh-off",  ficlCB_VMLCDAutoRefreshOff,      FICL_WORD_DEFAULT);

    ficlDictionarySetPrimitive(g_ficlDictionary, "vm-lcd-timer-refresh?",    ficlCB_VMLCDTimerRefreshQuery,   FICL_WORD_DEFAULT);
    ficlDictionarySetPrimitive(g_ficlDictionary, "vm-lcd-timer-refresh-on",  ficlCB_VMLCDTimerRefreshOn,      FICL_WORD_DEFAULT);
    ficlDictionarySetPrimitive(g_ficlDictionary, "vm-lcd-timer-refresh-off", ficlCB_VMLCDTimerRefreshOff,     FICL_WORD_DEFAULT);
#endif
}
