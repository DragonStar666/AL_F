static void my_atexit(void)
{
    MyBusHandle *handle;
    MyDevice *dev;
    APTR next;

    if (NULL != app)
    {
        MUI_DisposeObject(app);
    }

    ForeachNodeSafe(&gDeviceList, dev, next)
    {
        Helios_Device_Release(dev->Handle);
        FreeVec(dev->Name);
        FreeMem(dev, sizeof(*dev));
    }

    ForeachNodeSafe(&gBusHandleList, handle, next)
    {
        Helios_Bus_Release(handle->Handle.Bus);
        FreeMem(handle, sizeof(*handle));
    }

    if (NULL != HeliosBase)
    {
        CloseLibrary(HeliosBase);
    }
    if (NULL != gPreviewMCC)
    {
        VideoPreviewMCC_Delete(gPreviewMCC);
    }
    if (NULL != gDecoderMCC)
    {
        DecoderMCC_Delete(gDecoderMCC);
    }
    if (NULL != gCamCtrlMCC)
    {
        CamCtrlMCC_Delete(gCamCtrlMCC);
    }
    if (NULL != gRingBufferMCC)
    {
        RingBufferMCC_Delete(gRingBufferMCC);
    }
}

static LONG fail(char *str)
{
    if (str)
    {
        puts(str);
        exit(20);
    }

    exit(0);
    return 0;
}

BOOL Init(void)
{
    gCamCtrlMCC = CamCtrlMCC_Create();
    if (NULL == gCamCtrlMCC)
    {
        log_Error("CamCtrl MCC creation failed");
        return FALSE;
    }

    gPreviewMCC = VideoPreviewMCC_Create();
    if (NULL == gPreviewMCC)
    {
        log_Error("Preview MCC creation failed");
        return FALSE;
    }

    gDecoderMCC = DecoderMCC_Create();
    if (NULL == gDecoderMCC)
    {
        log_Error("Decoder MCC creation failed");
        return FALSE;
    }

    gRingBufferMCC = RingBufferMCC_Create();
    if (NULL == gRingBufferMCC)
    {
        log_Error("RingBuffer MCC creation failed");
        return FALSE;
    }

    HeliosBase = OpenLibrary("helios.library", 0);
    if (NULL != HeliosBase)
    {
        HeliosBridge *bridge;
        MyBusHandle *handle;
        int i;

        NEWLIST(&gBusHandleList);

        i = gBusCount = 0;
        bridge = NULL;
        while (NULL != (bridge = Helios_Bridge_Next(bridge)))
        {
            handle = AllocMem(sizeof(*handle), MEMF_PUBLIC | MEMF_CLEAR);
            if (NULL != handle)
            {
                HeliosBus *bus;

                bus = Helios_Bridge_Handle(bridge);
                if (NULL != bus)
                {
                    handle->Handle.Bus = bus;
                    if (Helios_BusHandle_Connect(&handle->Handle))
                    {
                        ADDTAIL(&gBusHandleList, handle);
                        gBusCount++;
                    }
                    else
                    {
                        log_Error("Failed to connect on bus %u.", i);
                        Helios_Bus_Release(bus);
                    }
                }
                else
                {
                    log_Error("Failed to handle bus #%u.", i);
                    FreeMem(handle, sizeof(*handle));
                }
            }
            else
            {
                ALLOCFAILURE();
            }

            i++;
        }

        if (gBusCount > 0)
        {
            return TRUE;
        }

        log_Error("No Firewire bus found on this machine!");
    }
    else
    {
        log_Error("can't open helios.library.");
    }

    return FALSE;
}

static MyDevice *CheckNode(HeliosBusHandle *bh, UBYTE nodeid)
{
    HeliosDeviceHandle *dh;
    HeliosRomDirectory *dir;
    MyDevice *dev;

    if (nodeid == bh->Local)
    {
        return NULL;
    }

    dh = Helios_Device_Obtain(bh, HTTAG_NODE_ID, nodeid, TAG_DONE);
    if (NULL != dh)
    {
        if (Helios_Device_GetAttr(dh, HTTAG_ROM_DIR, (ULONG *) &dir))
        {
            if ((0xa02d == dir->unit_spec_id) && (0x01 == (dir->unit_sw_version >> 16)))
            {
                dev = AllocMem(sizeof(*dev), MEMF_PUBLIC | MEMF_CLEAR);
                if (NULL != dev)
                {
                    if (NULL != dir->label)
                    {
                        dev->Name = AllocVec(strlen(dir->label), MEMF_PUBLIC);
                        if (NULL != dev->Name)
                        {
                            strcpy(dev->Name, dir->label);
                        }
                        else
                        {
                            ALLOCFAILURE();
                        }
                    }
                    else
                    {
                        dev->Name = AllocVec(16, MEMF_PUBLIC);
                        if (NULL != dev->Name)
                        {
                            snprintf(dev->Name, 16, "<%.6X-%.6X>", dir->vendor_id, dir->model_id);
                        }
                        else
                        {
                            ALLOCFAILURE();
                        }
                    }

                    if (NULL != dev->Name)
                    {
                        dev->AVC = dir->unit_sw_version & 1;
                        dev->Handle = dh;
                        FreeVec(dir);
                        return dev;
                    }

                    FreeMem(dev, sizeof(*dev));
                }
                else
                {
                    ALLOCFAILURE();
                }
            }

            FreeVec(dir);
        }
        else
        {
            log_Debug("Failed to get node ROM information for node %u.", nodeid);
        }

        Helios_Device_Release(dh);
    }
    else
    {
        log_Debug("Failed to obtain handle for node %u.", nodeid);
    }

    return NULL;
}

static BOOL ProcessHeliosMsg(void)
{
    return TRUE;
}

static void DoGlobalInit(void)
{
    int i;
    MyDevice *dev;
    MyBusHandle *handle;

    NEWLIST(&gDeviceList);

    /* Scan all bus to found valid nodes */
    ForeachNode(&gBusHandleList, handle)
    {
        for (i=0; i < handle->Handle.NodeCount; i++)
        {
            dev = CheckNode(&handle->Handle, i);
            if (NULL != dev)
            {
                dev->Index = gDevCount++;
                ADDTAIL(&gDeviceList, dev);
                DoMethod(obj_CaptureDeviceList, MUIM_List_InsertSingle, dev->Name, dev->Index);
            }
        }
    }
}