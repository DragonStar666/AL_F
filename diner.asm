MEMORY
{
PAGE 0 :  /* Program Memory */
   BEGIN           	: origin = 0x080000, length = 0x000002
   RAMLS0          	: origin = 0x008000, length = 0x000800
   RAMLS1          	: origin = 0x008800, length = 0x000800
   RAMLS2      		: origin = 0x009000, length = 0x000800
   RAMLS3      		: origin = 0x009800, length = 0x000800
   RAMGS14_         : origin = 0x01A000, length = 0x001000  /* non secure, only available on F28379D, F28377D, F28375D devices. Remove line on other devices. */
   RAMGS15_         : origin = 0x01B000, length = 0x001000  /* non secure, only available on F28379D, F28377D, F28375D devices. Remove line on other devices. */
   RESET           	: origin = 0x3FFFC0, length = 0x000002

   /* Flash sectors */
   FLASHABC           : origin = 0x080002, length = 0x005FFE	/* on-chip Flash */

   //FLASHA_           : origin = 0x080002, length = 0x001FFE	/* on-chip Flash */
   //FLASHB_           : origin = 0x082000, length = 0x002000	/* on-chip Flash */
   //FLASHC_           : origin = 0x084000, length = 0x002000	/* on-chip Flash */
   FLASHD_           : origin = 0x086000, length = 0x002000	/* on-chip Flash */
   FLASHE_           : origin = 0x088000, length = 0x008000	/* on-chip Flash */
   FLASHF_           : origin = 0x090000, length = 0x008000	/* on-chip Flash */
   FLASHG_           : origin = 0x098000, length = 0x008000	/* on-chip Flash */
   FLASHH_           : origin = 0x0A0000, length = 0x008000	/* on-chip Flash */
   FLASHI_           : origin = 0x0A8000, length = 0x008000	/* on-chip Flash */
   FLASHJ_           : origin = 0x0B0000, length = 0x008000	/* on-chip Flash */
   FLASHK_           : origin = 0x0B8000, length = 0x002000	/* on-chip Flash */
   FLASHL_           : origin = 0x0BA000, length = 0x002000	/* on-chip Flash */
   FLASHM_           : origin = 0x0BC000, length = 0x002000	/* on-chip Flash */
   FLASHN_           : origin = 0x0BE000, length = 0x002000	/* on-chip Flash */

PAGE 1 : /* Data Memory */
   BOOT_RSVD_       : origin = 0x000002, length = 0x000120	/* Part of M0, BOOT rom will use this for stack */
   RAMM0_           : origin = 0x000122, length = 0x0002DE	/* non secure */
   RAMM1_           : origin = 0x000400, length = 0x000400	/* non secure, on-chip RAM block M1 */
   RAMD0           	: origin = 0x00B000, length = 0x000800
   RAMD1          	: origin = 0x00B800, length = 0x000800

   RAMLS4      		: origin = 0x00A000, length = 0x000800
   RAMLS5      		: origin = 0x00A800, length = 0x000800

   RAMGS0_      	: origin = 0x00C000, length = 0x001000	/* non secure */
   RAMGS1_      	: origin = 0x00D000, length = 0x001000	/* non secure */
   RAMGS2_      	: origin = 0x00E000, length = 0x001000	/* non secure */
   RAMGS3_      	: origin = 0x00F000, length = 0x001000	/* non secure */
   RAMGS4_      	: origin = 0x010000, length = 0x001000	/* non secure */
   RAMGS5_      	: origin = 0x011000, length = 0x001000	/* non secure */
   RAMGS6_      	: origin = 0x012000, length = 0x001000	/* non secure */
   RAMGS7_      	: origin = 0x013000, length = 0x001000	/* non secure */
   RAMGS8_11_   	: origin = 0x014000, length = 0x004000	/* non secure */
   RAMGS12_     	: origin = 0x018000, length = 0x001000   /* non secure, only available on F28379D, F28377D, F28375D devices. Remove line on other devices. */
   RAMGS13_     	: origin = 0x019000, length = 0x001000   /* non secure, only available  on F28379D, F28377D, F28375D devices. Remove line on other devices. */
}

SECTIONS
{
    /* Allocate program areas: */
   .cinit              : > FLASHABC      	PAGE = 0, ALIGN(4)
   .pinit              : > FLASHABC      	PAGE = 0, ALIGN(4)
   .text               : > FLASHABC     	PAGE = 0, ALIGN(4)
   codestart           : > BEGIN      		PAGE = 0, ALIGN(4), { __APP_ENTRY = .;}


   /* Allocate uninitalized data sections: */
   .stack              : > RAMD0 		PAGE = 1
   .ebss               : >> RAMLS4 | RAMLS5     PAGE = 1
   .esysmem            : >> RAMLS4 | RAMLS5     PAGE = 1

   /* Initalized sections go in Flash */
   .econst             : > FLASHABC      PAGE = 0, ALIGN(4)
   .switch             : > FLASHABC      PAGE = 0, ALIGN(4)

   .reset              : > RESET,     PAGE = 0, TYPE = DSECT /* not used, */

   GROUP : LOAD = FLASHABC,
                         RUN = RAMLS0 | RAMLS1 | RAMLS2 |RAMLS3,
                         LOAD_START(_RamfuncsLoadStart),
                         LOAD_SIZE(_RamfuncsLoadSize),
                         LOAD_END(_RamfuncsLoadEnd),
                         RUN_START(_RamfuncsRunStart),
                         RUN_SIZE(_RamfuncsRunSize),
                         RUN_END(_RamfuncsRunEnd),
                         PAGE = 0, ALIGN(4)
   {
  	ramfuncs
#ifdef __TI_COMPILER_VERSION__
#if __TI_COMPILER_VERSION__ >= 15009000
  	.TI.ramfunc
#endif
#endif
   }
}


jump.
    [ORG 0]

            jmp 07C0h:start     ; Goto segment 07C0

    start:
            ; Update the segment registers
            mov ax, cs
            mov ds, ax
            mov es, ax


    reset:                      ; Reset the floppy drive
            mov ax, 0           ;
            mov dl, 0           ; Drive=0 (=A)
            int 13h             ;
            jc reset            ; ERROR => reset again


    read:
            mov ax, 1000h       ; ES:BX = 1000:0000
            mov es, ax          ;
            mov bx, 0           ;

            mov ah, 2           ; Load disk data to ES:BX
            mov al, 5           ; Load 5 sectors
            mov ch, 0           ; Cylinder=0
            mov cl, 2           ; Sector=2
            mov dh, 0           ; Head=0
            mov dl, 0           ; Drive=0
            int 13h             ; Read!

            jc read             ; ERROR => Try again


            jmp 1000h:0000      ; Jump to the program


    times 510-($-$$) db 0
    dw 0AA55h



    ; PROG.ASM

            mov ah, 9
            mov al, '='
            mov bx, 7
            mov cx, 10
            int 10h

    hang:
            jmp hang


ϟ⁅  ⁆⁜⁐⚿⚕⚟⚡⛧⛧⛧⚡〠⋤≬∻∷∻≬⋥

#define NDEBUG
#define DEBUG_SCSI
//#define DEBUG_MEM

#include "sbp2.class.h"

#include <scsi/commands.h>
#include <scsi/values.h>
#include <hardware/byteswap.h>

#include <proto/dos.h>
#include <clib/macros.h>

#include <string.h>

#define SysBase (unit->u_SBP2ClassBase->hc_SysBase)

#ifndef NDEBUG
static const STRPTR scsi_cmd_names[256] =
{
    [0x00]="TEST_UNIT_READY",
    [0x01]="REZERO_UNIT",
    [0x02]="$02",
    [0x03]="REQUEST_SENSE",
    [0x04]="$04",
    [0x05]="$05",
    [0x06]="$06",
    [0x07]="$07",
    [0x08]="READ_6",
    [0x09]="$09",
    [0x0a]="WRITE_6",
    [0x0b]="$0b",
    [0x0c]="$0c",
    [0x0d]="$0d",
    [0x0e]="$0e",
    [0x0f]="$0f",
    [0x10]="$10",
    [0x11]="$11",
    [0x12]="INQUIRY",
    [0x13]="$13",
    [0x14]="$14",
    [0x15]="MODE_SELECT_6",
    [0x16]="$16",
    [0x17]="$17",
    [0x18]="COPY",
    [0x19]="$19",
    [0x1a]="MODE_SENSE_6",
    [0x1b]="START_STOP_UNIT",
    [0x1c]="RECEIVE_DIAGNOSTIC_RESULTS",
    [0x1d]="SEND_DIAGNOSTIC",
    [0x1e]="PREVENT_ALLOW_MEDIUM_REMOVAL",
    [0x1f]="$1f",
    [0x20]="$20",
    [0x21]="$21",
    [0x22]="$22",
    [0x23]="$23",
    [0x24]="$24",
    [0x25]="READ_CAPACITY",
    [0x26]="$26",
    [0x27]="$27",
    [0x28]="READ_10",
    [0x29]="$29",
    [0x2a]="WRITE_10",
    [0x2b]="$2b",
    [0x2c]="$2c",
    [0x2d]="$2d",
    [0x2e]="WRITE_AND_VERIFY_10",
    [0x2f]="$2f",
    [0x30]="$30",
    [0x31]="$31",
    [0x32]="$32",
    [0x33]="$33",
    [0x34]="$34",
    [0x35]="SYNCHRONIZE_CACHE",
    [0x36]="$36",
    [0x37]="$37",
    [0x38]="$38",
    [0x39]="COMPARE",
    [0x3a]="COPY_AND_VERIFY",
    [0x3b]="WRITE_BUFFER",
    [0x3c]="READ_BUFFER",
    [0x3d]="$3d",
    [0x3e]="$3e",
    [0x3f]="$3f",
    [0x40]="CHANGE_DEFINITION",
    [0x41]="$41",
    [0x42]="$42",
    [0x43]="READ_TOC",
    [0x44]="$44",
    [0x45]="$45",
    [0x46]="$46",
    [0x47]="$47",
    [0x48]="$48",
    [0x49]="$49",
    [0x4a]="$4a",
    [0x4b]="$4b",
    [0x4c]="LOG_SELECT",
    [0x4d]="LOG_SENSE",
    [0x4e]="$4e",
    [0x4f]="$4f",
    [0x50]="$50",
    [0x51]="READ_DISC_INFORMATION",
    [0x52]="READ_TRACK_INFORMATION",
    [0x53]="$53",
    [0x54]="$54",
    [0x55]="MODE_SELECT_10",
    [0x56]="$56",
    [0x57]="$57",
    [0x58]="$58",
    [0x59]="$59",
    [0x5a]="MODE_SENSE_10",
    [0x5b]="CLOSE_TRACK/SESSION",
    [0x5c]="$5c",
    [0x5d]="$5d",
    [0x5e]="$5e",
    [0x5f]="$5f",
    [0x60]="$60",
    [0x61]="$61",
    [0x62]="$62",
    [0x63]="$63",
    [0x64]="$64",
    [0x65]="$65",
    [0x66]="$66",
    [0x67]="$67",
    [0x68]="$68",
    [0x69]="$69",
    [0x6a]="$6a",
    [0x6b]="$6b",
    [0x6c]="$6c",
    [0x6d]="$6d",
    [0x6e]="$6e",
    [0x6f]="$6f",
    [0x70]="$70",
    [0x71]="$71",
    [0x72]="$72",
    [0x73]="$73",
    [0x74]="$74",
    [0x75]="$75",
    [0x76]="$76",
    [0x77]="$77",
    [0x78]="$78",
    [0x79]="$79",
    [0x7a]="$7a",
    [0x7b]="$7b",
    [0x7c]="$7c",
    [0x7d]="$7d",
    [0x7e]="$7e",
    [0x7f]="$7f",
    [0x80]="$80",
    [0x81]="$81",
    [0x82]="$82",
    [0x83]="$83",
    [0x84]="$84",
    [0x85]="$85",
    [0x86]="$86",
    [0x87]="$87",
    [0x88]="READ_16",
    [0x89]="$89",
    [0x8a]="WRITE_16",
    [0x8b]="$8b",
    [0x8c]="$8c",
    [0x8d]="$8d",
    [0x8e]="WRITE_AND_VERIFY_16",
    [0x8f]="$8f",
    [0x90]="$90",
    [0x91]="$91",
    [0x92]="$92",
    [0x93]="$93",
    [0x94]="$94",
    [0x95]="$95",
    [0x96]="$96",
    [0x97]="$97",
    [0x98]="$98",
    [0x99]="$99",
    [0x9a]="$9a",
    [0x9b]="$9b",
    [0x9c]="$9c",
    [0x9d]="$9d",
    [0x9e]="$9e",
    [0x9f]="$9f",
    [0xa0]="$a0",
    [0xa1]="BLANK",
    [0xa2]="$a2",
    [0xa3]="$a3",
    [0xa4]="$a4",
    [0xa5]="$a5",
    [0xa6]="$a6",
    [0xa7]="$a7",
    [0xa8]="READ_12",
    [0xa9]="$a9",
    [0xaa]="WRITE_12",
    [0xab]="$ab",
    [0xac]="$ac",
    [0xad]="$ad",
    [0xae]="$ae",
    [0xaf]="$af",
    [0xb0]="$b0",
    [0xb1]="$b1",
    [0xb2]="$b2",
    [0xb3]="$b3",
    [0xb4]="$b4",
    [0xb5]="$b5",
    [0xb6]="$b6",
    [0xb7]="$b7",
    [0xb8]="$b8",
    [0xb9]="$b9",
    [0xba]="$ba",
    [0xbb]="SET_CD_SPEED",
    [0xbc]="$bc",
    [0xbd]="$bd",
    [0xbe]="READ_CD",
    [0xbf]="$bf",
    [0xc0]="$c0",
    [0xc1]="$c1",
    [0xc2]="$c2",
    [0xc3]="$c3",
    [0xc4]="$c4",
    [0xc5]="$c5",
    [0xc6]="$c6",
    [0xc7]="$c7",
    [0xc8]="$c8",
    [0xc9]="$c9",
    [0xca]="$ca",
    [0xcb]="$cb",
    [0xcc]="$cc",
    [0xcd]="$cd",
    [0xce]="$ce",
    [0xcf]="$cf",
    [0xd0]="$d0",
    [0xd1]="$d1",
    [0xd2]="$d2",
    [0xd3]="$d3",
    [0xd4]="$d4",
    [0xd5]="$d5",
    [0xd6]="$d6",
    [0xd7]="$d7",
    [0xd8]="$d8",
    [0xd9]="$d9",
    [0xda]="$da",
    [0xdb]="$db",
    [0xdc]="$dc",
    [0xdd]="$dd",
    [0xde]="$de",
    [0xdf]="$df",
    [0xe0]="$e0",
    [0xe1]="$e1",
    [0xe2]="$e2",
    [0xe3]="$e3",
    [0xe4]="$e4",
    [0xe5]="$e5",
    [0xe6]="$e6",
    [0xe7]="$e7",
    [0xe8]="$e8",
    [0xe9]="$e9",
    [0xea]="$ea",
    [0xeb]="$eb",
    [0xec]="$ec",
    [0xed]="$ed",
    [0xee]="$ee",
    [0xef]="$ef",
    [0xf0]="$f0",
    [0xf1]="$f1",
    [0xf2]="$f2",
    [0xf3]="$f3",
    [0xf4]="$f4",
    [0xf5]="$f5",
    [0xf6]="$f6",
    [0xf7]="$f7",
    [0xf8]="$f8",
    [0xf9]="$f9",
    [0xfa]="$fa",
    [0xfb]="$fb",
    [0xfc]="$fc",
    [0xfd]="$fd",
    [0xfe]="$fe",
    [0xff]="$ff"
};

#endif

/*----------------------------------------------------------------------------*/
/*--- LOCAL CODE SECTION -----------------------------------------------------*/

static LONG sbp2_scsi_getmodepage(SBP2Unit *unit, UBYTE page)
{
    UBYTE cmd10[10];
    struct SCSICmd scsicmd;
    UBYTE sensedata[18];
    LONG ioerr;
    UBYTE *res;

    bzero(unit->u_ModePageBuf, 18);
    bzero(sensedata, sizeof(sensedata));

    scsicmd.scsi_Data = (UWORD *) unit->u_ModePageBuf;
    scsicmd.scsi_Length = 256;
    scsicmd.scsi_Command = cmd10;
    scsicmd.scsi_CmdLength = 10;
    scsicmd.scsi_Flags = SCSIF_READ|SCSIF_AUTOSENSE;
    scsicmd.scsi_SenseData = sensedata;
    scsicmd.scsi_SenseLength = 18;
    cmd10[0] = SCSI_MODE_SENSE_10;
    cmd10[1] = 0x08; /* DisableBlockDescriptor=1 */
    cmd10[2] = page; /* PageControl=current */
    cmd10[3] = 0;    /* Reserved */
    cmd10[4] = 0;    /* Reserved */
    cmd10[5] = 0;    /* Reserved */
    cmd10[6] = 0;    /* Reserved */
    cmd10[7] = 1;    /* Alloc length hi */
    cmd10[8] = 0;    /* Alloc length lo */
    cmd10[9] = 0;    /* Control */

    _INFO_SCSI("do SCSI_MODE_SENSE_10...\n");
    ioerr = sbp2_do_scsi_cmd(unit, &scsicmd, ORB_TIMEOUT);
    if (ioerr)
    {
        _ERR("MODE_SENSE_10: page $%02x failed, ioerr=%ld\n", page, ioerr);
        return -1;
    }

    res = unit->u_ModePageBuf;
    /* check total amount of data */
    if ((scsicmd.scsi_Actual < 10) || (scsicmd.scsi_Actual < (((UWORD *)res)[0]+2U)))
    {
        _ERR("SCSI_MODE_SENSE($%02x) failed: not enought data (get %lu, need %lu)\n",
             page, scsicmd.scsi_Actual, ((UWORD *)res)[0]+2);
        return -1;
    }

    /* skip mode header, jump to page data */
    res += 7;
    scsicmd.scsi_Actual -= 7;

    /* check page code */
    if ((res[0] & 0x3f) != (page & 0x3f))
    {
        _ERR("SCSI_MODE_SENSE($%02x) failed: wrong page returned ($%02x)\n", page, res[0]);
        return -1;
    }

    /* check page length */
    if (scsicmd.scsi_Actual < res[1]+2U)
    {
        _ERR("SCSI_MODE_SENSE($%02x) failed: incomplete page\n", page);
        return -1;
    }

    return 0;
}

static void sbp2_fakegeometry(SBP2Unit *unit)
{
    _INFO("Faking geometry...\n");
    unit->u_Geometry.dg_Heads = 1;
    unit->u_Geometry.dg_TrackSectors = 1;
    unit->u_Geometry.dg_CylSectors = 1;
    unit->u_Geometry.dg_Cylinders = unit->u_Geometry.dg_TotalSectors;
}


/*----------------------------------------------------------------------------*/
/*--- PUBLIC CODE SECTION ----------------------------------------------------*/

LONG sbp2_iocmd_scsi(SBP2Unit *unit, struct IOStdReq *ioreq)
{
    UBYTE cmd10[10];
    struct SCSICmd *cmd, scsicmd10;
    LONG ioerr=0;
    BOOL try_again=FALSE;
    UBYTE *sensedata=NULL;
    UBYTE *buf;

    if (ioreq->io_Length < sizeof(struct SCSICmd))
    {
        _ERR("IO: ioreq data length too small for a SCSI cmd\n");
        ioreq->io_Actual = 0;
        ioerr = IOERR_BADLENGTH;
        goto out;
    }

    cmd = (APTR)ioreq->io_Data;
    if ((NULL == cmd) || (NULL == cmd->scsi_Command) || (12 < cmd->scsi_CmdLength))
    {
        _ERR("IO: no SCSI cmd or unsupported SCSI command length: cmd=%p, len=%u\n",
             cmd->scsi_Command, cmd->scsi_CmdLength);
        ioreq->io_Actual = 0;
        ioerr = IOERR_NOCMD;
        goto out;
    }

    _INFO_SCSI("SCSI[$%02x-%s] Sending, SCSI=[CmdLen=%u, Data=%p, Len=%lu]\n",
               cmd->scsi_Command[0], scsi_cmd_names[cmd->scsi_Command[0]],
               cmd->scsi_CmdLength, cmd->scsi_Data, cmd->scsi_Length);

    /* Force IMMED bit to not fall into sbp2 status timeout */
    switch (cmd->scsi_Command[0])
    {
        case SCSI_CD_BLANK:
            cmd->scsi_Command[1] |= 0x10;
            break;

        case SCSI_CD_CLOSE_TRACK:
        case SCSI_CD_LOAD_UNLOAD_MEDIUM:
            cmd->scsi_Command[1] |= 0x01;
            break;

        case SCSI_CD_SYNCHRONIZE_CACHE:
            cmd->scsi_Command[1] |= 0x02;
            break;
    }

send_cmd:
    ioerr = sbp2_do_scsi_cmd(unit, cmd, ORB_TIMEOUT);
    if (!ioerr)
    {
        ioreq->io_Actual = ioreq->io_Length;
        _INFO_SCSI("SCSI[$%02x-%s] OK, IO=[actual=%lu], SCSI=[Actual=%lu]\n",
                   cmd->scsi_Command[0], scsi_cmd_names[cmd->scsi_Command[0]],
                   ioreq->io_Actual, cmd->scsi_Actual);
    }
    else
    {
        _ERR_SCSI("SCSI[$%02x-%s] Failed, IO=[err=%ld, actual=%lu], SCSI=[Status=$%02x, Actual=%lu]\n",
                  cmd->scsi_Command[0], scsi_cmd_names[cmd->scsi_Command[0]],
                  ioerr, ioreq->io_Actual, cmd->scsi_Status, cmd->scsi_Actual);
        if (cmd->scsi_SenseActual > 13)
        {
            _ERR_SCSI("SCSI[$%02x-%s] Failed, SenseLen=%u, SenseData=[key=$%x, asc/ascq=$%02x/$%02x]\n",
                      cmd->scsi_Command[0], scsi_cmd_names[cmd->scsi_Command[0]],
                      cmd->scsi_SenseActual, cmd->scsi_SenseData[2] & SK_MASK,
                      cmd->scsi_SenseData[12], cmd->scsi_SenseData[13]);
        }
        switch (cmd->scsi_CmdLength)
        {
            case 6:
                _ERR_SCSI("SCSI cmd6 [%02x %02x %02x %02x %02x %02x]\n",
                          cmd->scsi_Command[0], cmd->scsi_Command[1], cmd->scsi_Command[2],
                          cmd->scsi_Command[3], cmd->scsi_Command[4], cmd->scsi_Command[5]);
                break;

            case 10:
                _ERR_SCSI("SCSI cmd10 [%02x %02x %02x %02x %02x %02x %02x %02x %02x %02x]\n",
                          cmd->scsi_Command[0], cmd->scsi_Command[1], cmd->scsi_Command[2],
                          cmd->scsi_Command[3], cmd->scsi_Command[4], cmd->scsi_Command[5],
                          cmd->scsi_Command[6], cmd->scsi_Command[7], cmd->scsi_Command[8],
                          cmd->scsi_Command[9]);
                break;

            case 12:
                _ERR_SCSI("SCSI cmd12 [%02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x]\n",
                          cmd->scsi_Command[0], cmd->scsi_Command[1], cmd->scsi_Command[2],
                          cmd->scsi_Command[3], cmd->scsi_Command[4], cmd->scsi_Command[5],
                          cmd->scsi_Command[6], cmd->scsi_Command[7], cmd->scsi_Command[8],
                          cmd->scsi_Command[9], cmd->scsi_Command[10], cmd->scsi_Command[11]);
                break;
        }

        if (!try_again)
        {
            if ((HFERR_BadStatus == ioerr) && (6 == cmd->scsi_CmdLength))
            {
                try_again = TRUE;

                CopyMem(cmd, &scsicmd10, sizeof(scsicmd10));

                switch (cmd->scsi_Command[0])
                {
                    case SCSI_MODE_SENSE_6:
                        cmd10[0] = SCSI_MODE_SENSE_10;
                        cmd10[1] = cmd->scsi_Command[1] & 0xf7;
                        cmd10[2] = cmd->scsi_Command[2];
                        cmd10[3] = 0;
                        cmd10[4] = 0;
                        cmd10[5] = 0;
                        cmd10[6] = 0;
                        if ((cmd->scsi_Command[4] > 251) &&
                            (cmd->scsi_Length == cmd->scsi_Command[4]))
                        {
                            cmd->scsi_Command[4] -= 4;
                            cmd->scsi_Length -= 4;
                        }
                        cmd10[7] = (cmd->scsi_Command[4]+4)>>8;
                        cmd10[8] = cmd->scsi_Command[4]+4;
                        cmd10[9] = cmd->scsi_Command[5];

                        sensedata = AllocVec(cmd->scsi_Length+4, MEMF_PUBLIC|MEMF_CLEAR);
                        if (NULL != sensedata)
                        {
                            scsicmd10.scsi_Length = cmd->scsi_Length+4;
                            scsicmd10.scsi_Data = (UWORD *) sensedata;
                        }
                        break;

                    case SCSI_MODE_SELECT_6:
                        cmd10[0] = SCSI_MODE_SELECT_10;
                        cmd10[1] = cmd->scsi_Command[1];
                        cmd10[2] = cmd->scsi_Command[2];
                        cmd10[3] = 0;
                        cmd10[4] = 0;
                        cmd10[5] = 0;
                        cmd10[6] = 0;
                        cmd10[7] = (cmd->scsi_Command[4]+4)>>8;
                        cmd10[8] = cmd->scsi_Command[4]+4;
                        cmd10[9] = cmd->scsi_Command[5];

                        sensedata = AllocVec(cmd->scsi_Length+4, MEMF_PUBLIC|MEMF_CLEAR);
                        if((NULL != sensedata) && (cmd->scsi_Length >= 4))
                        {
                            buf = (UBYTE *) cmd->scsi_Data;

                            sensedata[1] = *buf++;
                            sensedata[2] = *buf++;
                            sensedata[3] = *buf++;
                            sensedata[7] = *buf++;

                            scsicmd10.scsi_Length = cmd->scsi_Length+4;
                            scsicmd10.scsi_Data = (UWORD *) sensedata;

                            CopyMem(buf, &sensedata[8], cmd->scsi_Length-4);
                        }
                        break;

                    default:
                        try_again = FALSE;
                }

                if (try_again)
                {
                    _WARN("Xlate 6->10 cmd %s\n",
                          scsi_cmd_names[((struct SCSICmd *)ioreq->io_Data)->scsi_Command[0]]);

                    cmd = &scsicmd10;
                    cmd->scsi_Command = cmd10;
                    cmd->scsi_CmdLength = 10;
                    cmd->scsi_Status = 0;
                    cmd->scsi_Actual = 0;
                    cmd->scsi_SenseActual = 0;
                    goto send_cmd;
                }
            }
        }
    }

    if (try_again)
    {
        cmd = (APTR)ioreq->io_Data;

        if (NULL != sensedata)
        {
            cmd->scsi_Actual = 0;
            if(scsicmd10.scsi_Command[0] == SCSI_MODE_SENSE_10)
            {
                if(scsicmd10.scsi_Actual >= 8)
                {
                    cmd->scsi_Actual = scsicmd10.scsi_Actual - 4;
                    buf = (UBYTE *) cmd->scsi_Data;
                    *buf++ = sensedata[1];
                    *buf++ = sensedata[2];
                    *buf++ = sensedata[3];
                    *buf++ = sensedata[7];
                    CopyMem(&sensedata[8], buf, (ULONG) scsicmd10.scsi_Actual - 8);
                }
            }
            FreeVec(sensedata);
        }
        else
        {
            cmd->scsi_Actual = scsicmd10.scsi_Actual;
        }

        cmd->scsi_CmdActual = scsicmd10.scsi_CmdActual;
        cmd->scsi_Status = scsicmd10.scsi_Status;
        cmd->scsi_SenseActual = scsicmd10.scsi_SenseActual;
    }

out:
    ioreq->io_Error = ioerr;
    return ioerr;
}

LONG sbp2_iocmd_start_stop(SBP2Unit *unit, struct IOStdReq *ioreq)
{
    UBYTE cmd6[6];
    struct SCSICmd scsicmd;
    UBYTE sensedata[18];

    _INFO_SCSI("IO: CMD_START/CMD_STOP\n");

    bzero(sensedata, sizeof(sensedata));
    bzero(cmd6, 6);
    ioreq->io_Actual = 0;

    scsicmd.scsi_Data = NULL;
    scsicmd.scsi_Length = 0;
    scsicmd.scsi_Command = cmd6;
    scsicmd.scsi_CmdLength = 6;
    scsicmd.scsi_Flags = SCSIF_AUTOSENSE;
    scsicmd.scsi_SenseData = sensedata;
    scsicmd.scsi_SenseLength = 18;

    cmd6[0] = SCSI_DA_START_STOP_UNIT;

    switch(ioreq->io_Command)
    {
        case CMD_START:
            cmd6[4] = 0x01;
            break;

        case CMD_STOP:
            cmd6[4] = 0x00;
            break;

        case TD_EJECT:
            cmd6[4] = ioreq->io_Length ? 0x03 : 0x02;
            break;
    }

    _INFO_SCSI("do SCSI_DA_START_STOP_UNIT...\n");
    ioreq->io_Error = sbp2_do_scsi_cmd(unit, &scsicmd, ORB_TIMEOUT);
    return ioreq->io_Error;
}

LONG sbp2_iocmd_read64(SBP2Unit *unit, struct IOStdReq *ioreq)
{
    UBYTE cmd10[10];
    struct SCSICmd scsicmd;
    UBYTE sensedata[18];
    LONG err;

    ULONG datalen, dataremain = ioreq->io_Length;
    ULONG maxtrans = 1ul<<21; /* TODO: how to set the max transfer ? */
    ULONG insideblockoffset, startblock, dataoffset=0;
    UQUAD offset64;

    offset64 = ((UQUAD)ioreq->io_HighOffset<<32) + ioreq->io_LowOffset;

    if (0xff == unit->u_Read10Cmd)
    {
        ioreq->io_Actual = 0;
        ioreq->io_Error = IOERR_NOCMD;
        return ioreq->io_Error;
    }

    _INFO("Flags=$%02x, Data=%p, Length=%lu, Offset=$%llx\n",
          ioreq->io_Flags, ioreq->io_Data, ioreq->io_Length, offset64);

    if(dataremain & 511)
    {
        _ERR("Attempt to read partial block (%lu %% %lu != 0)!",
             dataremain, unit->u_BlockSize);
        ioreq->io_Actual = 0;
        ioreq->io_Error = IOERR_BADLENGTH;
        return ioreq->io_Error;
    }

    if(ioreq->io_LowOffset & 511)
    {
        _ERR("Attempt to read unaligned block (%lu %% %lu != 0)!",
             ioreq->io_LowOffset, unit->u_BlockSize);
        ioreq->io_Actual = 0;
        ioreq->io_Error = IOERR_BADADDRESS;
        return ioreq->io_Error;
    }

    if (unit->u_OneBlockSize < unit->u_BlockSize)
    {
        if (NULL != unit->u_OneBlock)
        {
            FreePooled(unit->u_SBP2ClassBase->hc_MemPool, unit->u_OneBlock, unit->u_OneBlockSize);
        }
        unit->u_OneBlock = AllocPooled(unit->u_SBP2ClassBase->hc_MemPool, unit->u_BlockSize);
        if (NULL == unit->u_OneBlock)
        {
            _ERR("Alloc block failed\n");
            ioreq->io_Actual = 0;
            ioreq->io_Error = IOERR_NOMEMORY;
            return ioreq->io_Error;
        }

        unit->u_OneBlockSize = unit->u_BlockSize;
    }

    /* This is the idea of operation:
     * the READ_10 scsi command permits to transfer zero or more blocks.
     * So we need to give the address of the start block and a block count.
     * Now the IO requests a buffer not aligned on block and of any size.
     * So we're going to split this buffer into block transfer and handles
     * non read used border data by doing mem copy from a temporary buffer.
     *
     * As the transfer length is a number of block, given as a 16-bits value,
     * we can't read more than 65,535 blocks per READ_10 command.
     * Most of Direct-Access Block Devices support a block length of 512 bytes.
     * That gives 33,553,920 bytes! Unfortunatly most of these devices doesn't
     * support this number of bytes per command.
     * That's why we also limit the total number of transfered bytes.
     *
     * Other thing: even if offset is a 64bit value, as the max logical blocks
     * is a 32bits value, so startblock is also a 32bit value.
     */

    startblock = offset64 >> unit->u_BlockShift;
    insideblockoffset = (ioreq->io_LowOffset & ((1ul << unit->u_BlockShift)-1));
    while (dataremain)
    {
        /* Limit transfer size */
        datalen = MIN(dataremain, maxtrans);

        _INFO("Reading %lu bytes from block %ld, %ld bytes left...\n", datalen, startblock, dataremain);

        /* if offset is not block aligned or xfer size is less than a block size */
        if (insideblockoffset || (datalen < unit->u_BlockSize))
        {
            /* now limit xfert size to the block size */
            if (datalen > unit->u_BlockSize - insideblockoffset)
            {
                datalen = unit->u_BlockSize - insideblockoffset;
            }

            scsicmd.scsi_Data = (UWORD *) unit->u_OneBlock;
            scsicmd.scsi_Length = unit->u_BlockSize;
            scsicmd.scsi_Command = cmd10;
            scsicmd.scsi_CmdLength = 10;
            scsicmd.scsi_Flags = SCSIF_READ|SCSIF_AUTOSENSE;
            scsicmd.scsi_SenseData = sensedata;
            scsicmd.scsi_SenseLength = 18;
            cmd10[0] = SCSI_DA_READ_10;
            cmd10[1] = 0;
            *((ULONG *) (&cmd10[2])) = LE_SWAPLONG(startblock);
            cmd10[6] = 0;
            cmd10[7] = 0;
            cmd10[8] = 1;
            cmd10[9] = 0;

            err = sbp2_do_scsi_cmd(unit, &scsicmd, ORB_TIMEOUT);
            if (err)
            {
                ioreq->io_Error = err;
                goto out;
            }

            if (((ULONG)&unit->u_OneBlock[insideblockoffset] & 3) ||
                ((ULONG)&(((UBYTE *) ioreq->io_Data)[dataoffset]) & 3) ||
                (datalen & 3))
            {
                _WARN("CopyMemQuick() will fails\n");
            }
            CopyMemQuick(&unit->u_OneBlock[insideblockoffset],
                         &(((UBYTE *) ioreq->io_Data)[dataoffset]),
                         datalen);
            insideblockoffset = 0;
            startblock++;
        }
        else
        {
            scsicmd.scsi_Data = (UWORD *) &(((UBYTE *) ioreq->io_Data)[dataoffset]);
            scsicmd.scsi_Length = datalen;
            scsicmd.scsi_Command = cmd10;
            scsicmd.scsi_CmdLength = 10;
            scsicmd.scsi_Flags = SCSIF_READ|SCSIF_AUTOSENSE;
            scsicmd.scsi_SenseData = sensedata;
            scsicmd.scsi_SenseLength = 18;
            cmd10[0] = SCSI_DA_READ_10;
            cmd10[1] = 0;
            *((ULONG *) (&cmd10[2])) = LE_SWAPLONG(startblock);
            cmd10[6] = 0;
            cmd10[7] = datalen >> (unit->u_BlockShift+8);
            cmd10[8] = datalen >> unit->u_BlockShift;
            cmd10[9] = 0;

            err = sbp2_do_scsi_cmd(unit, &scsicmd, ORB_TIMEOUT);
            if (err)
            {
                ioreq->io_Error = err;
                goto out;
            }

            startblock += (insideblockoffset+datalen) >> unit->u_BlockShift;
        }

        dataoffset += datalen;
        dataremain -= datalen;
    }

out:
    ioreq->io_Actual = dataoffset;
    return ioreq->io_Error;
}

LONG sbp2_iocmd_write64(SBP2Unit *unit, struct IOStdReq *ioreq)
{
    UBYTE cmd10[10];
    struct SCSICmd scsicmd;
    UBYTE sensedata[18];
    LONG err;

    ULONG datalen, dataremain = ioreq->io_Length;
    ULONG maxtrans = 1ul<<21; /* TODO: how to set the max transfer ? */
    ULONG insideblockoffset, startblock, dataoffset=0;
    UQUAD offset64;

    offset64 = ((UQUAD)ioreq->io_HighOffset<<32) + ioreq->io_LowOffset;

    if (unit->u_Flags.WriteProtected)
    {
        ioreq->io_Actual = 0;
        ioreq->io_Error = IOERR_NOCMD;
        return ioreq->io_Error;
    }

    _INFO("Flags=$%02x, Data=%p, Length=%lu, Offset=$%llx\n",
          ioreq->io_Flags, ioreq->io_Data, ioreq->io_Length, offset64);

    if(dataremain & 511)
    {
        _ERR("Attempt to write partial block (%lu %% %lu != 0)!",
             dataremain, unit->u_BlockSize);
        ioreq->io_Actual = 0;
        ioreq->io_Error = IOERR_BADLENGTH;
        return ioreq->io_Error;
    }

    if(ioreq->io_LowOffset & 511)
    {
        _ERR("Attempt to write unaligned block (%lu %% %lu != 0)!",
             ioreq->io_LowOffset, unit->u_BlockSize);
        ioreq->io_Actual = 0;
        ioreq->io_Error = IOERR_BADADDRESS;
        return ioreq->io_Error;
    }

    if (unit->u_OneBlockSize < unit->u_BlockSize)
    {
        if (NULL != unit->u_OneBlock)
        {
            FreePooled(unit->u_SBP2ClassBase->hc_MemPool, unit->u_OneBlock, unit->u_OneBlockSize);
        }
        unit->u_OneBlock = AllocPooled(unit->u_SBP2ClassBase->hc_MemPool, unit->u_BlockSize);
        if (NULL == unit->u_OneBlock)
        {
            _ERR("Alloc block failed\n");
            ioreq->io_Actual = 0;
            ioreq->io_Error = IOERR_NOMEMORY;
            return ioreq->io_Error;
        }

        unit->u_OneBlockSize = unit->u_BlockSize;
    }

    /* Same remarks as READ64 */

    startblock = offset64 >> unit->u_BlockShift;
    insideblockoffset = (ioreq->io_LowOffset & ((1ul << unit->u_BlockShift)-1));
    while (dataremain)
    {
        /* Limit transfer size */
        datalen = MIN(dataremain, maxtrans);

        _INFO("Writing %lu bytes from block %ld, %ld bytes left...\n", datalen, startblock, dataremain);

        /* if offset is not block aligned or xfer size is less than a block size */
        if (insideblockoffset || (datalen < unit->u_BlockSize))
        {
            /* now limit xfert size to the block size */
            if (datalen > unit->u_BlockSize - insideblockoffset)
            {
                datalen = unit->u_BlockSize - insideblockoffset;
            }

            scsicmd.scsi_Data = (UWORD *) unit->u_OneBlock;
            scsicmd.scsi_Length = unit->u_BlockSize;
            scsicmd.scsi_Command = cmd10;
            scsicmd.scsi_CmdLength = 10;
            scsicmd.scsi_Flags = SCSIF_READ|SCSIF_AUTOSENSE;
            scsicmd.scsi_SenseData = sensedata;
            scsicmd.scsi_SenseLength = 18;
            cmd10[0] = SCSI_DA_READ_10;
            cmd10[1] = 0;
            *((ULONG *) (&cmd10[2])) = LE_SWAPLONG(startblock);
            cmd10[6] = 0;
            cmd10[7] = 0;
            cmd10[8] = 1;
            cmd10[9] = 0;

            err = sbp2_do_scsi_cmd(unit, &scsicmd, ORB_TIMEOUT);
            if (err)
            {
                ioreq->io_Error = err;
                goto out;
            }

            CopyMemQuick(&(((UBYTE *) ioreq->io_Data)[dataoffset]),
                         &unit->u_OneBlock[insideblockoffset],
                         datalen);

            //scsicmd.scsi_Data = (UWORD *) unit->u_OneBlock;
            //scsicmd.scsi_Length = unit->u_BlockSize;
            //scsicmd.scsi_Command = cmd10;
            //scsicmd.scsi_CmdLength = 10;
            scsicmd.scsi_Flags = SCSIF_WRITE|SCSIF_AUTOSENSE;
            //scsicmd.scsi_SenseData = sensedata;
            //scsicmd.scsi_SenseLength = 18;
            cmd10[0] = SCSI_DA_WRITE_10;
            //cmd10[1] = 0;
            //*((ULONG *) (&cmd10[2])) = LE_SWAPLONG(startblock);
            //cmd10[6] = 0;
            //cmd10[7] = 0;
            //cmd10[8] = 1;
            //cmd10[9] = 0;

            err = sbp2_do_scsi_cmd(unit, &scsicmd, ORB_TIMEOUT);
            if (err)
            {
                ioreq->io_Error = err;
                goto out;
            }

            insideblockoffset = 0;
            startblock++;
        }
        else
        {
            scsicmd.scsi_Data = (UWORD *) &(((UBYTE *) ioreq->io_Data)[dataoffset]);
            scsicmd.scsi_Length = datalen;
            scsicmd.scsi_Command = cmd10;
            scsicmd.scsi_CmdLength = 10;
            scsicmd.scsi_Flags = SCSIF_WRITE|SCSIF_AUTOSENSE;
            scsicmd.scsi_SenseData = sensedata;
            scsicmd.scsi_SenseLength = 18;
            cmd10[0] = SCSI_DA_WRITE_10;
            cmd10[1] = 0;
            *((ULONG *) (&cmd10[2])) = LE_SWAPLONG(startblock);
            cmd10[6] = 0;
            cmd10[7] = datalen >> (unit->u_BlockShift+8);
            cmd10[8] = datalen >> unit->u_BlockShift;
            cmd10[9] = 0;

            err = sbp2_do_scsi_cmd(unit, &scsicmd, ORB_TIMEOUT);
            if (err)
            {
                ioreq->io_Error = err;
                goto out;
            }

            startblock += (insideblockoffset+datalen) >> unit->u_BlockShift;
        }

        dataoffset += datalen;
        dataremain -= datalen;
    }

out:
    ioreq->io_Actual = dataoffset;
    return ioreq->io_Error;
}

LONG sbp2_iocmd_get_geometry(SBP2Unit *unit, struct IOStdReq *ioreq)
{
    struct DriveGeometry *dg;
    ULONG length;
    //ULONG tmpval;
    BOOL gotblks = FALSE;
    BOOL gotcyl = FALSE;
    BOOL gotheads = FALSE;
    BOOL gotsect = FALSE;
    BOOL gotcylsect = FALSE;

    _INFO("IO: TD_GETGEOMETRY\n");

    ioreq->io_Actual = 0;
    ioreq->io_Error = 0;

    dg = (struct DriveGeometry *) ioreq->io_Data;
    if(NULL == dg)
    {
        _ERR("NULL io_Data\n");
        ioreq->io_Error = TDERR_NotSpecified;
        return ioreq->io_Error;
    }

    length = MIN(ioreq->io_Length, sizeof(struct DriveGeometry));

    if (unit->u_Geometry.dg_Cylinders > 0)
    {
        goto geo_ok;
    }

#if 0
    if (sbp2_scsi_read_capacity(unit, dev))
    {
        _ERR("ReadCapacity failed\n");
        ioreq->io_Error = TDERR_NotSpecified;
        return ioreq->io_Error;
    }
#endif

    if (0 == unit->u_BlockSize)
    {
        _ERR("Zero BlockSize!\n");
        ioreq->io_Error = TDERR_NotSpecified;
        return ioreq->io_Error;
    }

    gotblks = TRUE;

#if 0
    if (!sbp2_scsi_getmodepage(unit, 0x03))
    {
        tmpval = (unit->u_ModePageBuf[10] << 8) + unit->u_ModePageBuf[11];
        if (tmpval)
        {
            unit->u_Geometry.dg_TrackSectors = tmpval;
            gotsect = TRUE;

            if (!unit->u_Geometry.dg_Cylinders)
            {
                unit->u_Geometry.dg_Cylinders = unit->u_Geometry.dg_TotalSectors;
            }
        }
    }

    if (!sbp2_scsi_getmodepage(unit, 0x04))
    {
        tmpval = (unit->u_ModePageBuf[2] << 16) + (unit->u_ModePageBuf[3] << 8) + unit->u_ModePageBuf[4];
        if (tmpval)
        {
            unit->u_Geometry.dg_Cylinders = tmpval;
            unit->u_Geometry.dg_Heads = unit->u_ModePageBuf[5];
            gotcyl = gotheads = TRUE;
        }
    }

    if ((PDT_CDROM != unit->u_DeviceType) && (PDT_WORM != unit->u_DeviceType))
    {
        /* CD parameter page */
        if (!sbp2_scsi_getmodepage(unit, 0x05))
        {
            tmpval = (unit->u_ModePageBuf[4] << 8) + unit->u_ModePageBuf[5];
            if (tmpval)
            {
                unit->u_Geometry.dg_Cylinders = tmpval;
                unit->u_Geometry.dg_Heads = unit->u_ModePageBuf[4];
                unit->u_Geometry.dg_TrackSectors = unit->u_ModePageBuf[5];

                gotcyl = gotheads = gotsect = TRUE;

                if (!gotblks)
                {
                    unit->u_BlockSize = unit->u_Geometry.dg_SectorSize = (unit->u_ModePageBuf[6] << 8) + unit->u_ModePageBuf[7];
                    unit->u_BlockShift = 0;
                    while ((1ul << unit->u_BlockShift) < unit->u_BlockSize)
                    {
                        unit->u_BlockShift++;
                    }

                    _ERR("Capacity: %lu blocks of %lu bytes\n", unit->u_Geometry.dg_TotalSectors, unit->u_BlockSize);
                }
                else if (unit->u_BlockSize != (unit->u_ModePageBuf[6] <<8 ) + unit->u_ModePageBuf[7])
                    _ERR("Inconsistent block size information %lu != %lu!\n",
                         unit->u_BlockSize, (unit->u_ModePageBuf[6] << 8) + unit->u_ModePageBuf[7]);
            }
        }
    }
#endif

    _INFO("Capacity (temp): Cylinders=%lu, Heads=%lu, TrackSectors=%u\n",
          unit->u_Geometry.dg_Cylinders, unit->u_Geometry.dg_Heads,
          unit->u_Geometry.dg_TrackSectors);

    /* missing total sectors? */
    if ((!gotblks) && gotcyl && gotheads && gotsect)
    {
        unit->u_Geometry.dg_TotalSectors = unit->u_Geometry.dg_Cylinders * unit->u_Geometry.dg_Heads * unit->u_Geometry.dg_TrackSectors;
        gotblks = TRUE;
    }

    /* missing cylinders? */
    if (gotblks && (!gotcyl) && gotheads && gotsect)
    {
        unit->u_Geometry.dg_Cylinders = unit->u_Geometry.dg_TotalSectors / (unit->u_Geometry.dg_Heads * unit->u_Geometry.dg_TrackSectors);
        gotcyl = TRUE;
    }

    /* missing heads? */
    if (gotblks && gotcyl && (!gotheads) && gotsect)
    {
        unit->u_Geometry.dg_Heads = unit->u_Geometry.dg_TotalSectors / (unit->u_Geometry.dg_Cylinders * unit->u_Geometry.dg_TrackSectors);
        gotheads = TRUE;
    }

    /* missing tracks per sector */
    if (gotblks && gotcyl && gotheads && (!gotsect))
    {
        unit->u_Geometry.dg_TrackSectors = unit->u_Geometry.dg_TotalSectors / (unit->u_Geometry.dg_Cylinders * unit->u_Geometry.dg_Heads);
        gotsect = TRUE;
    }

    /* some devices report these bogus values regardless of actual device capacity,
     * though the total number of blocks is correct.
     */
    if (((unit->u_Geometry.dg_Cylinders == 500) && (unit->u_Geometry.dg_TrackSectors == 32) && (unit->u_Geometry.dg_Heads == 8)) ||
        ((unit->u_Geometry.dg_Cylinders == 16383) && (unit->u_Geometry.dg_TrackSectors == 63) && (unit->u_Geometry.dg_Heads == 16)))
    {
        _ERR("Firmware returns known bogus geometry, will fall back to faked geometry!\n");
        gotheads = gotcyl = gotsect = FALSE;
    }

    /* missing more than one? */
    if (gotblks && !(gotheads && gotcyl && gotsect))
    {
        sbp2_fakegeometry(unit);
    }

    if (!gotcylsect)
    {
        unit->u_Geometry.dg_CylSectors = unit->u_Geometry.dg_TrackSectors * unit->u_Geometry.dg_Heads;
    }

    if (unit->u_DeviceType == PDT_SIMPLE_DIRECT_ACCESS)
    {
        unit->u_Geometry.dg_DeviceType = DG_DIRECT_ACCESS;
    }
    else if (unit->u_DeviceType < 10)
    {
        unit->u_Geometry.dg_DeviceType = unit->u_DeviceType;
    }
    else
    {
        unit->u_Geometry.dg_DeviceType = DG_UNKNOWN;
    }

    unit->u_Geometry.dg_Flags = unit->u_Flags.Removable ? DGF_REMOVABLE : 0;
    unit->u_Geometry.dg_BufMemType = MEMF_PUBLIC;
    unit->u_Geometry.dg_Reserved = 0;

geo_ok:
    if (unit->u_Geometry.dg_Cylinders * unit->u_Geometry.dg_CylSectors != unit->u_Geometry.dg_TotalSectors)
        _INFO("Estimated Geometry yields %lu less total blocks %lu: Cylinders=%lu, CylSectors=%lu, Heads=%lu, TrackSectors=%lu, Blocks=%lu\n",
              unit->u_Geometry.dg_TotalSectors - unit->u_Geometry.dg_Cylinders * unit->u_Geometry.dg_CylSectors,
              unit->u_Geometry.dg_Cylinders * unit->u_Geometry.dg_CylSectors,
              unit->u_Geometry.dg_Cylinders, unit->u_Geometry.dg_CylSectors,
              unit->u_Geometry.dg_Heads, unit->u_Geometry.dg_TrackSectors,
              unit->u_Geometry.dg_TotalSectors);
    else
        _INFO("Capacity (final): Cylinders=%lu, CylSectors=%lu, Heads=%lu, TrackSectors=%lu, Blocks=%lu, SectorSize=%lu\n",
              unit->u_Geometry.dg_Cylinders, unit->u_Geometry.dg_CylSectors,
              unit->u_Geometry.dg_Heads, unit->u_Geometry.dg_TrackSectors,
              unit->u_Geometry.dg_TotalSectors, unit->u_Geometry.dg_SectorSize);

    CopyMem(&unit->u_Geometry, dg, length);
    ioreq->io_Actual = length;

    return ioreq->io_Error;
}


/* EOF */



__UNI__::__HASH_DUMP__::#[
6a	6d	31	f6	d4	36	cb	df	fb	5f	0a	29	53	b9	ae	bd
43	69	f9	b4	74	b1	5c	9a	6a	b6	94	fa	2f	61	aa	e9
a1	fa	48	45	fe	c0	00	dc	a3	b4	30	90	ea	4a	bf	cb
8a	f6	d7	31	64	57	ff	6f	6e	b7	82	17	41	8f	f0	94
d9	5e	85	f0	53	01	37	05	e8	e3	e1	36	49	fc	c8	cf
53	57	2f	23	ee	55	59	af	f1	01	bb	74	5c	b0	4c	8a
25	ce	e6	e3	c4	f0	b2	86	8c	20	fd	bd	21	97	fe	5b
3a	ed	f4	0e	fc	94	be	e0	e4	2d	ee	79	7a	31	9e	a8
65	67	7c	25	e5	9d	23	9c	18	d6	23	79	f4	72	31	d6
bd	15	7a	bc	c4	a7	9f	97	91	e3	0b	5c	d4	94	48	39
73	65	d5	1a	ee	c5	f7	21	fe	22	01	5f	36	36	3a	27
a2	e9	54	94	1d	77	72	1b	f8	45	0c	a5	79	38	10	24
ee	be	44	4e	93	dc	de	35	80	85	37	26	f3	36	65	65
45	92	44	95	81	b5	29	43	54	36	0f	19	ee	14	2a	c7
e5	3a	9b	76	7f	60	a0	28	8b	c7	00	80	a0	b0	36	e2
c9	71	66	ba	57	bf	68	4b	85	b0	9d	b1	ba	1b	b5	14
ae	96	dd	fb	60	48	1d	b0	c8	a8	9c	9c	8c	54	e1	24
87	1f	15	42	52	5f	4a	02	4d	dc	08	ed	57	8b	bd	1f
61	5b	8d	e7	d4	1a	f6	07	f7	91	e5	91	bd	b2	04	ad
58	08	c9	6e	cc	25	9e	d4	ac	16	67	db	7e	41	dd	e4
09	d2	c2	6a	c7	1e	aa	18	96	c8	c1	4a	ad	26	b9	38
9e	04	23	73	26	3c	cf	4d	fc	20	2b	07	ab	e3	cf	b1
86	29	a0	ed	a2	1e	09	f1	d7	c7	38	c8	81	f9	6e	57
58	94	73	cf	65	8d	2f	ba	2b	69	5f	b3	f7	5c	ed	a9
8d	3f	78	eb	3a	43	8c	72	a8	6b	f5	78	36	5a	3e	3d
22	a8	b3	66	07	ce	f9	0d	9e	40	6d	93	07	b9	52	8d
35	86	d8	73	ed	86	58	c5	46	97	54	b3	00	1b	31	82
2a	ec	66	2b	a8	7c	86	06	b7	70	c6	12	d4	df	f1	ea
97	56	a7	fa	5d	d0	98	3e	e4	58	72	9e	43	9e	b8	50
71	da	e1	93	da	93	ea	db	bb	1e	0e	e9	c2	e8	45	f4
f8	5d	66	b9	e6	b8	36	48	44	45	15	3a	51	9c	4c	fb
b1	7a	3c	d1	b2	ca	28	eb	82	1b	b8	62	65	03	e1	ee
84	8a	27	e9	c9	01	b9	1b	60	34	e8	75	2b	66	c1	4b
b5	a4	47	00	0f	78	2d	1e	c0	24	c5	99	36	64	da	9d
00	b1	08	86	c7	d7	1a	e3	92	d1	26	92	79	91	6b	9a
fa	bc	fa	a9	b7	30	90	81	ac	00	06	25	b2	d6	50	6b
aa	eb	e6	a5	66	d2	76	9c	0c	60	3d	40	e8	22	82	38
b1	ca	9c	b3	67	24	a5	73	c3	2d	4b	05	86	4f	13	f9
22	61	ca	68	a0	e8	1f	c8	c7	3b	45	09	06	e7	a7	71
b5	60	86	bd	3b	f0	27	cf	a4	35	1a	31	5a	e7	4c	27
1f	92	62	a4	09	44	91	6f	3c	09	48	64	42	46	3c	07
f4	a9	e0	ba	21	57	8d	bd	69	3a	0f	56	ab	d2	da	3d
05	ac	e4	a6	d1	60	df	a3	fe	82	44	8b	22	b3	9c	84
45	c2	74	42	46	88	2e	e4	82	5c	e3	ae	30	31	ff	89
a3	cc	01	11	e0	40	7c	c5	65	26	ba	d6	9e	7a	00	01
b9	ef	df	c6	f5	66	fe	16	10	b2	dc	3d	00	fb	f2	b3
59	67	75	15	32	f5	1d	f5	10	48	24	75	42	3d	dc	69
41	19	ce	f9	8b	34	d4	47	8b	ac	12	64	28	63	a1	ad
d5	72	dc	43	1f	f5	74	73	42	f1	27	97	4a	30	df	df
bd	a9	4a	b6	91	eb	79	cb	7c	92	44	4e	5c	6a	c2	7d
83	07	01	d7	cc	00	8e	c2	04	a5	da	b1	11	c2	4b	e1
2f	ce	47	7d	51	63	f4	74	48	1a	24	e1	5f	8e	29	42
2e	1e	70	c4	3b	40	be	eb	a4	ba	7c	8d	7f	17	d9	1f
de	e3	b7	2d	80	c0	57	73	52	f6	56	75	9a	25	82	d4
31	7d	5e	6d	d6	19	79	5e	10	34	04	e1	a3	90	5e	66
67	6f	19	91	34	6e	e2	20	9b	bd	38	1d	d7	3d	e4	5b
20	17	a1	f7	9d	1c	66	fb	1e	d5	e2	19	c2	8b	98	da
42	ed	86	c7	b2	54	2b	6d	71	1b	70	8c	1b	5f	13	e4
4f	67	a5	e6	c7	3e	49	5f	85	b2	cc	5c	d3	e4	ed	76
77	3b	43	21	77	d1	cc	68	fb	14	e9	ff	9d	2c	64	10
6b	01	85	dd	38	a5	09	f9	3e	88	20	36	e6	42	53	95
1f	67	0e	95	17	39	65	dd	6d	a1	48	8d	68	e1	b0	19
35	bf	58	ac	dd	45	48	43	61	0b	e9	8f	ae	58	81	15
8d	73	87	ed	88	c6	c5	1f	61	d3	ca	18	7e	db	ec	dd
0b	8b	1a	03	2e	91	1c	4a	69	b6	e9	93	22	11	b2	f9
ec	4f	f8	c1	8e	f5	82	69	7f	a9	cd	58	c6	30	b1	91
4c	89	bb	a0	34	8e	8b	24	94	be	a7	82	db	54	91	ed
f8	8d	0f	03	ff	19	e2	8e	8a	05	bc	a2	5c	93	87	e1
07	93	27	01	82	61	e6	5e	e6	8b	44	01	95	72	0e	43
d7	e3	28	3d	50	a6	1a	f9	d2	58	2c	ae	43	25	93	3c
f0	ec	5d	dd	30	3d	58	00	e5	0a	50	ba	58	0f	d2	37
30	68	8a	0b	fe	8e	a8	1c	dd	3e	fd	2a	97	28	98	d6
ac	35	0e	ad	d1	86	5d	9b	9a	11	89	46	58	c8	a3	e1
45	9a	45	57	f1	6b	7c	7d	11	41	75	0f	2f	95	b4	f3
a7	cd	ef	70	e0	5a	bd	c6	e0	59	5f	09	57	8e	17	df
28	64	ca	40	c8	a7	5d	8f	f3	da	ed	3f	9c	01	b6	c9
81	f3	08	d7	db	ae	f0	9b	36	f6	28	6c	49	82	6a	a1
d1	5f	a4	d0	c3	18	d6	2d	21	4d	ba	c5	52	f6	60	d4
b1	96	23	e8	11	a1	bc	0a	43	79	f6	ee	3c	ad	47	6b
b1	65	17	75	f6	5a	a3	b1	9a	ae	4c	7d	49	c1	21	ad
7f	bb	fa	75	e4	0b	43	cf	ae	3e	27	e2	c3	ac	3a	86
3b	bb	52	dc	e8	d0	35	51	40	f6	94	f4	7b	5c	db	c4
44	86	60	36	5b	b2	7c	06	45	60	9b	82	81	64	73	2c
58	00	1d	87	f3	1f	70	a8	43	9f	b7	9a	fe	64	eb	9b
a5	18	27	ab	2f	9f	3a	02	91	87	0d	8c	cf	3b	e4	c6
96	32	85	fc	bc	fc	18	43	d2	cf	a6	6d	5e	03	17	7a
78	ce	8a	9d	c9	c6	3f	a3	91	a3	70	df	2a	99	1d	b1
99	99	33	9b	5f	3b	06	3f	63	e2	be	df	6e	54	19	0e
49	74	3e	31	2b	75	12	2b	fc	9e	00	7a	de	bc	53	62
72	14	8b	7c	99	7f	6c	a9	e6	2d	d8	66	7b	bf	b4	90
77	d5	b7	73	47	87	6f	4b	eb	cb	c1	df	61	ec	af	f8
f9	97	c1	68	6e	58	25	ac	69	83	f0	43	8b	1d	8f	b3
4c	63	af	3b	e3	97	27	5d	06	3e	af	e7	be	60	e3	e0
13	13	7c	5a	55	6a	66	7e	2c	54	90	b7	f8	23	9a	c7
7c	97	bd	7a	12	32	4c	78	9e	4f	dd	88	05	98	e0	f5
8c	dc	38	c1	48	44	74	90	db	5e	16	91	96	93	0a	fc
27	a0	42	ed	d3	d8	36	bf	ad	19	11	25	25	39	74	4a
db	7b	5e	26	9b	01	41	4b	a6	db	ac	0d	b8	3d	42	ae
3b	c4	98	7b	94	96	7b	84	72	b1	8d	69	f0	a5	b1	10
40	59	12	c8	56	bc	61	23	58	b5	a6	c6	db	64	c8	59
da	b2	d3	26	ca	b0	2e	2d	a6	79	4b	c4	9f	70	e1	9a
62	c3	57	9b	44	77	2a	86	1a	ea	54	54	0f	d1	c6	55
21	ed	3e	ba	f8	79	7d	6b	65	b6	9c	e0	4b	93	d3	f6
d5	2b	ae	34	0b	22	d6	83	ff	36	8f	70	73	f4	58	dd
20	bb	80	99	59	3b	5d	b0	f5	a6	ae	5e	78	2a	d0	74
c6	3b	52	75	62	76	97	26	a5	cd	29	b7	2b	2d	77	34
8f	37	00	08	91	6c	a2	93	42	58	20	25	a7	51	52	09
bf	27	7d	50	f1	f8	31	49	b0	bd	d2	13	0c	d3	58	04
8a	79	a6	c4	e2	8b	44	d8	65	a2	bf	3a	52	c3	b8	43
c5	06	70	2c	dd	e6	b9	d8	1f	75	0d	58	9e	9f	f7	b1
1d	e0	9b	f4	20	0b	57	98	d1	8d	05	c8	9f	9a	c2	94
b4	dd	c1	cf	18	32	e9	65	fd	7a	11	a7	ef	97	1f	0b
da	45	3b	e1	8a	b1	4b	6e	7c	f5	cb	2c	0d	c3	0f	a2
aa	10	e2	c4	a2	6a	95	77	27	5c	6d	9b	c2	44	6a	97
1d	76	4d	4a	e4	f0	78	3d	3d	c5	73	07	c7	7e	1a	80
11	12	1b	5d	37	23	d0	82	84	6e	bc	23	fc	ff	48	91
74	d5	7a	2a	c6	f3	54	25	6c	5d	d0	71	cd	51	10	49
c1	07	fb	54	3f	f5	c9	35	ad	d4	5e	6a	4e	d3	7f	27
b9	a8	d3	01	b7	3f	76	84	42	fe	d1	db	7f	24	3d	77
fb	ec	a9	95	65	c2	f3	a4	de	96	22	60	6d	aa	d6	3f
90	46	0c	48	e8	ed	a3	f5	27	27	c9	65	26	c9	48	c7
64	41	e7	55	c5	71	30	f9	5b	a1	83	bd	16	47	19	fb
de	01	d9	d3	ad	e2	38	c4	c3	66	c4	73	9e	8f	29	bc
23	69	ea	72	00	92	24	c3	17	68	6e	1a	b8	94	2a	19
10	7e	f0	45	bb	78	bb	8e	b4	f1	13	9c	67	ca	67	99
d2	71	5c	00	13	c8	81	44	a1	93	76	d9	d6	70	41	4d
ca	dd	8a	84	96	8c	05	81	6a	b8	8d	ae	4a	cd	59	31
d6	84	d4	c1	44	b3	cd	77	f9	b2	c6	63	b8	0f	ef	36
f3	06	17	52	86	35	f0	14	e9	30	2f	49	d0	a4	1e	d3
78	48	99	35	82	1e	be	c7	77	87	8e	a4	38	ce	50	ee
a4	1f	26	95	8a	eb	e7	b9	87	cb	bd	7b	ac	ae	af	4b
89	32	37	ef	07	3a	05	f1	91	54	2c	d2	7d	1b	8e	91
be	b7	3b	dd	95	53	38	5c	c4	d0	91	7d	62	b8	4a	aa
29	cd	b5	28	05	a4	88	99	be	21	94	bd	96	9c	f2	a0
34	14	44	30	67	54	27	6f	21	3a	ff	18	8f	8b	4d	13
ce	ee	95	12	cd	05	44	25	13	61	9a	3b	71	8a	0b	2f
de	29	df	b2	a4	1e	ad	d5	8c	13	7c	d8	12	12	dd	d9
85	76	0f	82	35	3b	64	90	f8	7f	f3	77	5b	f1	eb	ed
b2	e8	ee	22	e3	56	91	4b	5a	7f	b6	26	15	03	e9	67
ff	b3	4b	91	ec	27	2d	66	df	ac	b2	0e	cf	9a	57	b9
8a	00	5c	52	8a	b5	0d	ad	7d	ec	12	3a	a3	28	54	a7
28	5c	76	2c	b4	d3	27	1a	28	12	b1	56	6b	c0	bc	91
d3	f5	09	cd	46	a8	32	79	f9	75	8d	9f	53	69	fd	fe
a4	b6	6c	ac	c2	28	c5	f4	91	a1	74	a9	9e	85	63	6a
4a	fa	ca	e5	b3	06	41	9c	05	97	84	d0	d8	1b	fe	9e
53	0c	b4	2c	ca	89	ab	e0	91	7e	d2	9a	87	73	d1	0f
38	58	4a	8c	6d	ba	61	90	ae	48	77	74	f7	98	11	e7
5b	2f	91	cb	b7	32	09	a9	dc	74	a8	66	9d	ce	6e	27
d0	00	df	00	18	53	49	41	e4	5e	51	c2	31	7c	33	a6
94	de	51	63	1e	be	9b	cd	2e	e5	c8	91	43	b8	60	13
c0	b4	da	a3	97	7d	8e	3d	52	6f	ee	79	71	3b	63	b6
c2	4c	1a	b9	d1	94	cf	e9	7f	d6	4f	d4	e2	38	aa	dd
d9	b4	84	95	71	d1	e7	da	6e	0a	66	68	20	1d	80	24
be	93	92	40	64	c7	ea	bd	8f	97	35	f3	b5	ac	1d	8a
90	08	39	58	74	fe	20	d8	99	32	d0	48	89	60	1d	8c
96	87	c8	20	cf	d8	7d	53	68	a8	cd	bc	75	8b	60	06
43	13	fc	77	04	9f	3a	44	48	02	8a	67	5b	e9	a9	d2
b3	9e	9d	7b	cd	bb	ec	94	9b	d2	78	d9	56	8a	8c	80
bd	75	b4	0e	60	92	11	a3	06	b3	84	45	6a	33	06	20
2a	68	be	93	28	2c	64	3a	f5	17	e7	68	49	8b	ae	17
85	8f	e6	cf	0a	16	5d	76	b6	28	3f	41	a7	49	f4	1b
d5	bf	34	12	86	d9	67	b5	d8	60	92	21	0c	c3	17	fe
61	b4	ee	2b	bf	0d	28	7e	97	f5	c4	88	46	de	c4	7b
ba	a7	15	a7	bb	0f	21	5b	03	fe	10	be	0e	85	21	42
fe	56	96	2d	30	ae	e4	3a	e2	8d	75	ba	1d	ac	fa	3f
74	7b	0a	ce	fc	61	03	f9	fa	21	08	0b	2d	a5	07	17
bb	5c	d5	49	51	59	71	dd	69	db	f4	16	c1	73	a0	44
f8	8e	9b	76	90	97	81	f8	cb	65	87	e8	21	a7	69	f3
6a	03	fb	b4	0c	67	9f	93	43	20	ee	70	a8	89	83	a7
bb	6d	ef	7b	8c	4c	8e	9f	4d	63	44	f3	76	f9	37	c9
b8	eb	5d	f2	4f	d6	ff	76	85	26	f9	34	bd	fd	e1	01
6b	37	f0	4e	b9	17	86	72	a3	d5	61	52	e6	56	8d	e1
8f	e5	f4	7a	df	0f	71	c0	63	d4	bd	28	3c	07	a9	fd
f4	04	5b	ef	db	49	d3	32	7d	8f	41	12	ef	37	6e	f6
7a	57	31	cf	ec	a9	82	2c	ba	08	fa	8f	24	a6	ce	d3
2c	22	d6	44	e0	ee	e5	fd	05	89	98	62	a2	72	0d	30
03	88	86	43	52	cb	14	65	2b	72	7c	32	4c	e1	36	e5
c6	2f	78	e2	75	94	01	41	64	e4	70	d5	0e	18	1c	ea
32	e6	b1	83	84	4a	7f	e4	21	2a	90	17	85	a5	f7	fa
c6	eb	65	21	10	e4	d8	66	27	2e	89	01	18	ad	a1	21
7e	23	24	9c	75	40	84	33	97	43	9e	67	91	69	47	79
e6	ce	de	05	bd	c4	e3	7d	fc	76	4a	d8	12	19	b2	62
37	05	b6	3e	40	dc	50	ff	df	ff	96	b3	9e	83	81	54
72	8c	9f	67	ff	0e	c5	d3	77	a1	ad	e4	64	58	f7	8f
7e	39	64	c9	22	75	6f	bb	0a	40	96	3e	e0	a0	fd	ba
f2	f0	47	44	87	74	18	11	7e	f6	88	1c	96	a5	0c	00
72	51	d1	f2	6b	67	36	a7	11	b0	89	fd	b8	24	89	00
64	bc	f8	ce	82	0d	4e	8c	0c	6c	5c	6f	9b	05	c4	4b
88	9d	fb	26	10	1e	88	bd	05	c7	b8	3c	1a	80	df	fc
ff	1c	cf	8a	dc	c8	b1	6f	84	39	24	53	60	9d	63	12
ee	f7	56	e0	fb	9c	61	a3	41	86	ea	cd	44	ed	31	06
1d	a1	62	2e	92	e6	4b	9a	7d	4e	90	89	1b	b8	5b	b1
ed	a0	5b	51	ec	95	c7	4b	73	01	ab	1e	90	5a	d1	e5
05	33	2a	fe	5f	52	19	33	15	b2	01	2a	78	1f	63	19
8a	f4	aa	fb	a5	3e	f5	cb	9d	0b	7b	8b	3e	7b	08	6a
92	88	2f	e0	46	10	c1	53	e0	f1	1a	dd	72	65	ea	8e
a2	48	d0	12	02	96	4d	05	4d	9e	95	37	bd	2b	a8	d6
56	09	a3	8c	80	96	e0	4e	a3	6d	a3	fc	d1	36	29	e3
ed	dd	5d	c9	4d	a6	21	54	08	7e	f6	fc	8e	47	57	09
fd	c6	2c	eb	39	59	2a	67	5b	67	7b	f6	db	3d	57	b4
bc	8e	e5	7a	01	7a	46	e9	74	76	31	f0	a7	27	75	a7
52	c3	7a	b7	55	4b	ba	cc	82	fa	e3	29	6b	59	0f	1d
02	3c	9f	13	4a	ab	97	9f	fa	aa	4d	6e	05	b6	ea	14
49	d0	10	7d	9e	d3	26	2b	92	77	4b	b7	65	e4	d5	4c
9b	9f	23	d3	b6	c4	1f	d0	9a	b8	81	ad	6d	e4	79	a2
5d	3a	f2	dc	64	9b	2b	22	01	bd	9c	71	a4	1b	7e	a8
b6	3f	13	44	01	ee	c5	9a	c0	6e	2a	c8	4c	92	a6	d9
5a	f3	28	1b	a8	e5	d9	f4	81	1a	95	ab	2f	81	ac	c5
35	eb	6a	5d	0e	67	37	90	99	1c	9e	54	1d	1f	47	6c
6a	b0	a7	b8	ae	22	49	4f	20	4c	78	00	f6	bc	22	05
e3	a7	7c	3d	17	bc	75	a8	29	2b	61	7d	64	49	cf	db
a4	3b	a7	7b	dd	56	05	a2	e0	57	2c	12	12	95	e2	78
5c	a3	2e	dc	9e	fc	12	43	a6	8c	a3	fa	31	ae	44	19
36	e4	5a	ed	e1	4b	14	f3	f4	4f	6c	b7	f3	df	b7	5f
b3	69	7a	11	5b	54	f0	48	c1	23	db	a8	8d	c4	02	e9
9b	f3	10	a8	33	ad	ed	2c	1b	d3	d1	9f	a2	0c	f8	80
6d	f3	f5	b1	52	dd	e7	73	17	7f	0c	1f	3f	a6	45	bb
a8	7a	40	87	50	a7	90	b0	55	0b	5b	f0	fc	43	25	02
33	98	57	a2	df	46	0c	9e	c6	15	80	9d	9f	90	5c	fe
a2	f4	e8	20	a3	c2	4b	21	ed	1d	60	54	b9	f6	2f	0a
56	7b	47	88	04	e8	af	7b	9e	46	d6	ed	0c	3c	ee	d8
54	b8	f0	50	57	bc	93	2d	74	33	1c	2a	30	5d	af	1a
d5	b0	0c	5e	21	49	f2	5e	6f	52	bc	fa	69	98	1e	c0
32	69	24	56	83	b5	ec	f6	84	fa	7b	ee	fb	30	d3	3f
7b	a0	23	42	af	9a	b7	ff	e3	3d	be	d3	cd	11	26	0a
f2	a8	84	0d	bd	8f	c8	53	a9	6b	0f	39	9e	28	8b	92
e0	23	3f	7e	cd	93	a2	bb	0f	8c	61	03	6a	9d	55	3f
f2	db	e1	ab	6b	cb	da	a0	7e	c4	a9	ad	60	02	a4	90
f1	0d	64	d8	68	25	eb	a6	3c	c8	4c	c4	dc	5d	ce	ac
06	94	74	3f	f2	b3	c5	df	8e	b2	c7	4c	a2	53	05	aa
e2	98	f2	bd	fa	7c	f2	d2	12	bb	ad	c0	59	dc	91	80
e0	bf	2d	50	d4	49	68	dd	ff	2e	2f	3e	2a	1f	7f	a2
fd	51	74	00	dd	c1	e7	ee	3d	51	40	49	72	10	bd	50
9a	61	4a	42	d3	65	0c	34	4d	34	14	d7	65	d3	3c	22
65	b8	22	f6	2f	88	e0	02	56	1d	a2	b5	f3	f8	7b	27
ef	52	28	c5	48	85	3c	ea	03	3f	58	a5	c4	84	6c	6d
36	2f	88	2f	6d	07	bb	c6	af	ca	fe	66	55	40	7e	61
d8	b9	8b	c1	34	c6	bf	a8	56	79	9e	23	74	f8	3f	bf
fe	2b	c7	df	4c	81	ba	1f	7a	5a	6d	17	1b	5b	8b	bc
7d	21	4d	28	8d	0a	9b	5c	ef	9b	c7	75	14	30	56	36
a8	10	9b	42	00	f7	7a	25	e3	48	6c	dd	82	95	2d	4f
c3	78	43	da	fd	b6	90	eb	88	9b	c3	29	48	8c	60	29
79	d7	ed	e2	3b	51	ab	31	bb	e8	7c	8f	f1	6b	65	b7
28	9e	53	bb	88	e8	4c	ce	71	e3	55	dd	fe	6e	90	14
ac	0e	6b	c0	3a	56	e2	67	37	ae	c5	50	18	53	a8	e2
db	c6	c0	26	e6	ae	db	01	b3	8e	21	d7	7c	21	ca	69
16	ea	37	b4	18	cb	29	5a	f2	46	7b	74	d3	8d	ab	4f
21	92	8f	ee	b5	f8	2e	b6	c2	a0	35	4d	7d	b9	83	66
27	70	30	91	b0	29	33	ab	c0	dc	3b	ff	69	4a	5d	41
5d	73	86	00	48	1d	48	54	b9	52	e0	a5	d0	0c	39	f3
a7	01	ee	55	84	0f	b7	d9	1c	da	e6	34	44	7a	47	1b
3d	ae	12	51	e0	c2	30	3f	4d	d5	d4	bc	59	31	ca	a8
6e	42	42	74	56	82	90	4c	9a	31	64	2c	07	69	c1	a1
54	fa	f4	79	38	a0	80	c5	ba	cd	10	af	5b	0b	50	18
38	26	04	0d	e9	92	7b	bf	fa	b9	85	30	f3	b3	c9	2e
77	55	bc	43	1a	24	cd	5c	1b	c6	eb	97	4f	77	e0	1f
ea	2f	e4	c3	05	b2	50	56	7f	2f	c4	6c	e9	4b	2a	23
25	3f	1f	f3	3f	ce	56	fe	ce	91	43	fe	10	cf	3d	c3
b9	4d	38	7b	fb	fc	cc	16	80	db	9d	eb	2d	b3	c4	1f
01	86	24	3d	11	e9	d9	5a	19	da	d5	5a	c5	1c	01	9c
66	06	9b	ad	3e	16	1f	b3	0d	60	95	e5	f7	08	9b	c4
69	f9	f2	9a	e6	43	c9	9a	04	a5	7d	35	96	a1	f9	a1
fd	12	69	dd	64	4a	3a	a7	29	88	d8	cf	27	e7	09	da
72	72	20	ff	92	1e	d7	e8	a8	2b	79	8c	8b	61	6a	26
fd	90	18	a7	2f	c1	54	b9	a5	4a	36	1e	69	e1	e0	4c
24	18	f2	4a	c7	0d	d9	04	b9	6d	fc	8a	ce	08	f9	ca
92	70	77	41	92	1b	a4	93	a3	ef	22	91	e5	fa	54	97
e6	0e	7c	7a	02	c1	35	d6	79	10	f1	40	7a	fb	e5	d4
97	69	87	63	0f	89	e6	ad	61	23	2c	7c	42	88	dc	19
f1	16	f1	75	6d	92	31	44	e0	f2	0c	fc	56	75	29	33
12	8b	f4	46	cc	14	8d	68	40	44	0a	0b	a3	21	ae	e0
81	b6	ef	15	0c	57	f6	00	d5	88	42	af	f7	d3	c8	66
6b	00	8d	82	e5	29	48	d8	1c	1a	f6	f5	eb	c7	89	88
7f	24	ae	65	ed	f5	da	d9	7b	6f	47	f0	c6	29	e2	02
74	f8	f5	a5	6f	8a	1d	4d	4d	60	18	50	64	35	80	86
ee	8c	0e	bc	18	e3	a6	83	1d	1f	ac	9c	40	3f	8e	29
c5	b8	37	de	47	20	5f	4e	f2	04	b0	dd	7c	03	d7	a2
ba	5d	cb	22	a5	78	96	c3	93	9c	a1	10	67	33	cf	7f
84	2a	6c	77	6d	68	00	f8	3f	19	6d	23	16	73	33	84
e9	b7	1a	ce	8c	c4	dc	f3	d5	57	07	3a	5f	79	19	81
48	93	47	be	fa	e2	19	c0	cc	f2	42	a6	26	56	96	53
48	22	7d	6a	5a	dd	b4	61	be	ae	67	e9	fa	60	03	bb
85	4e	5a	38	ab	b8	9c	28	dd	3c	a4	81	cc	9b	31	5f
41	6a	de	97	e2	75	e0	a3	8e	4b	d8	04	6c	7c	5f	9d
d7	8d	54	e5	74	d7	48	79	eb	90	94	96	09	9e	3b	09
6b	2f	e9	88	f6	60	e7	a2	13	77	42	e0	69	6b	a7	df
31	96	45	3c	51	64	41	25	29	04	98	0a	d2	0b	d9	48
42	a5	2a	63	2f	2e	19	10	67	6a	76	6d	51	1e	21	a7
bc	1b	a7	b3	88	08	41	34	80	7c	a5	7e	68	5a	0f	ee
90	63	d1	1e	25	28	aa	16	2d	d9	72	f2	86	20	bc	ae
99	b0	a3	59	fe	bc	c6	a0	13	22	bd	ae	e7	42	68	0d
d8	79	78	14	50	54	0d	be	c8	b7	6e	52	8a	a3	f7	8b
89	c2	fd	70	e5	37	76	c8	46	d8	58	1a	9d	29	5e	95
65	3d	ee	60	83	bc	0b	ad	58	c3	14	8d	e4	aa	ae	f6
d2	fb	5b	ca	15	a5	b1	24	33	8f	c8	be	04	56	b3	9a
b1	75	ae	21	b0	d0	27	c8	a3	ad	5e	a6	17	f9	a9	c4
0d	f1	c2	f6	51	4f	bc	a9	10	bb	b0	ae	77	d8	df	f0
1f	69	9c	03	b5	78	d2	96	ef	46	c1	e9	1f	a8	08	49
13	78	aa	49	b1	50	61	20	b8	8e	12	5d	8b	13	4f	2b
1d	07	39	bf	02	69	90	02	f5	ad	6b	ae	92	3d	4c	09
45	7d	e1	2a	e4	9a	5c	41	84	8e	c2	c4	54	ea	a0	d4
68	52	b5	b4	bf	73	c2	13	ef	c0	92	e0	f3	1c	ce	84
71	eb	e1	54	85	a6	f2	17	9c	e8	c4	62	88	ab	a2	7d
dc	3f	6b	0f	7e	69	b9	26	c6	c7	93	fe	24	91	dc	04
9f	72	15	fc	f6	48	c9	51	08	aa	c8	09	c4	21	63	28
2a	c7	94	a6	25	ea	1e	67	bb	01	a1	6f	e9	d4	b8	fb
33	61	c5	60	48	92	b4	db	8f	89	ee	71	98	e1	20	a6
46	49	e2	ab	e3	39	d5	81	11	da	0b	33	f5	2d	4c	1b
c5	5e	53	03	fb	0f	8a	6d	81	05	16	39	c4	a6	a1	78
ef	69	db	ff	3d	0b	9f	86	ce	0f	87	5b	da	2c	b3	35
eb	d6	70	5f	9a	25	e2	b2	f5	c4	62	ff	42	3d	fa	46
80	fc	a7	ba	65	4b	b8	2b	8a	c8	48	09	f7	3c	63	1b
18	6a	90	e5	df	76	bd	b6	eb	b5	a7	62	fb	c0	c2	83
18	6b	35	4a	ca	41	09	8b	6a	49	dc	c8	71	65	83	0b
dc	67	06	61	9b	80	02	3d	0b	86	c9	e6	18	7a	dd	1e
d7	1d	e1	a3	d3	5a	4f	f2	30	44	05	94	83	be	97	f6
f1	68	58	33	44	27	ed	0e	41	06	fe	a3	de	51	d7	65
dc	9a	6b	6b	cb	79	e8	11	da	f4	e5	1a	19	7b	65	e9
32	01	91	54	b4	a4	6b	3e	8f	44	55	0b	cd	11	0e	09
d0	9b	61	e8	c9	b1	09	9f	96	40	1c	e0	bc	29	72	ee
4f	9b	aa	a8	92	7e	cf	19	fd	a9	2f	89	1d	76	b8	bd
8a	1a	d0	b8	f2	e1	d3	45	00	03	6c	e6	16	d5	17	7c
83	bc	6d	03	b9	48	40	66	69	86	96	7b	4a	4d	eb	7e
ff	1e	de	bc	80	fd	03	8f	b4	83	08	78	5a	fa	f3	92
f5	68	4f	5f	fd	95	41	46	c3	52	4e	76	8c	fc	70	69
58	a0	8d	e8	c8	c2	02	a5	52	87	c8	e8	0e	2d	20	b9
14	1b	d8	b4	94	56	48	1f	a7	67	dc	8c	77	ce	47	ea
10	73	77	d6	b5	0c	2c	7b	25	30	5b	da	3f	b8	8f	19
bc	bf	e9	44	31	e4	c1	a8	c5	51	3e	48	a9	7a	d8	19
e1	84	ec	79	cc	0f	91	75	63	dd	0b	1e	96	0a	f9	04
16	e3	79	12	da	bb	78	5d	14	b4	8f	09	50	99	0e	1e
f7	fe	b1	9e	fb	36	ae	95	2c	1c	79	b5	a8	6a	68	62
43	f3	27	44	e0	2f	1b	57	a3	25	98	42	e8	32	18	15
e0	50	b3	9a	bc	21	00	22	69	91	9b	7f	28	c7	68	69
5b	94	a8	9f	5f	bb	4c	90	36	2b	19	85	d2	5e	23	cc
44	d9	10	ed	98	85	75	f1	03	e1	de	7d	a1	90	db	c3
22	74	29	9c	73	db	fc	8a	07	e5	37	ec	42	1c	21	c1
c0	e3	0a	39	50	d8	99	53	90	bc	27	ca	b2	9e	99	9d
43	3f	2c	af	4b	a4	c9	aa	88	d0	ce	17	fd	67	80	b0
2e	79	34	38	8a	c5	5f	c0	79	19	a9	8c	aa	cc	5a	c4
2e	7b	73	44	55	b9	58	78	b7	2c	87	ef	80	91	f6	fe
10	dd	99	fa	21	b3	a4	80	1e	22	4f	2d	35	46	1b	ec
0e	c4	fd	92	31	94	fa	e5	72	d5	7d	1e	20	bb	12	44
ee	2d	f6	73	ad	dc	f9	96	38	42	50	15	6f	f2	6d	16
cf	44	ee	ba	47	ee	43	ee	b2	c5	87	96	d7	a0	0a	0f
9b	58	30	65	a7	fe	26	94	c8	e3	a2	a0	1f	f8	57	fa
80	f4	ef	4b	a6	4c	c5	cf	f2	30	5a	be	b4	03	51	a4
76	d4	97	14	fb	06	23	89	5d	90	fc	0c	89	25	ba	57
16	81	bf	c3	40	e2	28	a2	4d	3d	c8	83	9b	62	61	e4
c4	99	90	67	cc	c6	f9	f8	5e	38	79	50	67	74	40	b4
d3	4c	af	1b	78	58	75	f9	63	37	17	cf	f4	ac	5a	a2
d8	84	70	7c	a3	13	b1	29	fb	12	43	fc	2b	64	c5	2c
fb	9c	9e	c5	ae	f6	5d	de	52	07	6d	e5	53	72	22	3b
2b	12	76	a1	6a	c5	41	0f	ea	d5	d9	7b	d7	38	14	46
04	99	ff	1d	fa	d9	ae	18	9c	77	81	90	f5	6a	2a	49
26	fd	f2	46	7a	37	92	17	f0	96	2b	68	a3	07	7a	82
da	39	2a	eb	45	2d	75	dd	8c	68	4d	81	25	5e	96	db
c5	21	d0	3c	01	0d	fe	bb	b2	e0	65	82	09	fd	66	f8
f4	22	44	18	ae	d9	28	dc	96	a1	34	3b	3e	97	9f	01
c5	d3	3c	ec	90	b7	e2	22	40	a0	61	3f	f9	94	b0	ef
e0	10	29	b5	a5	c3	1e	40	7a	c0	8f	7b	df	9f	8d	89
4e	65	2a	68	4c	a6	5b	d8	e0	5f	4d	5a	3c	1c	83	f4
ee	c7	df	21	b5	0c	a2	08	f0	1a	cf	68	cb	c7	2a	fa
18	ed	58	d4	f0	6e	8b	e3	09	ab	7f	08	78	aa	4a	33
b1	05	62	b3	6b	13	b5	0b	7f	a5	d3	ff	7f	bc	65	8e
0c	87	e2	ce	3e	a7	65	8c	c0	b3	4b	1d	ff	e7	88	36
c5	a5	7b	e4	48	d4	31	22	7a	1d	c9	a6	41	76	0a	94
1d	58	1b	a8	d2	c5	6f	43	08	50	e4	e0	52	a9	3b	c0
70	40	d4	cd	0a	16	8f	89	da	d4	e4	2c	7d	e8	17	a3
48	2f	35	27	19	bf	aa	e7	e4	58	65	a5	28	61	4d	38
39	12	c7	91	d8	1f	55	d8	3e	14	05	d2	16	4d	ec	f1
7c	0d	a7	5b	cc	4e	e3	c0	b2	b0	07	19	2c	88	5e	a4
8d	bd	59	9c	84	86	e6	2f	a6	b4	98	3d	d6	2f	75	20
07	4c	00	ce	e2	1c	51	39	9d	5b	33	13	fe	cc	4d	10
03	34	21	7e	88	f5	c2	57	05	03	82	01	7f	ce	c1	53
35	a3	8f	8d	87	d8	a1	17	cf	be	a8	81	b7	f7	8b	93
25	ea	8d	cd	d1	6a	d4	83	7e	1d	d8	9c	1b	9e	dc	12
e6	d2	50	13	d9	de	7c	4c	da	f5	71	6a	cd	d2	69	83
ae	20	d5	12	85	27	7e	3a	95	f3	fa	e2	c1	88	3a	f6
9e	c3	65	14	6d	6c	0d	9c	2d	99	15	6e	f4	d5	69	d2
86	f0	7e	e2	51	71	e9	2f	63	ec	a1	a0	97	32	63	4b
2d	82	53	88	ae	dd	35	51	38	a3	9c	3a	bf	7f	9a	e0
4e	a4	78	32	97	e7	22	88	bb	80	5e	61	17	bd	d3	ca
43	7f	df	83	54	82	8a	42	9e	5f	48	ca	45	40	00	fd
87	6a	e2	a1	d0	10	f6	31	eb	bc	09	f4	47	d9	78	be
c2	8b	69	29	79	7d	88	21	08	c8	02	b9	a5	13	40	0c
b0	a9	af	ef	db	5b	12	ce	fb	57	a6	fe	cd	5d	09	1c
f8	8a	e2	d1	cb	4e	0c	29	b3	91	d6	77	d4	ef	dc	31
bc	3a	0b	e7	43	c8	76	da	5c	21	8d	0d	09	d6	3d	71
2f	b8	47	2d	84	a8	4c	89	fc	73	2f	0d	e9	5f	d1	6b
c6	fa	e3	64	49	d4	ca	69	1f	7d	8a	02	7b	a2	cf	15
da	c6	18	64	64	6a	83	ba	08	e8	c8	d8	ba	ca	c6	c6
76	ba	19	3f	1a	46	56	48	d7	57	9e	23	37	7a	1c	20
0d	1f	ec	db	fa	07	32	a9	f0	01	de	a4	b0	8e	83	25
40	0f	52	4c	4b	ee	13	b1	05	00	94	d4	4a	63	fc	4e
da	79	17	3d	71	52	c0	a9	c8	c7	9b	40	d8	1e	ac	db
32	97	32	65	67	05	f0	e6	e7	8d	38	99	e3	56	f5	3b
25	9c	88	28	80	6c	40	4a	5b	35	65	ff	4b	5e	19	10
16	86	f6	d1	45	5e	ab	bb	45	b0	95	b5	8a	be	96	1e
3d	27	34	5d	b3	4d	4f	b7	c2	57	f5	ed	2c	c2	45	8f
51	c5	24	44	aa	5a	63	c3	ec	4d	9f	56	45	15	08	d3
ee	23	56	81	53	57	b6	99	d3	7b	3f	74	2b	61	51	40
29	94	6c	cd	e9	55	ef	d4	cd	f5	12	1b	e0	53	b2	9b
64	30	f0	a9	91	a2	7b	68	60	2e	c8	d8	6e	75	be	a1
ce	2d	87	8f	f9	ee	f7	af	18	c3	ed	6e	b2	85	bf	55
4d	bb	a5	c3	73	9e	77	a5	58	54	9b	e2	6d	0a	28	60
10	0a	40	3f	5c	6a	41	9f	d6	59	c4	95	6e	b0	34	1f
e8	f7	61	07	bf	5f	28	42	ee	9e	e9	9f	71	fa	cf	98
4a	23	57	73	cd	eb	06	50	63	9d	82	0e	7e	84	8a	54
49	6b	13	d7	68	14	7e	79	97	5b	3a	95	6b	b6	af	ed
48	60	73	40	41	2b	a2	1e	38	95	ab	12	a9	1c	5b	67
80	ac	8c	c4	47	ab	8e	52	ff	be	84	cf	4f	7d	4e	10
39	d1	5d	f5	23	85	16	fe	e1	c5	2c	35	82	fd	fa	ed
0b	19	86	60	5e	32	35	ab	43	f7	07	f3	06	57	0f	51
5b	52	d3	79	04	ff	33	74	3a	73	d8	ab	59	ff	74	91
5f	54	fd	ac	c7	1f	fd	d4	9b	93	17	e3	ef	98	40	db
51	8a	cd	a1	a7	b2	c8	f5	87	3a	be	ba	ef	94	a3	d6
38	ae	82	53	60	51	f4	c5	dc	6b	65	af	51	bf	6d	ce
43	fe	0a	88	64	a9	ad	2f	bf	a5	af	f0	ca	1a	2c	d4
72	51	1f	46	05	f7	45	c1	0e	50	2b	71	70	b4	2b	a6
66	76	85	cf	2c	d5	f7	41	68	d3	e2	86	0a	bf	d4	5d
5e	6c	a1	6d	95	17	b8	97	4f	89	a6	07	72	a6	54	d4
bd	46	24	08	57	cf	d6	0d	7b	aa	74	dd	17	d3	f3	4f
96	42	a2	5c	ce	85	06	88	ca	f6	ab	50	32	98	36	81
33	ef	ca	47	7b	90	a1	84	b4	63	5a	c1	76	2b	c1	c9
be	ef	52	71	70	61	39	a9	e7	18	fd	f6	68	46	9d	98
b5	e6	7a	72	82	87	4c	25	39	1c	0a	1d	6a	6c	52	14
36	71	b5	24	b0	dc	6a	68	f9	52	07	fa	89	b0	83	6a
f0	bc	1c	70	39	70	9e	f3	c2	30	b5	7e	52	78	48	9a
24	d5	8f	a3	dc	20	0e	ee	9d	aa	4e	22	79	e0	9a	26
b1	a7	1c	fb	00	62	15	b1	f1	5e	a0	7e	02	68	d9	5a
12	19	13	5f	0e	61	22	5c	28	36	74	da	c8	78	62	50
21	f5	63	a8	c4	4b	7f	81	5d	14	25	04	4e	82	9c	1d
34	a6	5c	86	a7	ed	24	d7	6f	b9	3c	15	c0	ce	d0	bf
3a	3b	2d	36	e0	c5	71	ad	e4	95	75	b6	98	28	d5	f8
4f	45	f1	1e	25	97	ac	e5	c3	08	7d	81	4a	64	d8	80
9c	6f	78	6c	bb	b6	55	74	ac	3c	23	e8	cc	f9	84	6b
78	a5	19	12	4f	93	10	8d	fe	49	ad	25	0c	a6	44	18
77	5c	68	74	f3	03	25	bf	6f	6b	8b	24	59	47	d1	f3
84	b4	e8	97	5a	30	e9	cc	59	02	f7	b9	d3	33	a6	2b
87	dd	d4	ed	b1	ce	ef	96	3f	8c	7a	df	e8	1c	62	44
8c	eb	df	ff	8e	6d	0c	46	d6	aa	33	be	09	2a	dd	3d
15	f0	a6	28	b5	44	49	33	07	9f	94	a7	52	3a	48	97
82	f7	62	cf	11	d2	c6	92	38	bb	28	f3	e7	94	3f	bb
52	35	43	30	ad	47	e2	6b	d8	34	ed	4a	b8	9e	d8	70
de	02	56	bb	37	b5	1d	96	68	6b	44	2c	93	1c	d0	ed
53	74	a0	9f	ac	1a	bc	e3	de	6a	e6	bd	f8	dc	c6	a8
b7	02	8b	df	85	8b	e7	d9	e4	49	11	aa	0b	a7	c6	e4
75	7b	db	d3	48	6c	b2	6c	ac	35	49	74	6f	54	9a	e8
fc	a9	97	d3	3c	6c	6c	10	a6	71	77	7a	50	06	45	26
fc	49	1d	95	1d	76	b8	dc	c6	77	36	eb	dc	81	fe	63
5b	3f	39	8e	a1	a3	87	fe	60	02	09	26	4a	c5	db	44
82	bd	83	f8	a6	b1	07	91	dc	1e	28	5a	f3	9f	1a	4e
c1	c4	82	8b	fe	54	e9	72	38	de	9b	88	f8	61	2c	69
3f	f8	31	80	0c	11	70	f1	59	7a	90	3b	7e	de	e3	1f
ac	07	eb	6c	eb	b0	e8	f5	bb	d4	21	97	0a	69	b4	06
8e	f7	0f	46	54	fe	29	60	0e	a2	ce	f6	77	dd	0b	d2
63	b7	4c	88	1b	d3	9e	34	84	7d	2f	af	66	e8	13	6a
7c	f8	1b	b1	7f	d0	df	45	3f	00	02	b1	4c	8b	f8	da
9d	a1	74	97	51	f0	2b	65	c8	d4	65	56	d2	2e	64	d6
b2	c8	bc	8a	c8	53	ed	13	95	58	1d	59	dd	c4	50	1b
04	d7	9d	43	cd	fa	ef	ba	7a	9b	33	2a	66	b0	d3	a0
27	77	50	b6	b5	47	d3	86	fb	4b	25	da	07	8f	f7	54
e1	e2	d0	9f	7a	ec	13	9b	20	bc	f0	9c	31	2f	60	6e
df	7e	ac	45	40	e5	c0	cb	e0	02	14	f6	29	77	a2	b1
7b	a9	f0	5d	8a	54	3d	55	ee	ac	7a	e5	27	a8	1d	20
f6	75	81	fc	c6	79	8b	2e	53	a4	77	87	5e	e3	8f	90
30	e8	e6	78	06	6b	da	94	fc	27	51	2b	6d	fc	a6	28
52	2a	b6	a0	15	57	86	9b	66	5c	61	8d	9c	da	b1	e0
c4	5a	96	c7	83	cb	8e	e8	a9	7c	3f	0f	06	ea	00	0d
c2	39	f6	ef	6d	2b	c7	79	a6	38	85	31	52	4a	c2	76
80	80	82	2f	c5	b8	56	20	87	41	f8	a8	62	c7	a6	7c
9b	6c	05	38	3f	c5	e3	05	5f	e2	47	88	df	eb	76	fe
76	bf	d6	18	1e	39	48	60	8c	c6	b4	64	15	b8	84	d6
86	27	bb	be	40	0a	e8	41	d9	0e	28	14	0a	8d	46	5e
ab	5e	87	52	c0	1d	35	df	d7	25	a0	b3	75	32	43	69
06	b7	59	68	d9	f8	0b	cc	2a	a9	c2	0e	72	9e	c4	ed
5e	b7	6d	3b	a9	e5	e3	25	1a	f5	a9	eb	d4	82	f6	34
c8	52	d3	61	06	60	48	98	06	e1	e0	73	ff	cc	15	e2
0f	e5	30	33	64	de	d9	b0	dc	b3	a8	b5	af	c7	c4	4d
52	8c	3d	ce	48	5b	35	19	4c	a9	b5	ec	69	9f	e7	51
a9	12	12	77	33	c3	9f	42	2e	9c	c1	78	c8	da	c4	4b
00	0e	df	67	86	7f	cf	71	e8	8b	b1	49	1f	68	8a	49
8f	bb	33	c2	81	ae	d3	65	c1	7b	6b	1f	91	c7	00	ab
72	7b	96	f5	a1	2d	a7	2f	db	11	c6	c3	58	93	53	03
fb	b0	5f	70	09	e6	de	17	c4	aa	9c	8c	f0	12	c6	ed
61	71	db	09	73	16	21	28	1a	6d	15	f5	aa	35	0b	f6
77	fa	69	67	a5	f2	0c	03	3c	d5	71	8f	08	b8	32	c6
2f	b8	04	eb	b9	3e	91	1f	13	36	b8	8e	2f	02	84	dd
56	7e	33	4c	cc	d1	60	ce	fd	c7	ea	3a	2e	bb	af	3b
67	3d	0a	88	4d	a3	66	29	64	2d	90	51	82	28	0a	f7
ce	2d	2e	fc	b9	e6	51	3f	28	a4	a6	5e	03	5b	49	25
b3	3f	6f	9c	65	78	e5	55	0b	61	26	95	67	c0	4d	7b
93	12	7f	22	68	7b	8e	5b	2b	89	bc	00	f0	9b	9c	e1
03	0b	b4	ca	22	a6	a1	36	72	e2	da	8a	d2	36	14	30
26	d5	86	77	7e	7a	a5	78	95	fe	12	db	f3	8d	f2	4e
a0	af	36	b5	eb	c0	41	54	b6	01	d9	d2	44	bb	cd	6d
32	66	e3	f7	9f	37	24	12	3d	df	d3	e6	81	64	e0	b9
99	a9	5e	ca	07	de	8b	f4	1a	94	17	a6	88	b9	1b	b4
6c	18	53	30	f7	eb	ca	e5	88	2d	4b	f6	6f	74	1c	5b
4b	13	89	d3	c0	14	0f	12	d3	18	07	97	1e	19	b1	12
13	61	fe	3f	cd	3a	c9	53	57	ef	da	c0	c2	4f	a7	30
be	e5	ec	63	52	dd	f1	41	eb	6a	28	66	e7	ae	a9	e3
85	4e	ef	3d	2b	51	b9	5b	f5	74	8e	c9	9d	7b	d7	55
8e	9e	fb	21	03	6b	6f	5b	19	3d	0c	50	24	c6	bc	3e
0d	9e	bd	3b	70	35	00	2a	77	85	18	68	7d	65	01	ec
eb	3a	9a	29	de	e3	21	a4	7b	ad	c6	c8	dd	63	92	b0
49	64	4c	53	d3	89	4e	28	d4	70	5b	97	5a	6e	bf	48
2d	1c	83	6e	09	41	50	9b	7c	76	e1	8d	a9	22	5a	27
87	ea	2d	be	9a	be	77	00	f7	c9	ea	17	77	aa	4f	8a
67	17	f1	b0	63	1f	f1	99	20	8a	a8	8c	d0	1c	c6	97
1a	92	92	01	ff	6b	06	04	d8	90	90	d4	c3	4c	aa	2e
6a	db	18	f6	9c	38	f8	a5	ba	35	b2	25	1e	9f	6b	da
44	9a	e0	b5	8d	ac	08	ad	7c	38	3d	43	6d	86	c2	a2
64	2b	3f	e5	86	0f	df	0a	b0	78	6f	46	1a	1d	e9	70
90	23	81	ef	c6	7f	45	f0	19	6b	dc	5d	b2	98	d0	c5
93	3c	32	85	26	5b	9e	eb	77	63	0d	0e	43	70	5f	53
c5	13	b4	6a	c4	12	67	3c	24	05	7f	e1	ea	1e	d8	13
50	5a	4a	b9	cb	a8	8a	d0	01	c5	e1	b9	a1	c3	77	5e
b4	1d	f0	f9	25	5e	15	af	a2	7c	a5	4f	c9	e3	ae	72
d3	be	ba	0c	75	c5	eb	2d	29	57	ec	ed	7e	d7	6a	33
ba	9b	e9	c3	a2	1f	f4	e6	97	e7	f0	79	1f	18	ab	82
35	48	3f	82	15	d5	f7	38	db	89	1a	d1	04	44	22	e7
39	30	3d	77	62	25	b0	3d	f8	f8	08	74	3f	79	23	ca
a9	a5	2e	5f	0d	3b	ba	09	90	53	2a	52	20	6d	e8	ba
b6	47	37	dc	52	0c	f1	8a	82	37	fc	30	51	cb	a4	ff
e9	d5	d6	12	0b	03	dc	a0	d4	31	80	44	3b	e9	c6	ee
b6	93	29	b9	b9	4e	23	62	39	60	d6	50	48	b0	f9	7a
79	11	91	74	49	ea	cc	6d	7a	71	7a	31	74	28	b6	d3
78	21	0b	53	38	62	4f	11	8f	b8	0b	7a	f1	d0	70	2c
1c	c6	b5	4e	13	f6	46	07	37	a8	53	fb	48	3b	8d	78
a9	e4	e8	94	a4	de	69	09	df	85	4d	b5	45	5e	4c	dd
c4	97	0a	35	1a	a2	37	e5	c5	a1	b9	e3	44	8a	f6	18
17	e5	ca	79	dc	35	e9	86	9f	c7	03	8b	6b	1d	ff	ea
4e	a8	a3	b4	eb	df	cf	33	99	a9	84	e6	db	95	ab	5c
ef	a0	0e	ee	6a	f1	16	0d	80	8c	bb	f4	80	04	e8	2c
ba	3f	79	8b	c1	ce	84	df	fd	0b	79	5d	8c	76	43	35
56	dd	71	80	5d	ce	a5	01	68	e2	ec	40	a0	0d	1d	7f
5f	80	dc	b4	42	cb	36	04	ff	01	b7	99	35	cf	cb	be
a7	e3	66	a5	f3	2b	6e	70	74	12	c5	eb	1a	3b	be	27
4c	26	76	c0	7c	d9	fb	8f	70	d9	0c	66	1a	46	73	e8
c2	22	d2	e6	1b	08	43	fa	59	38	8a	fb	3f	0b	57	dc
d5	b2	c0	28	cf	a2	0b	41	b6	aa	f6	07	4c	98	ec	13
77	66	51	56	f4	1d	e3	2d	5b	a4	a7	bd	d5	20	23	2d
b1	e5	82	6e	0e	ab	6d	dc	20	b5	75	1a	7b	c2	e3	41
57	96	66	a0	68	d9	6d	92	5e	4e	ca	8d	19	8b	16	a3
05	45	23	66	e4	68	39	b6	28	43	e7	39	7d	29	63	32
02	d2	51	37	a0	cb	52	0a	59	9f	00	58	8d	6b	34	64
57	f3	a1	83	a9	a0	76	50	63	e3	8b	b9	62	5e	b7	c3
4d	1a	28	b8	f1	3e	0e	2e	95	d1	be	d6	64	b3	59	3b
4c	fe	7b	c3	ef	55	70	2d	a7	24	5a	dd	92	c3	08	ce
c8	e7	b8	0f	77	47	8f	7a	d6	6e	fa	3d	42	cc	be	8b
c8	69	c1	bb	b0	8c	e7	1a	f3	e4	a5	08	d7	2d	9b	8e
74	8c	cf	e6	e1	6e	a4	3e	8a	94	84	99	84	51	a5	a2
01	d3	f1	88	c3	ee	66	5f	26	17	8a	0a	c3	d6	87	68
7f	66	d1	2f	aa	ce	39	25	0a	29	90	26	ae	e9	f0	9f
30	a0	3d	7c	b2	e0	a6	4b	31	ff	59	db	4a	4e	71	11
f3	ed	dc	91	e2	38	05	4f	ed	bb	3f	63	be	e8	8f	f8
21	64	b7	bf	ae	cb	07	e6	a1	d2	59	e5	41	0b	7c	56
f2	ec	3b	90	19	67	e4	45	dd	a8	ed	9e	e1	d5	c6	3d
9e	3e	47	8e	35	7d	ea	4c	da	1e	df	78	7a	d2	aa	2e
74	83	7b	61	63	2a	cf	4c	91	9b	a6	df	e3	00	af	17
40	0f	76	3b	cc	03	d9	8d	71	5d	99	65	12	0a	a3	e6
f8	39	9c	9a	c3	fb	99	3b	11	e1	18	1f	68	2a	99	57
df	06	42	79	0f	b7	e4	7d	ea	05	e3	17	cf	9e	cc	a0
ac	ba	a0	18	ed	cd	8d	2a	95	54	c5	21	b1	31	5e	dd
63	0c	68	01	07	43	40	99	c1	e9	db	01	d9	9e	ab	bc
9e	7f	09	71	e8	6f	c4	b9	df	32	6e	66	74	5d	34	72
01	d1	9f	db	00	7f	63	20	39	dd	98	ae	0c	15	9c	08
11	14	c2	20	53	c2	5d	da	e7	b1	73	fa	ca	bc	01	ce
b1	1b	61	8b	34	6e	90	2c	69	78	03	e1	e6	67	de	16
ff	f2	7c	67	ec	61	a5	d4	e7	dc	64	a7	23	23	84	b8
be	9c	7b	c3	78	ba	e1	f1	d6	bb	f2	a7	93	13	f2	ec
38	ea	24	99	5c	38	81	6a	b1	d6	a3	25	ef	37	de	24
80	f5	5d	7d	63	39	ff	7c	34	70	f7	43	51	36	dc	be
be	72	98	ad	7b	b7	48	a9	cc	37	2b	1f	74	ea	fe	0f
2c	87	84	fc	11	88	d3	c0	ce	32	ef	14	dc	48	70	60
28	44	f0	3c	f2	2e	fd	6d	bf	3a	f4	16	6f	64	31	ac
3f	6a	90	66	35	bd	92	0d	ab	f1	7a	e7	34	2f	28	c1
14	51	c9	41	71	e7	be	c5	4b	03	1d	af	15	fc	ac	14
9a	77	58	be	53	96	39	7a	61	dc	b6	fb	0d	93	41	57
1e	1c	0f	e7	53	8d	31	4f	86	37	e8	a5	d5	1d	43	d1
ae	80	06	88	eb	cc	23	fe	0b	99	a6	97	7e	c0	28	81
59	bb	f1	a8	6e	5f	e2	4c	3f	08	9d	ab	8d	3e	eb	90
24	03	d3	23	d3	a2	4f	5f	11	b3	a0	fa	9b	66	52	9b
45	0f	55	11	2a	25	50	2e	73	0a	e3	7e	58	c6	5d	1b
31	9e	dc	16	7c	c5	6b	d6	a0	8a	55	ab	8f	9b	6e	8e
62	50	6b	2a	6c	ec	b6	c8	19	3e	d5	8b	f6	b2	7b	ee
5a	90	7e	6e	56	2f	4f	0a	48	93	a4	9f	48	aa	ee	8d
62	fc	09	2a	f8	8e	19	c0	e4	0f	48	19	8f	52	5a	7d
70	48	ab	2c	8d	4c	1b	39	b1	8d	6f	6c	32	9d	2b	ea
c1	c1	36	65	75	42	a3	7f	e4	93	46	e4	f3	2a	c2	e2
c3	b6	65	9e	8f	7d	b0	6a	49	dd	b6	f0	30	3e	00	cd
b9	67	d9	9b	d2	ef	f9	bd	cb	74	3b	2d	b9	2c	67	3f
88	65	8f	f1	2e	16	2c	ae	32	ea	ce	bf	6e	c0	b1	79
f8	a6	4a	2d	b4	84	70	a8	6c	bb	0b	6f	23	6b	0d	a8
b8	9f	1b	c2	a7	4b	53	52	a8	c1	70	81	c8	5d	e3	f0
9d	b2	4c	03	28	1b	11	f4	bb	9a	7f	fe	f3	da	57	dc
ab	a8	68	73	54	e5	e2	af	0a	dd	45	c6	6d	5b	1e	92
c0	d5	e6	1b	08	d5	71	04	ce	95	52	c7	e1	bb	da	bd
7e	a8	90	e0	8f	66	3d	d8	5f	bf	5a	82	c4	9c	6b	92
cb	7c	35	d7	65	ac	35	35	87	0f	45	dc	2e	70	2f	97
e7	a5	1b	1d	ba	f4	6c	64	02	fb	0e	6b	2b	32	c4	c7
87	79	42	3b	82	6c	64	ba	ad	a3	d7	86	1c	50	ec	21
b6	2e	c0	eb	0f	b4	11	82	35	f9	ed	f1	c6	ef	6b	fa
78	4e	3f	bd	42	91	44	89	f4	bf	66	83	97	90	bf	03
9d	49	d0	56	c6	95	6e	8f	1e	46	03	0b	69	f4	c5	b6
2b	9a	c0	8b	c6	35	3b	2c	a5	43	a2	ac	1d	b6	e9	b5
25	8a	22	73	cf	05	51	f2	c5	a1	49	b6	0b	34	b3	ff
89	dc	34	9c	b3	3a	83	55	12	1d	6d	9a	ef	48	19	37
5e	96	f5	45	a5	26	e7	24	c6	65	7b	2e	44	95	6f	55
8d	45	7c	4c	0d	c9	fe	e9	e7	38	0c	aa	3f	41	83	6f
b6	22	9d	d8	cc	72	d0	a5	f7	ea	a2	39	77	de	4d	f1
61	44	25	72	96	e8	74	f3	3f	c7	11	0e	e6	34	39	bb
2d	e5	5f	1a	68	ab	f2	3e	92	0f	1e	7e	10	d3	66	23
de	94	3d	44	4d	64	ab	53	3f	e0	e4	6c	b9	55	29	bd
a8	9b	a9	6b	dc	ee	53	7a	97	d3	81	c3	24	0b	8e	97
a8	02	b9	22	af	04	68	c1	50	20	8a	49	90	e7	30	30
39	30	19	05	fc	a0	38	6e	5a	7c	e7	4d	21	e3	3d	6a
a6	bb	6e	70	b6	9e	65	ae	2a	e3	82	0c	2c	bc	b3	de
17	2f	10	27	d0	74	ab	c7	ef	f6	b7	af	af	38	13	b6
f2	bb	d9	fe	ea	96	49	95	c9	36	8d	b3	64	4c	c3	43
fc	57	a3	de	b6	f8	c8	3e	00	98	b0	11	64	19	70	8e
85	19	c0	f4	b7	fc	62	62	57	b7	21	a3	6d	cd	d7	8c
91	25	3c	7a	81	0a	84	bf	fa	e3	be	f3	f3	24	e3	1e
4b	58	d2	d9	a3	42	a2	a4	d3	41	1e	e5	75	0d	5e	c9
79	4a	d2	cd	ec	2d	d5	38	93	23	bc	20	96	7b	a5	ee
14	81	62	90	40	a6	d5	55	9c	57	75	6a	f3	10	d3	be
59	b8	ed	af	8f	35	d5	4c	9f	0c	0d	22	57	85	48	4e
b0	1d	a5	bb	54	5a	ad	bf	53	15	b9	ea	6a	2e	fe	f1
ee	9c	53	56	da	f8	db	87	29	bb	3e	db	dd	3a	c0	49
fb	a8	25	2c	16	90	4f	fe	7e	33	62	3e	10	67	50	6f
8e	03	8d	ae	d4	77	9d	95	a7	d6	10	49	4b	36	68	c7
cf	26	ae	2a	ac	d1	f7	94	66	5d	0f	41	77	fb	6c	f1
27	fc	88	4d	84	c7	9e	c2	f3	2c	41	2d	41	d0	de	e2
b4	0d	13	fc	e8	32	a1	ed	30	cc	21	95	69	29	81	5a
09	79	b3	be	50	a9	98	e0	e2	29	ec	f6	1f	9e	63	d8
7d	a8	70	8a	e8	8d	fa	9d	d1	a3	36	42	09	da	07	ff
df	bc	f4	dc	0a	d5	76	d1	73	2d	72	42	c7	6f	62	6f
b3	db	1d	2b	3f	78	09	f0	cc	77	b4	e0	e1	e4	16	a7
66	54	51	32	25	5a	93	51	b7	a6	80	e8	eb	4d	dc	06
e8	33	20	c9	bc	47	d5	4f	fc	fb	94	e6	56	57	51	8a
ca	ac	d0	be	3d	c2	52	bb	27	6b	fd	d4	7f	0b	c6	84
e8	b8	a2	2f	e3	3f	a0	a6	85	25	2c	a0	b5	79	6a	99
92	ae	32	ce	82	4c	cb	26	3b	3c	9f	35	87	b6	29	5a
70	07	c0	21	39	da	36	ea	8e	b4	e6	5c	f0	cc	67	b7
2b	f2	0c	bf	b7	a0	8e	08	73	a7	c6	bd	08	70	aa	42
e1	ac	22	84	a0	82	33	26	d0	1a	15	23	9c	4f	0f	49
2f	15	4e	dd	00	5e	ae	f6	ac	3f	35	ce	fd	05	cf	fd
70	a8	0b	3d	bf	40	1b	e9	83	d7	ed	fd	3f	f1	3d	8b
88	51	56	f6	24	13	98	c4	da	02	ab	1c	f6	9b	03	30
9d	f9	e2	83	45	36	5d	5d	83	87	38	1a	a0	4b	d0	a5
6e	d4	0d	4b	02	fc	f2	62	76	ba	13	54	34	da	90	2c
bb	80	50	94	bc	ad	f2	30	9d	15	5e	93	0f	f3	83	4d
54	6d	6e	b7	e3	76	e1	f0	69	a1	d9	eb	02	c7	17	05
76	72	43	a7	22	a5	7f	03	c1	29	c0	99	7e	1d	7b	91
d1	fe	72	98	ca	48	79	de	19	60	bc	03	98	a1	18	b8
86	c9	e9	e5	ba	6e	2f	cb	dd	ac	3b	3f	1a	8a	39	be
c2	c4	92	11	19	e7	1a	30	a0	07	9e	bf	2b	44	79	c8
8d	85	05	03	df	26	7e	b7	f7	eb	fe	c8	ce	ba	a0	79
75	aa	5d	44	c8	0d	b2	0f	cd	5c	1c	07	42	e2	3e	a9
86	23	74	bb	af	1d	51	e9	1a	bb	ea	28	65	f4	4e	79
bc	25	9a	ee	0b	f2	63	b2	2d	ab	ed	b1	54	72	de	ac
85	d6	6c	04	86	4c	01	e1	64	49	b7	c1	c1	e5	33	fc
6c	2d	dd	2a	c5	df	6c	dd	ae	f7	30	38	96	4b	c2	fa
0d	9f	e0	17	b2	97	fc	fa	b4	ca	2b	39	61	45	1c	0c
d0	a9	20	d9	4c	c1	58	00	0e	48	d1	77	ef	c9	a9	cc
b6	21	85	cb	95	5d	31	e0	97	8b	0d	c6	dc	b6	7f	22
96	20	28	50	24	31	40	45	d1	9d	e8	7a	4b	0d	1c	38
8c	90	55	fd	bc	ca	2c	02	a0	5d	55	7c	7c	0b	15	41
ff	ec	f3	a3	e4	7c	19	5f	b4	a7	9f	c6	0b	58	f0	81
48	fa	4b	24	c7	c0	6c	f8	0d	6a	a5	e7	cd	30	6f	fa
5b	7c	7f	7a	85	cc	24	a8	4e	3d	b7	62	3c	95	c6	87
89	1e	2a	2e	16	07	77	29	70	ee	28	78	d3	76	4b	e3
62	26	60	a0	a0	26	4f	e8	5a	e5	8e	36	2d	43	04	64
26	fb	e7	e9	aa	fc	4d	bc	b4	41	a8	38	89	fe	91	13
a9	3c	41	63	66	52	37	1d	e3	55	95	26	d2	f1	2c	1e
28	a1	09	f8	90	9a	dd	c6	a7	4f	ac	30	65	b7	24	05
76	ba	5e	62	fb	42	21	49	c9	5f	93	c8	8a	51	b3	d6
f0	49	cc	c6	9f	30	78	8a	84	f7	62	2b	61	bd	ad	74
f0	35	fa	c6	25	f7	41	45	18	6a	76	9f	80	84	af	78
09	d8	a6	5f	33	da	4c	48	09	17	9d	c8	53	f3	a3	99
c3	71	df	56	96	2a	7e	74	b8	14	88	78	9f	0d	6c	3d
e5	e5	ab	1b	73	e7	65	7a	a6	a8	a1	8a	1a	6b	80	79
2c	e4	4f	bd	5b	b1	e8	5e	46	87	10	ab	d9	d1	af	67
40	43	73	b1	44	64	df	5a	41	37	17	0c	78	d6	8c	d3
e8	cc	54	f8	e8	d8	9a	70	a1	42	91	d5	78	74	4b	c1
5a	a9	cd	15	14	ed	1e	58	b4	03	3c	76	c3	5c	92	68
83	79	63	2f	83	bf	6b	ca	80	16	a3	2c	cf	4a	06	fa
d3	09	6c	96	19	5e	ef	28	07	dc	87	97	c5	b9	90	1c
89	56	e5	02	c3	db	f9	bb	35	40	64	e2	f5	1e	70	c0
3d	ee	63	28	0f	3b	ad	f6	9d	31	b8	0b	11	8e	34	3e
4d	40	e9	1d	82	af	83	8b	ba	46	a0	5c	96	38	7d	3b
82	79	92	e5	dd	73	30	e1	6a	ea	c2	19	c8	50	ac	2f
72	37	df	0f	3b	d7	93	2c	23	b8	c6	ee	92	d3	88	64
ec	3b	fd	44	99	a4	d8	ab	bf	e1	8a	8b	b0	15	f7	77
ea	7d	07	be	ac	40	33	aa	9b	f6	ed	fc	e5	55	a8	ee
8f	39	79	2b	6a	55	0b	bf	73	93	c9	78	4e	42	52	95
ed	ac	86	52	53	87	77	f6	50	e0	59	55	48	38	73	70
43	6f	bc	92	ad	32	a9	3f	ad	a7	36	a4	4f	6e	79	43
17	49	11	ba	12	d2	d9	01	84	46	7b	88	c2	f0	3e	38
69	b1	4d	50	e2	b3	86	77	f3	da	01	20	cc	f7	0c	49
cf	14	e0	65	eb	de	a4	05	5c	b9	00	3a	17	de	9f	b3
d0	b9	db	d1	8b	a1	8f	a7	18	60	81	af	7f	72	93	70
2a	fa	03	79	43	c7	c9	e8	e2	75	c6	0e	d0	30	47	0b
53	04	fb	76	2e	06	42	ee	a3	fa	9a	1d	5c	e5	71	b2
cb	6e	18	df	b0	6c	11	0f	fb	e5	e4	0a	af	ae	88	9b
ca	99	d6	ab	47	92	9a	6c	2f	6d	c6	85	81	33	39	af
c2	90	9d	6a	2c	c9	e3	a6	e6	ca	0b	6b	82	1f	df	c4
9c	a6	89	dc	ac	c2	94	43	62	e0	b8	62	46	ab	0e	27
79	4a	1f	05	d7	c8	3a	02	b1	38	bf	38	5c	8d	c1	98
44	35	68	2b	8a	41	8f	90	e7	24	9a	e1	9d	91	8d	38
f0	9c	e0	9f	3e	8b	2e	02	f9	0e	36	91	59	d2	74	4f
7a	78	1c	96	8e	26	2a	c9	50	4f	f6	e8	75	7d	bc	6e
44	78	4b	42	67	e0	79	c7	e4	2d	2f	c8	b2	90	43	35
16	ea	9d	08	e9	0c	0c	ca	ba	f7	89	10	1a	ba	2d	31
cf	0c	e9	14	6d	84	0b	f2	f3	b3	bd	5b	9d	5c	1c	f0
ab	0c	a9	17	84	6d	c2	75	51	66	1d	12	64	1e	73	3b
7c	9c	7f	f1	84	e9	89	5f	11	67	57	08	48	07	54	e9
23	55	78	67	f4	5c	59	3b	01	98	1a	18	aa	95	32	95
bf	46	df	ba	91	59	bf	7a	08	ac	ee	43	4c	82	87	2f
34	10	c6	f7	b7	40	5f	d3	2a	5e	60	23	58	59	44	8c
8b	e0	24	26	75	0e	bf	fe	1d	57	3d	db	53	b6	61	0b
f0	64	59	b8	b8	bc	8a	0f	99	4d	92	fe	73	7b	9a	0e
85	99	c5	82	67	fe	cc	64	c9	55	30	59	49	da	21	52
df	f2	a3	e1	dc	57	f5	c8	2d	73	a1	ac	2c	a7	51	d2
f7	09	a2	11	5a	2a	11	cd	0a	b3	16	d5	c9	6f	97	3b
d7	d0	fa	d2	d0	7f	28	39	eb	79	8d	83	5f	de	44	7c
7b	36	82	06	ac	76	da	0e	67	82	85	39	f9	06	f6	3f
d3	de	14	bc	9e	bd	f8	da	a4	56	18	28	2f	ca	1e	00
39	5a	87	59	2c	72	0c	7f	a0	77	54	a8	c4	13	66	7e
27	75	74	1c	10	f3	be	04	8b	dc	d1	07	7c	4a	11	41
cd	b7	83	aa	bd	e2	16	fb	9e	21	1e	c6	bc	f3	2a	70
0a	b3	f5	5a	3d	b6	ea	62	66	d8	c8	b8	28	9f	be	03
3b	34	ef	b1	c3	76	b1	a3	01	49	ab	b7	72	41	6c	f4
a9	fe	a1	34	52	18	09	0b	da	cb	6d	e2	cd	a0	85	51
97	11	2c	1d	7b	79	eb	f5	5d	c6	55	16	e4	ff	33	3c
fe	2d	f0	aa	be	8e	e8	af	ce	ed	ae	24	57	65	5e	f2
fa	90	7f	57	4c	2c	5f	48	07	31	c7	ff	8d	f6	0d	67
d7	db	50	4f	a0	6c	e2	bf	06	2b	b6	dc	55	21	f5	e2
d8	af	a0	1c	ec	1a	5f	b8	da	51	33	6b	92	b3	2b	42
61	e2	32	60	d0	ea	60	2a	d4	dc	3c	02	2d	6c	9b	68
1f	ea	5f	4a	de	03	4f	da	9c	b5	b7	2a	91	8d	50	d7
8e	a3	b7	39	f2	ed	ef	82	bc	37	5a	ce	99	33	08	f7
42	47	9e	b7	37	ce	e2	21	7c	20	3c	5f	b2	85	7b	a1
f0	d9	07	27	03	d2	66	f6	c6	be	d0	a6	a8	06	08	47
b8	8d	65	40	01	3d	ec	57	73	86	1c	d2	64	a5	0d	99
d3	a9	7a	0a	98	42	20	f4	44	92	43	63	f6	b9	b7	a7
d1	42	24	e9	ca	58	45	d2	8f	0c	0d	ec	d0	83	8c	08
12	59	6d	7f	7a	e0	5a	41	40	31	1c	cd	56	6a	ad	a4
8d	3a	e7	ed	54	c0	3e	fa	a7	db	bf	77	d0	92	eb	54
b0	0e	d5	00	2f	91	18	4b	9e	a0	39	a1	80	6e	88	c6
33	4a	86	ad	eb	4c	ea	9a	01	73	76	2c	e9	63	3f	ed
f9	00	5c	7d	70	7a	0c	25	88	5c	e7	52	7f	e2	36	f0
8f	8a	62	fa	3a	7d	f7	ae	3d	3e	ed	21	e8	fe	72	bf
ae	0d	20	68	67	c3	67	30	a5	97	7d	2e	2f	ab	75	be
a3	ae	33	be	f6	22	1f	57	fd	48	89	cc	d4	82	ba	d1
4c	d7	48	8c	01	c1	88	8f	f1	29	5c	9b	6f	93	a6	cc
f9	a2	f3	6c	4f	7c	c8	c1	13	16	55	e5	5e	99	b1	9c
b5	71	e2	3d	54	1b	02	f0	46	e3	b5	b2	f7	5f	0d	fa
44	29	2b	47	23	0c	e0	61	43	0b	d4	d3	36	f9	35	b9
20	cb	5a	a1	6a	71	09	a8	79	ee	95	85	1e	bd	36	95
11	82	af	20	a7	6d	05	80	31	19	23	3c	dc	a6	6c	c8
99	28	27	b5	da	d1	97	85	bd	ed	55	75	6e	7b	0b	5b
ee	cb	58	45	98	83	7d	a5	8e	93	c4	70	47	cc	f2	43
c0	16	d9	38	7f	2c	9d	3e	28	0d	ee	3c	a0	f0	62	65
07	bf	73	74	ee	91	37	20	86	57	e6	63	2a	60	3a	0f
76	19	52	81	d9	57	68	3d	d2	e6	31	65	bc	31	63	e2
56	4a	16	6e	73	8e	cf	56	05	2d	19	02	56	14	af	df
af	7a	3c	aa	86	2e	d6	57	2a	79	83	f5	a3	a8	05	ba
0d	6f	ff	b1	d0	00	48	26	e4	e8	d4	71	42	eb	5d	af
44	66	c1	e2	81	37	69	ee	cb	93	2c	6c	06	45	f0	f6
65	b7	11	73	01	6b	b1	d1	b3	14	63	68	2a	b8	29	0e
b7	8b	45	33	68	bf	51	66	18	9f	2b	17	eb	e3	75	c8
4e	58	38	4b	d0	fa	ae	82	60	3c	93	d3	d5	a1	af	38
57	47	3d	25	cc	58	9d	f6	98	eb	08	33	48	96	96	00
f7	6c	31	fb	ef	25	82	5a	a3	f8	dc	42	90	ca	f2	2a
bf	51	d1	f5	6f	87	1c	ff	26	53	29	3b	83	a4	52	ab
1e	48	d8	37	d2	fc	a9	9e	c7	9f	3a	4e	ec	d7	c3	a6
98	7a	8d	88	56	94	8d	15	18	e0	23	b3	28	14	66	cf
a9	f7	ba	dc	46	3d	27	23	9d	b5	88	56	e2	70	11	d2
eb	11	8c	e0	08	fa	a6	58	04	cb	2f	8d	ff	8f	45	f8
a0	a1	78	db	e9	5a	a3	11	4a	20	33	5e	7a	94	90	28
7d	a2	a4	ba	32	47	b3	bd	08	8f	09	6e	30	5a	5a	5b
8a	1e	1f	c2	13	76	79	af	d2	75	a8	b7	e4	6d	be	d8
d5	62	3b	2e	3f	f9	98	db	ac	c7	6d	69	a6	f0	12	91
22	7b	9e	b9	08	29	49	29	cc	ee	65	0a	6b	de	15	16
67	d7	af	3d	36	ce	67	59	56	63	65	21	70	45	c0	0c
a4	76	cd	42	39	9b	9d	3c	f1	22	44	6f	c3	cf	8c	53
84	fd	ff	14	fd	02	ba	3c	0a	72	cb	79	35	81	15	f0
07	6b	f4	fc	6e	6e	68	0e	ed	59	dc	2f	7d	c8	db	74
19	2e	5b	d7	5b	cd	79	29	3c	e9	b7	c1	3b	40	cc	58
0b	1f	7d	22	3a	5c	64	35	d7	5c	de	5e	97	44	1b	a1
01	30	55	5e	6a	ad	01	1e	04	52	a0	5c	7e	c2	93	5b
45	73	ca	18	6e	66	e0	f8	09	15	72	3c	a5	64	e7	f5
d1	05	41	45	4b	ba	07	b2	fe	95	d0	e5	62	83	1e	f9
d8	5e	8f	f4	dd	7c	99	d6	1d	d0	3e	fb	ae	c3	9a	0e
56	84	3f	06	d8	ac	7b	5f	94	69	cb	87	94	c2	3b	88
e8	f7	3b	b9	ec	58	89	66	a3	e9	d0	9e	71	ae	d9	8c
45	1a	16	57	ba	dc	b2	63	81	5c	f5	34	3e	24	2e	f6
0d	03	b0	30	7e	c4	2c	e9	c9	72	e9	2c	f0	e3	11	64
2d	b6	f6	a6	0e	0b	08	f5	f8	f7	e0	53	e9	2f	ed	c8
d5	11	6b	b0	0e	2c	52	ec	bb	3a	3e	00	a7	3c	2f	04
e2	a4	76	11	31	da	bf	36	fd	8f	f1	e5	1f	61	97	cf
f4	a7	49	d6	30	49	f1	fd	fc	73	a3	cb	f5	c8	a6	3a
e2	a0	d1	d9	05	ca	62	54	37	3d	e3	83	39	84	a6	d7
16	33	45	0c	1c	f9	36	e8	14	3b	c0	41	50	38	e6	35
04	97	09	fc	cf	f5	82	39	3c	d2	28	3e	30	b1	f6	8b
1c	9f	ff	bc	72	bc	bc	bc	35	3f	59	72	8e	a5	31	fb
9a	a6	25	74	14	5d	a0	5f	52	15	75	ea	78	f9	a5	7c
96	6f	75	6e	7e	bd	c9	46	12	09	5f	19	32	7e	00	7f
0b	4d	45	48	7d	8c	4f	24	55	dc	b4	31	dd	fa	b9	ef
ac	c9	1f	0f	59	62	07	19	e6	38	f5	8f	55	98	96	79
8e	01	f6	cc	eb	a0	08	f1	08	7f	3d	89	00	25	68	bb
d9	56	a8	6a	24	f6	58	04	1c	05	74	5c	a9	65	9a	6e
34	3d	83	7f	ef	2c	9b	b0	b6	4d	a7	77	65	11	a9	f1
14	7c	35	37	e6	e0	26	10	cd	15	57	3d	3b	a4	a5	f0
70	1a	36	b4	2b	65	0c	9a	37	ca	12	d9	bc	55	ab	33
2a	59	14	06	9a	97	ed	7c	69	f9	c1	1f	2e	16	9b	10
64	34	18	93	f1	6e	fa	13	68	3f	4b	9a	e0	e2	06	c1
a4	18	d1	93	ea	7b	49	38	fd	3b	e7	a7	cf	81	95	0b
b5	81	b5	0e	52	4a	b4	48	e2	4a	d2	dd	f9	04	8e	bb
9d	d4	3b	97	bc	07	bc	95	84	fb	3c	99	ce	46	a0	7c
da	e3	b1	f6	8a	26	79	61	48	3b	2f	69	9e	09	42	65
91	6f	e8	b4	90	33	50	1b	2d	3a	66	be	0c	dd	30	b8
a9	0f	38	f6	b0	f2	b3	b4	0a	7c	20	c4	43	5e	f1	61
2e	4e	34	7c	06	1e	ce	0a	1e	6c	dd	62	d2	b8	cd	89
ef	01	56	5c	28	bc	6f	89	d4	90	9f	be	f2	5a	e6	60
99	41	80	b0	b9	d1	0a	69	1e	02	36	69	5d	62	9d	c3
69	cc	e9	79	37	c2	33	60	9b	c0	08	42	41	06	fe	d0
5b	9e	13	86	d6	09	bc	ab	28	dd	71	0e	11	de	30	35
c9	b4	7b	5d	44	7a	47	88	83	53	85	ba	dd	23	94	76
23	ee	8f	1e	24	da	80	76	56	dc	e1	2b	62	08	db	49
21	0a	26	5c	85	cd	69	70	96	5e	e6	fe	d8	eb	8b	6a
c1	cc	de	11	40	23	ec	3d	61	f7	a0	dd	a1	09	ed	d9
c3	38	2a	f5	ee	fe	7b	6f	26	5d	4a	6e	a3	07	14	c3
26	f1	1e	fe	d0	91	96	ed	4b	50	a5	9f	26	b1	d3	63
f2	6a	fa	7c	4b	5b	36	8d	8f	99	a0	16	0e	b9	36	78
ed	19	26	f2	9a	d6	ff	f8	6c	05	42	d2	c7	83	b1	2d
e7	25	51	a5	47	3c	f7	5b	b1	a6	28	26	f1	7b	d1	7f
46	06	0a	8c	3b	34	2e	bf	99	7b	ed	2a	de	95	c9	52
b1	30	6a	d9	d0	52	6e	61	a3	44	b7	34	ca	31	eb	7e
40	a1	6c	84	54	34	5c	04	55	e0	0e	a1	bc	ea	3d	ff
e5	7d	12	b6	c9	92	96	f0	56	cc	78	98	df	f2	4f	cf
d4	31	d0	dc	6b	f4	bd	e6	84	9c	81	52	d0	a7	3a	3a
69	24	e9	27	64	8a	42	37	38	0a	cb	db	4d	f9	82	b2
b1	d8	27	07	98	95	eb	f1	b3	98	f3	cf	04	9e	60	d7
1d	bf	86	0c	10	89	db	6a	44	25	18	07	e3	cd	df	16
d0	37	b0	de	34	87	4f	59	51	af	b2	23	07	8e	07	96
a7	0e	ff	4a	bf	a8	63	48	c0	0f	da	a9	f3	8c	ed	5f
47	22	78	19	bd	d9	c1	98	47	7d	bd	05	59	63	3f	ab
3f	7a	fd	f4	30	6f	e7	8f	11	ae	57	03	3c	5b	02	5f
47	6f	bd	6f	33	3d	60	09	cd	11	6d	46	f0	6b	fa	91
4d	45	ac	1d	af	23	7a	52	ef	bd	71	ac	1e	7c	c0	0e
99	1b	9b	e8	74	28	a0	f7	08	a6	9e	86	44	99	b1	20
20	2e	5d	59	87	ba	74	81	a5	79	bc	d4	41	1a	5a	31
79	a0	f1	04	0c	4a	3c	bf	0f	ad	2c	74	83	fc	ea	ef
e7	93	79	d9	cd	e1	c9	bf	f0	2c	86	2a	be	4f	6c	62
27	ff	33	b7	d4	25	08	97	80	82	57	bf	7d	15	d5	9c
36	2f	36	bf	79	ff	95	f5	c2	25	cf	a1	6c	16	1e	52
86	f7	08	e0	07	b7	9a	84	16	fd	3f	51	2c	8f	76	d2
cd	b6	41	09	3a	ba	18	8e	92	86	89	58	a4	1b	f1	e7
ca	2a	9c	54	2b	ea	92	a2	c5	29	0b	c9	e0	2d	0a	67
26	4e	f3	40	b8	64	4c	03	6f	74	08	28	ec	5e	ca	40
eb	bf	4f	bc	5f	31	00	04	46	64	b6	64	d0	b4	67	89
bd	70	e0	0f	74	53	af	83	ad	9e	ae	b4	c2	07	79	bd
61	df	21	94	4f	70	9a	09	56	7c	69	ed	5b	a4	b9	1e
be	16	43	b5	03	3d	ad	98	29	c2	7e	dd	a1	c9	e1	50
bf	52	bb	31	88	da	c1	9d	86	6a	bf	a7	7e	34	a7	d2
94	8c	70	39	b9	a6	30	6d	90	78	3f	54	05	0c	9e	a2
28	85	e4	93	d5	42	ba	3b	60	20	28	95	94	1d	48	42
92	9f	30	99	24	19	65	6f	98	08	d1	82	51	e6	a0	1e
0f	c0	b3	20	31	35	de	15	81	a8	7a	f7	e2	96	9f	a4
2f	9c	6c	2d	9b	ed	1c	2c	b0	d6	fb	64	61	1d	69	e6
b7	9e	30	ec	81	b3	98	6f	2a	1b	1a	81	b4	be	71	6d
59	49	1b	8b	9f	5b	1c	8b	ff	9f	49	e4	60	85	12	7b
83	3d	07	6c	0c	87	ed	d8	76	69	52	25	4a	ba	0c	14
fc	43	de	7a	8d	f6	e7	cb	a3	6b	28	af	15	f6	10	3f
16	09	05	0b	31	21	3b	11	37	9f	80	de	aa	dd	c2	42
a2	72	b7	af	ad	c0	df	41	e4	ef	0c	6f	d1	1e	3f	d6
af	e0	d0	d6	ba	a4	5b	e7	59	5a	9a	18	e8	d9	26	62
10	2f	39	73	b8	d1	a2	b5	df	4a	d0	68	03	a8	78	77
a0	35	03	6d	aa	ba	58	ef	f1	81	d3	65	dd	73	e7	be
97	17	5f	4e	79	36	86	a5	dd	b3	30	90	fb	b2	12	2a
48	1d	0f	d3	68	60	4b	3a	42	ed	82	47	9e	49	d3	6d
cc	f2	4f	e5	6d	f5	fc	1b	44	4b	69	8b	c0	2d	73	fa
77	63	17	6a	92	9a	61	ed	fb	8e	8c	e9	0a	23	9a	12
a1	cb	5f	d6	4e	eb	53	27	61	aa	58	82	e8	e2	83	88
75	9b	df	7f	fc	50	5f	ac	6a	6c	80	bc	54	12	94	b8
91	a3	a0	ef	10	50	13	04	5a	31	34	32	1e	a4	8f	fc
bd	c6	a7	1a	5e	a5	0c	ca	bd	53	8d	f8	6d	03	3f	b9
35	a3	51	59	c8	74	45	ea	6b	99	4c	da	6b	fa	f4	17
10	0e	83	c4	03	9f	60	9f	b7	dd	ab	e4	87	ed	05	c8
d3	d2	c8	7e	e2	99	84	1c	8d	61	47	12	e8	3c	d2	e1
05	97	4b	80	29	a5	40	6e	4b	d0	7e	b4	63	c4	fb	af
46	5b	f7	a5	a4	ab	07	e8	35	fc	39	48	cb	17	4a	f6
03	6c	a3	64	94	9a	96	24	99	ff	b8	1a	28	bb	f7	ad
4a	e8	13	57	7b	38	b8	7e	56	e2	63	57	73	2b	5a	69
02	4b	f1	0c	d9	11	8c	38	6d	1c	c6	78	ea	6c	01	08
79	53	9a	58	78	16	e4	3d	9a	e3	56	13	eb	fc	55	e9
2c	18	b9	84	d5	64	a2	ed	53	41	1b	75	5c	36	19	12
5c	cd	09	01	3b	e0	a1	8d	95	3b	72	3f	e8	66	22	48
ab	6f	d4	88	33	6c	d7	20	dd	5a	a7	1d	b5	da	29	bf
d2	c4	56	80	ac	f3	da	e4	60	00	24	5b	e6	e1	4a	e3
38	53	30	cc	74	ee	d4	5c	4f	eb	be	10	4e	af	ed	98
c7	c3	6a	92	cb	fd	59	0f	11	e6	3e	d6	cf	31	59	2e
c0	8f	ec	dd	8d	79	32	3c	02	08	f0	26	00	f7	1b	7c
58	2c	47	ea	1c	6d	7a	bd	cb	43	01	f7	e4	39	f0	ee
7d	3b	57	7f	d7	b7	e8	ef	35	74	26	30	6b	74	09	69
69	66	b8	78	cb	47	d9	18	c0	3e	a7	00	fc	08	a0	91
86	aa	9f	06	20	7b	a5	41	a3	09	93	8f	b8	e8	cc	29
44	67	56	c8	06	c3	92	97	27	c7	b9	7e	89	01	56	bc
a2	a4	f8	c0	ff	ea	17	83	01	b9	b3	0d	07	bb	29	d6
22	9d	bb	6e	18	68	89	b2	bf	e9	e0	1c	36	6a	06	2c
d0	3d	17	1c	50	4a	a5	11	3e	c2	13	b0	6e	9b	68	f5
a4	fa	4a	4f	f9	0d	5d	38	ca	89	37	96	40	42	95	34
66	cf	6c	31	34	5a	af	ec	f2	3c	5a	68	05	a0	9c	4f
4b	23	87	64	11	55	4b	9c	02	c7	4f	bd	13	c7	dc	49
6f	ce	f7	65	14	16	06	b5	27	c4	aa	3b	b4	b2	35	3e
81	3d	db	15	7c	70	fa	cd	5e	be	aa	95	7b	ac	ea	a2
ea	26	b1	76	c9	52	6e	03	9b	53	97	91	25	f1	fa	2d
be	ab	e9	62	8e	6d	ec	a3	c1	73	2c	77	9c	22	66	74
67	eb	d0	cf	a2	8a	6c	e9	fe	b4	90	75	29	57	7d	4c
9d	05	c9	80	84	69	35	4f	32	50	d5	fc	0c	2e	8d	32
64	3d	76	84	af	d5	cd	41	69	3e	4a	26	91	a9	2c	59
eb	dd	af	9e	66	4f	1b	38	82	28	31	5a	4c	a3	59	8f
00	b9	b1	7b	c4	3a	06	9d	74	21	8d	7d	aa	f0	05	06
8b	b9	c6	28	7b	97	f5	3e	c1	f6	64	57	1c	15	d8	98
4e	2b	b7	af	8e	6d	ca	dc	38	95	c6	3e	16	06	72	e4
c0	b1	1b	03	05	97	a6	58	f5	a8	66	9e	05	29	72	40
e3	8d	2d	0d	ef	57	e6	a2	98	dc	12	01	1c	e5	a2	fb
b1	31	ad	ab	21	a0	21	20	aa	b5	99	68	f4	00	db	03
21	c9	29	ff	7c	95	e2	fc	de	4d	c4	c5	b3	08	0e	91
06	e1	d1	d3	b5	13	47	b2	b5	a7	16	cf	ed	27	a9	e4
9b	12	2c	9b	9f	15	3e	9f	e1	7f	31	79	85	38	8c	57
0f	2d	eb	cc	be	a7	5e	bb	15	a2	e0	7c	3b	26	07	b6
00	4c	43	8d	76	a8	66	f8	4c	28	49	e2	0a	b1	c4	0c
7b	cd	1f	4c	02	b3	2f	0c	a5	d9	0a	6b	ff	2b	f9	50
af	5a	d8	72	28	d5	11	4f	7f	75	dd	34	c3	b5	72	79
48	32	bd	2d	41	4b	16	9e	cb	91	5f	f9	d6	78	ce	0d
07	83	3c	f3	c6	90	a0	da	08	21	8b	ee	f5	7c	ab	7e
1e	d2	30	f8	de	cb	fd	96	88	61	5f	b3	1e	25	99	2b
33	84	b8	8e	c4	68	ce	3c	dc	4e	7e	48	16	5a	b1	d4
fb	bd	9e	6f	7f	d3	d1	d8	b6	d6	1a	8f	b6	2d	a2	00
13	9a	0a	0c	0b	fb	6d	44	b1	73	13	c4	fc	c1	a6	26
5e	e1	e2	b4	6c	fb	06	22	8d	fd	bd	f1	45	14	84	a0
d8	38	1f	7d	8b	74	38	e9	1b	f2	ce	63	9c	78	4f	17
e8	e4	2a	b9	4f	e6	32	04	6a	f8	0e	39	c4	0e	e4	24
4f	a6	bb	b6	dd	54	4f	8a	16	1f	98	c5	60	39	13	9e
62	fe	90	63	5b	8b	aa	c5	31	5b	b3	a5	51	2b	1b	0d
21	3b	f9	55	5f	03	81	e3	4d	c5	a6	fd	3f	43	9c	a0
0c	47	5b	8f	b0	84	b7	58	a8	cc	fb	2b	af	ac	18	cc
60	e0	a1	3f	13	56	fa	14	5e	cf	56	86	2c	48	c5	ab
14	27	65	77	56	1d	78	c1	eb	73	2a	e6	7f	48	f4	44
03	d3	e2	29	b6	12	f8	b0	ca	cc	0b	f0	53	5e	d1	58
80	b3	70	11	11	c0	66	05	ab	f0	0b	2e	b2	a3	d8	e3
52	fc	a0	fb	6a	e7	23	e1	dc	ab	21	eb	dc	59	75	a9
a3	12	d4	76	1e	cf	d7	d8	9d	36	87	bd	4a	f0	10	27
76	66	67	74	3b	14	eb	21	ac	60	d9	a6	45	7b	c1	87
ec	ba	e9	80	c6	24	b1	61	2e	28	ae	bc	67	c3	7d	b4
a5	0f	0c	b6	14	df	8c	37	51	52	58	29	25	11	56	6c
88	43	80	3d	8e	7a	d1	1f	bf	7d	f2	e6	c2	21	04	27
ff	0c	68	b5	4c	fe	9a	74	22	4f	1e	49	26	05	36	54
80	b3	44	61	d8	4a	5e	a8	41	76	43	09	8d	cd	6d	52
ef	92	96	fa	96	c8	37	9e	0d	1f	b6	92	fe	da	90	51
ac	01	2d	6e	b0	a5	54	99	ca	c5	ae	ce	63	dd	96	39
40	50	e6	5d	d7	e3	02	3f	a8	ae	17	f1	61	ce	f8	56
09	d3	42	a3	40	6d	5d	1c	66	13	9a	cc	59	ca	93	63
98	f6	46	1f	ba	2c	79	6b	9b	b3	66	0e	0c	37	bf	03
76	a4	d4	f7	b6	c7	6f	ac	8f	c7	e9	0b	2e	46	d5	6b
5e	94	89	bd	02	3c	54	da	db	af	c8	83	5d	88	4a	14
e6	5b	37	43	59	13	fe	9e	fe	65	af	29	38	d4	d6	16
f2	85	a9	df	1f	96	b8	fd	03	50	7b	30	ec	8e	85	da
d4	9b	fd	ce	94	6d	9b	5c	01	3f	24	8f	65	2e	01	7c
fc	45	01	dd	1c	1d	2b	ec	84	5d	2e	12	3c	77	d4	a7
04	9b	2a	6f	b3	8d	ce	d4	6a	d9	da	12	db	d1	19	e8
c5	39	ea	45	fe	c6	1b	c3	4b	fc	b4	a2	26	16	35	27
94	7b	07	e5	28	4c	4d	52	0f	36	ff	7e	a2	2b	0d	99
d0	52	03	45	f3	d1	56	a4	33	a8	81	9a	1a	ba	e2	7f
3c	8f	e5	41	9e	df	51	7d	2d	20	04	6f	61	3b	e6	b5
19	e2	a5	88	6b	f6	1f	cc	55	43	60	d0	72	93	fa	05
42	6a	b9	75	6d	27	61	91	ff	67	34	c9	54	30	6a	24
17	ad	a1	2c	34	f2	f3	dc	f5	0a	cb	10	13	0e	84	a2
b6	d6	9e	75	1f	42	68	c1	37	3e	79	1c	25	a2	bd	95
77	91	7c	0d	fb	f3	9d	5f	7e	88	60	69	e3	15	1b	52
cf	d1	94	5c	14	83	08	5f	f1	26	39	8e	11	e4	8b	04
86	04	f3	25	b0	d0	2e	af	88	a7	26	75	f7	56	2c	cb
ec	c9	6c	fd	c5	91	45	53	82	80	e6	8c	0a	18	ba	3d
20	f2	06	0b	6c	26	be	03	9b	ca	cf	50	26	3a	fa	19
da	b7	66	f9	82	d9	ea	e9	9a	12	82	6a	67	ba	27	cc
65	6a	d2	46	cd	79	fd	2d	8a	99	57	7f	22	3d	ce	25
ce	4c	d5	d4	d8	92	2b	a6	da	58	35	f2	80	70	05	bf
e2	73	5f	eb	63	d8	ea	40	42	e6	fb	ae	a8	5f	59	e1
85	7c	61	58	b5	2e	2a	ea	44	70	41	15	05	a1	73	92
a2	1d	9f	37	6a	36	d1	03	65	74	ed	33	33	db	22	33
a0	c7	81	dd	a7	a0	13	e3	55	8b	b7	d3	ec	d9	59	d1
47	d8	7b	d9	7e	be	99	7a	7a	f5	d1	d6	e2	8d	61	76
44	c4	30	60	66	99	9b	4d	33	7a	12	f1	c9	bc	ea	b1
a6	c2	5c	43	d5	db	b1	63	91	d8	bc	ce	ef	eb	af	a3
9e	79	e3	dc	43	44	c2	e8	7c	04	ef	bf	90	de	07	1d
52	fe	7d	38	de	8c	a9	38	d1	e9	81	4d	b0	50	c6	65
b0	01	a1	24	6e	bd	c0	aa	64	f7	5f	34	41	a6	24	b3
0d	ca	22	67	a3	b8	c0	99	07	f9	de	92	f5	66	69	fb
9d	ba	2b	cd	03	de	4b	04	18	60	c6	a1	a0	50	9b	4f
13	be	3a	0d	f0	2a	57	c7	73	4b	bd	a2	ce	91	4b	d3
5c	0b	38	25	8c	77	ca	48	3e	62	d7	28	e5	5b	3b	59
11	db	2c	c8	91	82	f3	d7	61	50	ff	ef	45	26	73	e8
92	65	6a	c1	8c	1a	9d	c5	ea	7c	7c	11	d8	82	08	36
63	4b	a6	d6	b5	c4	30	32	8b	39	6c	b9	4b	9a	da	43
81	9e	6c	1f	3b	39	58	b2	2b	31	7e	18	d3	3e	b5	d1
12	b4	fb	93	6e	eb	b8	46	ef	ed	2d	be	90	fd	1b	f7
a9	73	69	fe	17	7c	26	d9	97	ef	a3	61	3b	15	1e	65
cd	24	22	5d	4a	0f	70	1b	ce	8b	be	66	74	46	37	41
32	fa	c2	f1	8c	ab	e7	3c	16	f2	ca	27	bf	d0	05	64
fe	6b	93	2f	ed	07	40	52	a2	39	3d	dd	aa	85	0f	b9
36	c6	58	39	de	c8	b5	05	0c	2c	bc	e2	2a	9f	61	68
ba	75	e2	58	21	6d	ea	25	44	33	30	cf	de	ca	96	ab
c3	94	29	3c	68	00	e8	17	44	36	d1	36	db	7c	01	6c
b8	30	58	a4	9c	11	78	5d	9c	18	72	0e	5b	8a	9f	07
37	c2	15	62	37	43	c0	78	f5	a4	34	a0	9d	a2	d3	3e
1e	b4	8c	50	02	c2	0c	40	f7	2e	c3	b5	92	7b	38	6c
ac	c8	45	ca	63	77	f1	20	c7	fc	bc	2e	c6	a5	57	5a
e0	fa	25	92	e9	04	c3	47	a4	0c	4c	c7	5f	af	9d	f6
f7	57	d0	9b	d4	27	d2	23	95	ea	60	54	a7	68	15	54
 	 	
j	m	1	ö	Ô	6	Ë	ß	û	_	.	)	S	¹	®	½
C	i	ù	´	t	±	\	.	j	¶	.	ú	/	a	ª	é
¡	ú	H	E	þ	À	.	Ü	£	´	0		ê	J	¿	Ë
.	ö	×	1	d	W	ÿ	o	n	·	.	.	A		ð	.
Ù	^		ð	S	.	7	.	è	ã	á	6	I	ü	È	Ï
S	W	/	#	î	U	Y	¯	ñ	.	»	t	\	°	L	.
%	Î	æ	ã	Ä	ð	²	.	.		ý	½	!	.	þ	[
:	í	ô	.	ü	.	¾	à	ä	-	î	y	z	1	.	¨
e	g	|	%	å		#		.	Ö	#	y	ô	r	1	Ö
½	.	z	¼	Ä	§	.	.		ã	.	\	Ô	.	H	9
s	e	Õ	.	î	Å	÷	!	þ	"	.	_	6	6	:	'
¢	é	T	.	.	w	r	.	ø	E	.	¥	y	8	.	$
î	¾	D	N	.	Ü	Þ	5	.		7	&	ó	6	e	e
E	.	D		.	µ	)	C	T	6	.	.	î	.	*	Ç
å	:	.	v		`	 	(		Ç	.	.	 	°	6	â
É	q	f	º	W	¿	h	K		°		±	º	.	µ	.
®		Ý	û	`	H	.	°	È	¨			.	T	á	$
.	.	.	B	R	_	J	.	M	Ü	.	í	W		½	.
a	[		ç	Ô	.	ö	.	÷		å		½	²	.	.
X	.	É	n	Ì	%	.	Ô	¬	.	g	Û	~	A	Ý	ä
.	Ò	Â	j	Ç	.	ª	.		È	Á	J	.	&	¹	8
.	.	#	s	&	<	Ï	M	ü		+	.	«	ã	Ï	±
.	)	 	í	¢	.	.	ñ	×	Ç	8	È	.	ù	n	W
X	.	s	Ï	e		/	º	+	i	_	³	÷	\	í	©
	?	x	ë	:	C	.	r	¨	k	õ	x	6	Z	>	=
"	¨	³	f	.	Î	ù	.	.	@	m	.	.	¹	R	
5	.	Ø	s	í	.	X	Å	F	.	T	³	.	.	1	.
*	ì	f	+	¨	|	.	.	·	p	Æ	.	Ô	ß	ñ	ê
.	V	§	ú	]	Ð		>	ä	X	r	.	C	.	¸	P
q	Ú	á	.	Ú	.	ê	Û	»	.	.	é	Â	è	E	ô
ø	]	f	¹	æ	¸	6	H	D	E	.	:	Q		L	û
±	z	<	Ñ	²	Ê	(	ë	.	.	¸	b	e	.	á	î
.	.	'	é	É	.	¹	.	`	4	è	u	+	f	Á	K
µ	¤	G	.	.	x	-	.	À	$	Å	.	6	d	Ú	
.	±	.	.	Ç	×	.	ã	.	Ñ	&	.	y		k	.
ú	¼	ú	©	·	0		.	¬	.	.	%	²	Ö	P	k
ª	ë	æ	¥	f	Ò	v		.	`	=	@	è	"	.	8
±	Ê		³	g	$	¥	s	Ã	-	K	.	.	O	.	ù
"	a	Ê	h	 	è	.	È	Ç	;	E	.	.	ç	§	q
µ	`	.	½	;	ð	'	Ï	¤	5	.	1	Z	ç	L	'
.	.	b	¤	.	D		o	<	.	H	d	B	F	<	.
ô	©	à	º	!	W		½	i	:	.	V	«	Ò	Ú	=
.	¬	ä	¦	Ñ	`	ß	£	þ	.	D		"	³		.
E	Â	t	B	F		.	ä	.	\	ã	®	0	1	ÿ	.
£	Ì	.	.	à	@	|	Å	e	&	º	Ö	.	z	.	.
¹	ï	ß	Æ	õ	f	þ	.	.	²	Ü	=	.	û	ò	³
Y	g	u	.	2	õ	.	õ	.	H	$	u	B	=	Ü	i
A	.	Î	ù		4	Ô	G		¬	.	d	(	c	¡	.
Õ	r	Ü	C	.	õ	t	s	B	ñ	'	.	J	0	ß	ß
½	©	J	¶		ë	y	Ë	|	.	D	N	\	j	Â	}
.	.	.	×	Ì	.	.	Â	.	¥	Ú	±	.	Â	K	á
/	Î	G	}	Q	c	ô	t	H	.	$	á	_	.	)	B
.	.	p	Ä	;	@	¾	ë	¤	º	|			.	Ù	.
Þ	ã	·	-	.	À	W	s	R	ö	V	u	.	%	.	Ô
1	}	^	m	Ö	.	y	^	.	4	.	á	£		^	f
g	o	.		4	n	â		.	½	8	.	×	=	ä	[
.	¡	÷		.	f	û	.	Õ	â	.	Â			Ú
B	í	.	Ç	²	T	+	m	q	.	p	.	.	_	.	ä
O	g	¥	æ	Ç	>	I	_		²	Ì	\	Ó	ä	í	v
w	;	C	!	w	Ñ	Ì	h	û	.	é	ÿ		,	d	.
k	.		Ý	8	¥	.	ù	>			6	æ	B	S	
.	g	.		.	9	e	Ý	m	¡	H		h	á	°	.
5	¿	X	¬	Ý	E	H	C	a	.	é		®	X	.	.
	s	.	í		Æ	Å	.	a	Ó	Ê	.	~	Û	ì	Ý
.		.	.	.		.	J	i	¶	é	.	"	.	²	ù
ì	O	ø	Á	.	õ	.	i		©	Í	X	Æ	0	±	
L	.	»	 	4	.		$	.	¾	§	.	Û	T		í
ø		.	.	ÿ	.	â	.	.	.	¼	¢	\	.	.	á
.	.	'	.	.	a	æ	^	æ		D	.		r	.	C
×	ã	(	=	P	¦	.	ù	Ò	X	,	®	C	%	.	<
ð	ì	]	Ý	0	=	X	.	å	.	P	º	X	.	Ò	7
0	h	.	.	þ	.	¨	.	Ý	>	ý	*	.	(		Ö
¬	5	.	.	Ñ	.	]	.	.	.	.	F	X	È	£	á
E	.	E	W	ñ	k	|	}	.	A	u	.	/		´	ó
§	Í	ï	p	à	Z	½	Æ	à	Y	_	.	W	.	.	ß
(	d	Ê	@	È	§	]		ó	Ú	í	?		.	¶	É
.	ó	.	×	Û	®	ð	.	6	ö	(	l	I	.	j	¡
Ñ	_	¤	Ð	Ã	.	Ö	-	!	M	º	Å	R	ö	`	Ô
±		#	è	.	¡	¼	.	C	y	ö	î	<	.	G	k
±	e	.	u	ö	Z	£	±	.	®	L	}	I	Á	!	.
	»	ú	u	ä	.	C	Ï	®	>	'	â	Ã	¬	:	.
;	»	R	Ü	è	Ð	5	Q	@	ö	.	ô	{	\	Û	Ä
D	.	`	6	[	²	|	.	E	`	.	.	.	d	s	,
X	.	.	.	ó	.	p	¨	C	.	·	.	þ	d	ë	.
¥	.	'	«	/	.	:	.		.	.	.	Ï	;	ä	Æ
	2		ü	¼	ü	.	C	Ò	Ï	¦	m	^	.	.	z
x	Î	.		É	Æ	?	£		£	p	ß	*	.	.	±
.	.	3	.	_	;	.	?	c	â	¾	ß	n	T	.	.
I	t	>	1	+	u	.	+	ü	.	.	z	Þ	¼	S	b
r	.		|	.		l	©	æ	-	Ø	f	{	¿	´	
w	Õ	·	s	G	.	o	K	ë	Ë	Á	ß	a	ì	¯	ø
ù	.	Á	h	n	X	%	¬	i	.	ð	C		.		³
L	c	¯	;	ã	.	'	]	.	>	¯	ç	¾	`	ã	à
.	.	|	Z	U	j	f	~	,	T		·	ø	#	.	Ç
|	.	½	z	.	2	L	x	.	O	Ý		.		à	õ
.	Ü	8	Á	H	D	t		Û	^	.			.	.	ü
'	 	B	í	Ó	Ø	6	¿	.	.	.	%	%	9	t	J
Û	{	^	&	.	.	A	K	¦	Û	¬	.	¸	=	B	®
;	Ä		{	.		{	.	r	±		i	ð	¥	±	.
@	Y	.	È	V	¼	a	#	X	µ	¦	Æ	Û	d	È	Y
Ú	²	Ó	&	Ê	°	.	-	¦	y	K	Ä	.	p	á	.
b	Ã	W	.	D	w	*	.	.	ê	T	T	.	Ñ	Æ	U
!	í	>	º	ø	y	}	k	e	¶		à	K	.	Ó	ö
Õ	+	®	4	.	"	Ö	.	ÿ	6		p	s	ô	X	Ý
»	.	.	Y	;	]	°	õ	¦	®	^	x	*	Ð	t
Æ	;	R	u	b	v	.	&	¥	Í	)	·	+	-	w	4
	7	.	.		l	¢	.	B	X		%	§	Q	R	.
¿	'	}	P	ñ	ø	1	I	°	½	Ò	.	.	Ó	X	.
.	y	¦	Ä	â		D	Ø	e	¢	¿	:	R	Ã	¸	C
Å	.	p	,	Ý	æ	¹	Ø	.	u	.	X	.	.	÷	±
.	à	.	ô		.	W		Ñ		.	È	.	.	Â	.
´	Ý	Á	Ï	.	2	é	e	ý	z	.	§	ï	.	.	.
Ú	E	;	á	.	±	K	n	|	õ	Ë	,	.	Ã	.	¢
ª	.	â	Ä	¢	j		w	'	\	m	.	Â	D	j	.
.	v	M	J	ä	ð	x	=	=	Å	s	.	Ç	~	.	.
.	.	.	]	7	#	Ð	.	.	n	¼	#	ü	ÿ	H	
t	Õ	z	*	Æ	ó	T	%	l	]	Ð	q	Í	Q	.	I
Á	.	û	T	?	õ	É	5	.	Ô	^	j	N	Ó		'
¹	¨	Ó	.	·	?	v	.	B	þ	Ñ	Û		$	=	w
û	ì	©		e	Â	ó	¤	Þ		"	`	m	ª	Ö	?
	F	.	H	è	í	£	õ	'	'	É	e	&	É	H	Ç
d	A	ç	U	Å	q	0	ù	[	¡	.	½	.	G	.	û
Þ	.	Ù	Ó	.	â	8	Ä	Ã	f	Ä	s	.		)	¼
#	i	ê	r	.	.	$	Ã	.	h	n	.	¸	.	*	.
.	~	ð	E	»	x	»	.	´	ñ	.		g	Ê	g	.
Ò	q	\	.	.	È	.	D	¡	.	v	Ù	Ö	p	A	M
Ê	Ý	.	.		.	.	.	j	¸		®	J	Í	Y	1
Ö	.	Ô	Á	D	³	Í	w	ù	²	Æ	c	¸	.	ï	6
ó	.	.	R	.	5	ð	.	é	0	/	I	Ð	¤	.	Ó
x	H	.	5	.	.	¾	Ç	w	.	.	¤	8	Î	P	î
¤	.	&		.	ë	ç	¹	.	Ë	½	{	¬	®	¯	K
.	2	7	ï	.	:	.	ñ		T	,	Ò	}	.	.	
¾	·	;	Ý		S	8	\	Ä	Ð		}	b	¸	J	ª
)	Í	µ	(	.	¤		.	¾	!	.	½			ò	 
4	.	D	0	g	T	'	o	!	:	ÿ	.			M	.
Î	î		.	Í	.	D	%	.	a	.	;	q	.	.	/
Þ	)	ß	²	¤	.	.	Õ	.	.	|	Ø	.	.	Ý	Ù
	v	.	.	5	;	d		ø		ó	w	[	ñ	ë	í
²	è	î	"	ã	V		K	Z		¶	&	.	.	é	g
ÿ	³	K		ì	'	-	f	ß	¬	²	.	Ï	.	W	¹
.	.	\	R	.	µ	.	.	}	ì	.	:	£	(	T	§
(	\	v	,	´	Ó	'	.	(	.	±	V	k	À	¼	
Ó	õ	.	Í	F	¨	2	y	ù	u		.	S	i	ý	þ
¤	¶	l	¬	Â	(	Å	ô		¡	t	©	.		c	j
J	ú	Ê	å	³	.	A		.	.	.	Ð	Ø	.	þ	.
S	.	´	,	Ê	.	«	à		~	Ò	.	.	s	Ñ	.
8	X	J	.	m	º	a		®	H	w	t	÷		.	ç
[	/		Ë	·	2	.	©	Ü	t	¨	f		Î	n	'
Ð	.	ß	.	.	S	I	A	ä	^	Q	Â	1	|	3	¦
.	Þ	Q	c	.	¾	.	Í	.	å	È		C	¸	`	.
À	´	Ú	£	.	}	.	=	R	o	î	y	q	;	c	¶
Â	L	.	¹	Ñ	.	Ï	é		Ö	O	Ô	â	8	ª	Ý
Ù	´	.		q	Ñ	ç	Ú	n	.	f	h		.	.	$
¾	.	.	@	d	Ç	ê	½		.	5	ó	µ	¬	.	.
	.	9	X	t	þ		Ø	.	2	Ð	H	.	`	.	.
	.	È		Ï	Ø	}	S	h	¨	Í	¼	u		`	.
C	.	ü	w	.	.	:	D	H	.	.	g	[	é	©	Ò
³	.		{	Í	»	ì	.	.	Ò	x	Ù	V	.	.	.
½	u	´	.	`	.	.	£	.	³	.	E	j	3	.	
*	h	¾	.	(	,	d	:	õ	.	ç	h	I		®	.
		æ	Ï	.	.	]	v	¶	(	?	A	§	I	ô	.
Õ	¿	4	.	.	Ù	g	µ	Ø	`	.	!	.	Ã	.	þ
a	´	î	+	¿	.	(	~	.	õ	Ä		F	Þ	Ä	{
º	§	.	§	»	.	!	[	.	þ	.	¾	.		!	B
þ	V		-	0	®	ä	:	â		u	º	.	¬	ú	?
t	{	.	Î	ü	a	.	ù	ú	!	.	.	-	¥	.	.
»	\	Õ	I	Q	Y	q	Ý	i	Û	ô	.	Á	s	 	D
ø	.	.	v		.	.	ø	Ë	e	.	è	!	§	i	ó
j	.	û	´	.	g	.	.	C		î	p	¨	.	.	§
»	m	ï	{	.	L	.	.	M	c	D	ó	v	ù	7	É
¸	ë	]	ò	O	Ö	ÿ	v		&	ù	4	½	ý	á	.
k	7	ð	N	¹	.	.	r	£	Õ	a	R	æ	V		á
	å	ô	z	ß	.	q	À	c	Ô	½	(	<	.	©	ý
ô	.	[	ï	Û	I	Ó	2	}		A	.	ï	7	n	ö
z	W	1	Ï	ì	©	.	,	º	.	ú		$	¦	Î	Ó
,	"	Ö	D	à	î	å	ý	.	.		b	¢	r	.	0
.		.	C	R	Ë	.	e	+	r	|	2	L	á	6	å
Æ	/	x	â	u	.	.	A	d	ä	p	Õ	.	.	.	ê
2	æ	±	.	.	J		ä	!	*		.		¥	÷	ú
Æ	ë	e	!	.	ä	Ø	f	'	.	.	.	.	.	¡	!
~	#	$		u	@	.	3	.	C	.	g		i	G	y
æ	Î	Þ	.	½	Ä	ã	}	ü	v	J	Ø	.	.	²	b
7	.	¶	>	@	Ü	P	ÿ	ß	ÿ		³	.	.	.	T
r	.	.	g	ÿ	.	Å	Ó	w	¡	.	ä	d	X	÷	
~	9	d	É	"	u	o	»	.	@		>	à	 	ý	º
ò	ð	G	D	.	t	.	.	~	ö		.		¥	.	.
r	Q	Ñ	ò	k	g	6	§	.	°	.	ý	¸	$	.	.
d	¼	ø	Î	.	.	N	.	.	l	\	o	.	.	Ä	K
		û	&	.	.		½	.	Ç	¸	<	.	.	ß	ü
ÿ	.	Ï	.	Ü	È	±	o	.	9	$	S	`		c	.
î	÷	V	à	û		a	£	A	.	ê	Í	D	í	1	.
.	¡	b	.	.	æ	K	.	}	N		.	.	¸	[	±
í	 	[	Q	ì		Ç	K	s	.	«	.		Z	Ñ	å
.	3	*	þ	_	R	.	3	.	²	.	*	x	.	c	.
.	ô	ª	û	¥	>	õ	Ë		.	{		>	{	.	j
.		/	à	F	.	Á	S	à	ñ	.	Ý	r	e	ê	.
¢	H	Ð	.	.		M	.	M	.		7	½	+	¨	Ö
V	.	£	.	.		à	N	£	m	£	ü	Ñ	6	)	ã
í	Ý	]	É	M	¦	!	T	.	~	ö	ü	.	G	W	.
ý	Æ	,	ë	9	Y	*	g	[	g	{	ö	Û	=	W	´
¼	.	å	z	.	z	F	é	t	v	1	ð	§	'	u	§
R	Ã	z	·	U	K	º	Ì	.	ú	ã	)	k	Y	.	.
.	<	.	.	J	«	.	.	ú	ª	M	n	.	¶	ê	.
I	Ð	.	}	.	Ó	&	+	.	w	K	·	e	ä	Õ	L
.	.	#	Ó	¶	Ä	.	Ð	.	¸	.	.	m	ä	y	¢
]	:	ò	Ü	d	.	+	"	.	½		q	¤	.	~	¨
¶	?	.	D	.	î	Å	.	À	n	*	È	L	.	¦	Ù
Z	ó	(	.	¨	å	Ù	ô	.	.		«	/	.	¬	Å
5	ë	j	]	.	g	7		.	.	.	T	.	.	G	l
j	°	§	¸	®	"	I	O		L	x	.	ö	¼	"	.
ã	§	|	=	.	¼	u	¨	)	+	a	}	d	I	Ï	Û
¤	;	§	{	Ý	V	.	¢	à	W	,	.	.		â	x
\	£	.	Ü	.	ü	.	C	¦	.	£	ú	1	®	D	.
6	ä	Z	í	á	K	.	ó	ô	O	l	·	ó	ß	·	_
³	i	z	.	[	T	ð	H	Á	#	Û	¨		Ä	.	é
.	ó	.	¨	3	.	í	,	.	Ó	Ñ	.	¢	.	ø	.
m	ó	õ	±	R	Ý	ç	s	.		.	.	?	¦	E	»
¨	z	@	.	P	§		°	U	.	[	ð	ü	C	%	.
3		W	¢	ß	F	.	.	Æ	.	.		.		\	þ
¢	ô	è		£	Â	K	!	í	.	`	T	¹	ö	/	.
V	{	G		.	è	¯	{	.	F	Ö	í	.	<	î	Ø
T	¸	ð	P	W	¼	.	-	t	3	.	*	0	]	¯	.
Õ	°	.	^	!	I	ò	^	o	R	¼	ú	i		.	À
2	i	$	V	.	µ	ì	ö	.	ú	{	î	û	0	Ó	?
{	 	#	B	¯	.	·	ÿ	ã	=	¾	Ó	Í	.	&	.
ò	¨	.	.	½		È	S	©	k	.	9	.	(		.
à	#	?	~	Í	.	¢	»	.	.	a	.	j		U	?
ò	Û	á	«	k	Ë	Ú	 	~	Ä	©	.	`	.	¤	
ñ	.	d	Ø	h	%	ë	¦	<	È	L	Ä	Ü	]	Î	¬
.	.	t	?	ò	³	Å	ß	.	²	Ç	L	¢	S	.	ª
â		ò	½	ú	|	ò	Ò	.	»	.	À	Y	Ü		.
à	¿	-	P	Ô	I	h	Ý	ÿ	.	/	>	*	.		¢
ý	Q	t	.	Ý	Á	ç	î	=	Q	@	I	r	.	½	P
.	a	J	B	Ó	e	.	4	M	4	.	×	e	Ó	<	"
e	¸	"	ö	/		à	.	V	.	¢	µ	ó	ø	{	'
ï	R	(	Å	H		<	ê	.	?	X	¥	Ä	.	l	m
6	/		/	m	.	»	Æ	¯	Ê	þ	f	U	@	~	a
Ø	¹		Á	4	Æ	¿	¨	V	y	.	#	t	ø	?	¿
þ	+	Ç	ß	L	.	º	.	z	Z	m	.	.	[		¼
}	!	M	(		.	.	\	ï	.	Ç	u	.	0	V	6
¨	.	.	B	.	÷	z	%	ã	H	l	Ý	.		-	O
Ã	x	C	Ú	ý	¶		ë		.	Ã	)	H	.	`	)
y	×	í	â	;	Q	«	1	»	è	|		ñ	k	e	·
(	.	S	»		è	L	Î	q	ã	U	Ý	þ	n		.
¬	.	k	À	:	V	â	g	7	®	Å	P	.	S	¨	â
Û	Æ	À	&	æ	®	Û	.	³	.	!	×	|	!	Ê	i
.	ê	7	´	.	Ë	)	Z	ò	F	{	t	Ó		«	O
!	.		î	µ	ø	.	¶	Â	 	5	M	}	¹	.	f
'	p	0		°	)	3	«	À	Ü	;	ÿ	i	J	]	A
]	s	.	.	H	.	H	T	¹	R	à	¥	Ð	.	9	ó
§	.	î	U	.	.	·	Ù	.	Ú	æ	4	D	z	G	.
=	®	.	Q	à	Â	0	?	M	Õ	Ô	¼	Y	1	Ê	¨
n	B	B	t	V	.		L	.	1	d	,	.	i	Á	¡
T	ú	ô	y	8	 	.	Å	º	Í	.	¯	[	.	P	.
8	&	.	.	é	.	{	¿	ú	¹		0	ó	³	É	.
w	U	¼	C	.	$	Í	\	.	Æ	ë	.	O	w	à	.
ê	/	ä	Ã	.	²	P	V		/	Ä	l	é	K	*	#
%	?	.	ó	?	Î	V	þ	Î		C	þ	.	Ï	=	Ã
¹	M	8	{	û	ü	Ì	.	.	Û		ë	-	³	Ä	.
.	.	$	=	.	é	Ù	Z	.	Ú	Õ	Z	Å	.	.	
f	.	.	.	>	.	.	³	.	`		å	÷	.	.	Ä
i	ù	ò	.	æ	C	É	.	.	¥	}	5		¡	ù	¡
ý	.	i	Ý	d	J	:	§	)		Ø	Ï	'	ç	.	Ú
r	r		ÿ	.	.	×	è	¨	+	y	.		a	j	&
ý		.	§	/	Á	T	¹	¥	J	6	.	i	á	à	L
$	.	ò	J	Ç	.	Ù	.	¹	m	ü	.	Î	.	ù	Ê
.	p	w	A	.	.	¤	.	£	ï	"		å	ú	T	.
æ	.	|	z	.	Á	5	Ö	y	.	ñ	@	z	û	å	Ô
.	i	.	c	.	.	æ	.	a	#	,	|	B		Ü	.
ñ	.	ñ	u	m	.	1	D	à	ò	.	ü	V	u	)	3
.		ô	F	Ì	.		h	@	D	.	.	£	!	®	à
.	¶	ï	.	.	W	ö	.	Õ		B	¯	÷	Ó	È	f
k	.		.	å	)	H	Ø	.	.	ö	õ	ë	Ç	.	
	$	®	e	í	õ	Ú	Ù	{	o	G	ð	Æ	)	â	.
t	ø	õ	¥	o	.	.	M	M	`	.	P	d	5	.	.
î	.	.	¼	.	ã	¦	.	.	.	¬		@	?	.	)
Å	¸	7	Þ	G		_	N	ò	.	°	Ý	|	.	×	¢
º	]	Ë	"	¥	x		Ã	.		¡	.	g	3	Ï	
.	*	l	w	m	h	.	ø	?	.	m	#	.	s	3	.
é	·	.	Î	.	Ä	Ü	ó	Õ	W	.	:	_	y	.	.
H	.	G	¾	ú	â	.	À	Ì	ò	B	¦	&	V		S
H	"	}	j	Z	Ý	´	a	¾	®	g	é	ú	`	.	»
	N	Z	8	«	¸		(	Ý	<	¤	.	Ì	.	1	_
A	j	Þ	.	â	u	à	£	.	K	Ø	.	l	|	_	
×		T	å	t	×	H	y	ë		.		.	.	;	.
k	/	é		ö	`	ç	¢	.	w	B	à	i	k	§	ß
1		E	<	Q	d	A	%	)	.		.	Ò	.	Ù	H
B	¥	*	c	/	.	.	.	g	j	v	m	Q	.	!	§
¼	.	§	³		.	A	4	.	|	¥	~	h	Z	.	î
	c	Ñ	.	%	(	ª	.	-	Ù	r	ò	.		¼	®
.	°	£	Y	þ	¼	Æ	 	.	"	½	®	ç	B	h	.
Ø	y	x	.	P	T	.	¾	È	·	n	R	.	£	÷	
.	Â	ý	p	å	7	v	È	F	Ø	X	.		)	^	
e	=	î	`	.	¼	.	.	X	Ã	.		ä	ª	®	ö
Ò	û	[	Ê	.	¥	±	$	3		È	¾	.	V	³	.
±	u	®	!	°	Ð	'	È	£	.	^	¦	.	ù	©	Ä
.	ñ	Â	ö	Q	O	¼	©	.	»	°	®	w	Ø	ß	ð
.	i		.	µ	x	Ò		ï	F	Á	é	.	¨	.	I
.	x	ª	I	±	P	a		¸	.	.	]		.	O	+
.	.	9	¿	.	i		.	õ	.	k	®	.	=	L	.
E	}	á	*	ä	.	\	A	.	.	Â	Ä	T	ê	 	Ô
h	R	µ	´	¿	s	Â	.	ï	À	.	à	ó	.	Î	.
q	ë	á	T		¦	ò	.		è	Ä	b		«	¢	}
Ü	?	k	.	~	i	¹	&	Æ	Ç	.	þ	$		Ü	.
.	r	.	ü	ö	H	É	Q	.	ª	È	.	Ä	!	c	(
*	Ç	.	¦	%	ê	.	g	»	.	¡	o	é	Ô	¸	û
3	a	Å	`	H	.	´	Û		.	î	q		á		¦
F	I	â	«	ã	9	Õ	.	.	Ú	.	3	õ	-	L	.
Å	^	S	.	û	.	.	m	.	.	.	9	Ä	¦	¡	x
ï	i	Û	ÿ	=	.	.	.	Î	.	.	[	Ú	,	³	5
ë	Ö	p	_	.	%	â	²	õ	Ä	b	ÿ	B	=	ú	F
.	ü	§	º	e	K	¸	+	.	È	H	.	÷	<	c	.
.	j		å	ß	v	½	¶	ë	µ	§	b	û	À	Â	.
.	k	5	J	Ê	A	.		j	I	Ü	È	q	e	.	.
Ü	g	.	a	.	.	.	=	.	.	É	æ	.	z	Ý	.
×	.	á	£	Ó	Z	O	ò	0	D	.	.	.	¾	.	ö
ñ	h	X	3	D	'	í	.	A	.	þ	£	Þ	Q	×	e
Ü	.	k	k	Ë	y	è	.	Ú	ô	å	.	.	{	e	é
2	.		T	´	¤	k	>		D	U	.	Í	.	.	.
Ð	.	a	è	É	±	.	.		@	.	à	¼	)	r	î
O	.	ª	¨	.	~	Ï	.	ý	©	/	.	.	v	¸	½
.	.	Ð	¸	ò	á	Ó	E	.	.	l	æ	.	Õ	.	|
.	¼	m	.	¹	H	@	f	i	.		{	J	M	ë	~
ÿ	.	Þ	¼	.	ý	.		´	.	.	x	Z	ú	ó	.
õ	h	O	_	ý		A	F	Ã	R	N	v	.	ü	p	i
X	 		è	È	Â	.	¥	R	.	È	è	.	-		¹
.	.	Ø	´	.	V	H	.	§	g	Ü	.	w	Î	G	ê
.	s	w	Ö	µ	.	,	{	%	0	[	Ú	?	¸		.
¼	¿	é	D	1	ä	Á	¨	Å	Q	>	H	©	z	Ø	.
á	.	ì	y	Ì	.		u	c	Ý	.	.		.	ù	.
.	ã	y	.	Ú	»	x	]	.	´		.	P	.	.	.
÷	þ	±	.	û	6	®		,	.	y	µ	¨	j	h	b
C	ó	'	D	à	/	.	W	£	%		B	è	2	.	.
à	P	³	.	¼	!	.	"	i		.		(	Ç	h	i
[	.	¨	.	_	»	L		6	+	.		Ò	^	#	Ì
D	Ù	.	í			u	ñ	.	á	Þ	}	¡		Û	Ã
"	t	)		s	Û	ü	.	.	å	7	ì	B	.	!	Á
À	ã	.	9	P	Ø	.	S		¼	'	Ê	²	.	.	
C	?	,	¯	K	¤	É	ª		Ð	Î	.	ý	g	.	°
.	y	4	8	.	Å	_	À	y	.	©	.	ª	Ì	Z	Ä
.	{	s	D	U	¹	X	x	·	,	.	ï	.		ö	þ
.	Ý	.	ú	!	³	¤	.	.	"	O	-	5	F	.	ì
.	Ä	ý	.	1	.	ú	å	r	Õ	}	.		»	.	D
î	-	ö	s	.	Ü	ù		8	B	P	.	o	ò	m	.
Ï	D	î	º	G	î	C	î	²	Å	.		×	 	.	.
.	X	0	e	§	þ	&	.	È	ã	¢	 	.	ø	W	ú
.	ô	ï	K	¦	L	Å	Ï	ò	0	Z	¾	´	.	Q	¤
v	Ô	.	.	û	.	#	.	]		ü	.	.	%	º	W
.	.	¿	Ã	@	â	(	¢	M	=	È	.	.	b	a	ä
Ä	.		g	Ì	Æ	ù	ø	^	8	y	P	g	t	@	´
Ó	L	¯	.	x	X	u	ù	c	7	.	Ï	ô	¬	Z	¢
Ø	.	p	|	£	.	±	)	û	.	C	ü	+	d	Å	,
û		.	Å	®	ö	]	Þ	R	.	m	å	S	r	"	;
+	.	v	¡	j	Å	A	.	ê	Õ	Ù	{	×	8	.	F
.	.	ÿ	.	ú	Ù	®	.		w	.		õ	j	*	I
&	ý	ò	F	z	7	.	.	ð		+	h	£	.	z	.
Ú	9	*	ë	E	-	u	Ý	.	h	M	.	%	^		Û
Å	!	Ð	<	.	.	þ	»	²	à	e	.	.	ý	f	ø
ô	"	D	.	®	Ù	(	Ü		¡	4	;	>	.	.	.
Å	Ó	<	ì		·	â	"	@	 	a	?	ù	.	°	ï
à	.	)	µ	¥	Ã	.	@	z	À		{	ß	.		.
N	e	*	h	L	¦	[	Ø	à	_	M	Z	<	.	.	ô
î	Ç	ß	!	µ	.	¢	.	ð	.	Ï	h	Ë	Ç	*	ú
.	í	X	Ô	ð	n		ã	.	«		.	x	ª	J	3
±	.	b	³	k	.	µ	.		¥	Ó	ÿ		¼	e	.
.	.	â	Î	>	§	e	.	À	³	K	.	ÿ	ç		6
Å	¥	{	ä	H	Ô	1	"	z	.	É	¦	A	v	.	.
.	X	.	¨	Ò	Å	o	C	.	P	ä	à	R	©	;	À
p	@	Ô	Í	.	.		.	Ú	Ô	ä	,	}	è	.	£
H	/	5	'	.	¿	ª	ç	ä	X	e	¥	(	a	M	8
9	.	Ç		Ø	.	U	Ø	>	.	.	Ò	.	M	ì	ñ
|	.	§	[	Ì	N	ã	À	²	°	.	.	,		^	¤
	½	Y		.	.	æ	/	¦	´		=	Ö	/	u	
.	L	.	Î	â	.	Q	9		[	3	.	þ	Ì	M	.
.	4	!	~		õ	Â	W	.	.	.	.		Î	Á	S
5	£			.	Ø	¡	.	Ï	¾	¨	.	·	÷		.
%	ê		Í	Ñ	j	Ô	.	~	.	Ø		.	.	Ü	.
æ	Ò	P	.	Ù	Þ	|	L	Ú	õ	q	j	Í	Ò	i	.
®		Õ	.		'	~	:		ó	ú	â	Á		:	ö
.	Ã	e	.	m	l	.		-	.	.	n	ô	Õ	i	Ò
.	ð	~	â	Q	q	é	/	c	ì	¡	 	.	2	c	K
-	.	S		®	Ý	5	Q	8	£		:	¿		.	à
N	¤	x	2	.	ç	"		»	.	^	a	.	½	Ó	Ê
C		ß	.	T	.	.	B	.	_	H	Ê	E	@	.	ý
.	j	â	¡	Ð	.	ö	1	ë	¼	.	ô	G	Ù	x	¾
Â		i	)	y	}		!	.	È	.	¹	¥	.	@	.
°	©	¯	ï	Û	[	.	Î	û	W	¦	þ	Í	]	.	.
ø	.	â	Ñ	Ë	N	.	)	³		Ö	w	Ô	ï	Ü	1
¼	:	.	ç	C	È	v	Ú	\	!		.	.	Ö	=	q
/	¸	G	-	.	¨	L	.	ü	s	/	.	é	_	Ñ	k
Æ	ú	ã	d	I	Ô	Ê	i	.	}	.	.	{	¢	Ï	.
Ú	Æ	.	d	d	j	.	º	.	è	È	Ø	º	Ê	Æ	Æ
v	º	.	?	.	F	V	H	×	W	.	#	7	z	.	
.	.	ì	Û	ú	.	2	©	ð	.	Þ	¤	°	.	.	%
@	.	R	L	K	î	.	±	.	.	.	Ô	J	c	ü	N
Ú	y	.	=	q	R	À	©	È	Ç	.	@	Ø	.	¬	Û
2	.	2	e	g	.	ð	æ	ç		8	.	ã	V	õ	;
%			(	.	l	@	J	[	5	e	ÿ	K	^	.	.
.	.	ö	Ñ	E	^	«	»	E	°		µ	.	¾		.
=	'	4	]	³	M	O	·	Â	W	õ	í	,	Â	E	
Q	Å	$	D	ª	Z	c	Ã	ì	M	.	V	E	.	.	Ó
î	#	V	.	S	W	¶	.	Ó	{	?	t	+	a	Q	@
)	.	l	Í	é	U	ï	Ô	Í	õ	.	.	à	S	²	.
d	0	ð	©		¢	{	h	`	.	È	Ø	n	u	¾	¡
Î	-	.		ù	î	÷	¯	.	Ã	í	n	²		¿	U
M	»	¥	Ã	s	.	w	¥	X	T	.	â	m	.	(	`
.	.	@	?	\	j	A	.	Ö	Y	Ä		n	°	4	.
è	÷	a	.	¿	_	(	B	î	.	é	.	q	ú	Ï	
J	#	W	s	Í	ë	.	P	c		.	.	~	.	.	T
I	k	.	×	h	.	~	y	.	[	:		k	¶	¯	í
H	`	s	@	A	+	¢	.	8		«	.	©	.	[	g
.	¬	.	Ä	G	«	.	R	ÿ	¾	.	Ï	O	}	N	.
9	Ñ	]	õ	#		.	þ	á	Å	,	5	.	ý	ú	í
.	.	.	`	^	2	5	«	C	÷	.	ó	.	W	.	Q
[	R	Ó	y	.	ÿ	3	t	:	s	Ø	«	Y	ÿ	t	
_	T	ý	¬	Ç	.	ý	Ô	.	.	.	ã	ï		@	Û
Q	.	Í	¡	§	²	È	õ	.	:	¾	º	ï	.	£	Ö
8	®	.	S	`	Q	ô	Å	Ü	k	e	¯	Q	¿	m	Î
C	þ	.		d	©	.	/	¿	¥	¯	ð	Ê	.	,	Ô
r	Q	.	F	.	÷	E	Á	.	P	+	q	p	´	+	¦
f	v		Ï	,	Õ	÷	A	h	Ó	â	.	.	¿	Ô	]
^	l	¡	m		.	¸	.	O	.	¦	.	r	¦	T	Ô
½	F	$	.	W	Ï	Ö	.	{	ª	t	Ý	.	Ó	ó	O
	B	¢	\	Î		.		Ê	ö	«	P	2		6	.
3	ï	Ê	G	{		¡	.	´	c	Z	Á	v	+	Á	É
¾	ï	R	q	p	a	9	©	ç	.	ý	ö	h	F		
µ	æ	z	r	.	.	L	%	9	.	.	.	j	l	R	.
6	q	µ	$	°	Ü	j	h	ù	R	.	ú	.	°	.	j
ð	¼	.	p	9	p	.	ó	Â	0	µ	~	R	x	H	.
$	Õ		£	Ü		.	î		ª	N	"	y	à	.	&
±	§	.	û	.	b	.	±	ñ	^	 	~	.	h	Ù	Z
.	.	.	_	.	a	"	\	(	6	t	Ú	È	x	b	P
!	õ	c	¨	Ä	K		.	]	.	%	.	N	.		.
4	¦	\	.	§	í	$	×	o	¹	<	.	À	Î	Ð	¿
:	;	-	6	à	Å	q	.	ä		u	¶		(	Õ	ø
O	E	ñ	.	%	.	¬	å	Ã	.	}	.	J	d	Ø	.
	o	x	l	»	¶	U	t	¬	<	#	è	Ì	ù	.	k
x	¥	.	.	O	.	.		þ	I	.	%	.	¦	D	.
w	\	h	t	ó	.	%	¿	o	k		$	Y	G	Ñ	ó
.	´	è	.	Z	0	é	Ì	Y	.	÷	¹	Ó	3	¦	+
.	Ý	Ô	í	±	Î	ï		?	.	z	ß	è	.	b	D
.	ë	ß	ÿ	.	m	.	F	Ö	ª	3	¾	.	*	Ý	=
.	ð	¦	(	µ	D	I	3	.	.	.	§	R	:	H	.
.	÷	b	Ï	.	Ò	Æ	.	8	»	(	ó	ç	.	?	»
R	5	C	0	.	G	â	k	Ø	4	í	J	¸	.	Ø	p
Þ	.	V	»	7	µ	.		h	k	D	,	.	.	Ð	í
S	t	 	.	¬	.	¼	ã	Þ	j	æ	½	ø	Ü	Æ	¨
·	.		ß			ç	Ù	ä	I	.	ª	.	§	Æ	ä
u	{	Û	Ó	H	l	²	l	¬	5	I	t	o	T	.	è
ü	©	.	Ó	<	l	l	.	¦	q	w	z	P	.	E	&
ü	I	.		.	v	¸	Ü	Æ	w	6	ë	Ü	.	þ	c
[	?	9	.	¡	£	.	þ	`	.	.	&	J	Å	Û	D
.	½	.	ø	¦	±	.		Ü	.	(	Z	ó	.	.	N
Á	Ä	.		þ	T	é	r	8	Þ	.		ø	a	,	i
?	ø	1	.	.	.	p	ñ	Y	z		;	~	Þ	ã	.
¬	.	ë	l	ë	°	è	õ	»	Ô	!	.	.	i	´	.
.	÷	.	F	T	þ	)	`	.	¢	Î	ö	w	Ý	.	Ò
c	·	L		.	Ó	.	4	.	}	/	¯	f	è	.	j
|	ø	.	±		Ð	ß	E	?	.	.	±	L		ø	Ú
	¡	t	.	Q	ð	+	e	È	Ô	e	V	Ò	.	d	Ö
²	È	¼	.	È	S	í	.		X	.	Y	Ý	Ä	P	.
.	×		C	Í	ú	ï	º	z	.	3	*	f	°	Ó	 
'	w	P	¶	µ	G	Ó	.	û	K	%	Ú	.		÷	T
á	â	Ð	.	z	ì	.	.		¼	ð		1	/	`	n
ß	~	¬	E	@	å	À	Ë	à	.	.	ö	)	w	¢	±
{	©	ð	]	.	T	=	U	î	¬	z	å	'	¨	.	
ö	u	.	ü	Æ	y		.	S	¤	w	.	^	ã		
0	è	æ	x	.	k	Ú	.	ü	'	Q	+	m	ü	¦	(
R	*	¶	 	.	W	.	.	f	\	a			Ú	±	à
Ä	Z		Ç	.	Ë	.	è	©	|	?	.	.	ê	.	.
Â	9	ö	ï	m	+	Ç	y	¦	8		1	R	J	Â	v
.	.	.	/	Å	¸	V		.	A	ø	¨	b	Ç	¦	|
.	l	.	8	?	Å	ã	.	_	â	G		ß	ë	v	þ
v	¿	Ö	.	.	9	H	`	.	Æ	´	d	.	¸	.	Ö
.	'	»	¾	@	.	è	A	Ù	.	(	.	.		F	^
«	^	.	R	À	.	5	ß	×	%	 	³	u	2	C	i
.	·	Y	h	Ù	ø	.	Ì	*	©	Â	.	r	.	Ä	í
^	·	m	;	©	å	ã	%	.	õ	©	ë	Ô	.	ö	4
È	R	Ó	a	.	`	H		.	á	à	s	ÿ	Ì	.	â
.	å	0	3	d	Þ	Ù	°	Ü	³	¨	µ	¯	Ç	Ä	M
R	.	=	Î	H	[	5	.	L	©	µ	ì	i	.	ç	Q
©	.	.	w	3	Ã	.	B	.		Á	x	È	Ú	Ä	K
.	.	ß	g	.		Ï	q	è		±	I	.	h	.	I
	»	3	Â	.	®	Ó	e	Á	{	k	.		Ç	.	«
r	{		õ	¡	-	§	/	Û	.	Æ	Ã	X	.	S	.
û	°	_	p	.	æ	Þ	.	Ä	ª		.	ð	.	Æ	í
a	q	Û	.	s	.	!	(	.	m	.	õ	ª	5	.	ö
w	ú	i	g	¥	ò	.	.	<	Õ	q		.	¸	2	Æ
/	¸	.	ë	¹	>		.	.	6	¸	.	/	.	.	Ý
V	~	3	L	Ì	Ñ	`	Î	ý	Ç	ê	:	.	»	¯	;
g	=	.		M	£	f	)	d	-		Q	.	(	.	÷
Î	-	.	ü	¹	æ	Q	?	(	¤	¦	^	.	[	I	%
³	?	o		e	x	å	U	.	a	&		g	À	M	{
.	.		"	h	{	.	[	+	.	¼	.	ð	.		á
.	.	´	Ê	"	¦	¡	6	r	â	Ú	.	Ò	6	.	0
&	Õ	.	w	~	z	¥	x		þ	.	Û	ó		ò	N
 	¯	6	µ	ë	À	A	T	¶	.	Ù	Ò	D	»	Í	m
2	f	ã	÷	.	7	$	.	=	ß	Ó	æ	.	d	à	¹
.	©	^	Ê	.	Þ		ô	.	.	.	¦		¹	.	´
l	.	S	0	÷	ë	Ê	å		-	K	ö	o	t	.	[
K	.	.	Ó	À	.	.	.	Ó	.	.	.	.	.	±	.
.	a	þ	?	Í	:	É	S	W	ï	Ú	À	Â	O	§	0
¾	å	ì	c	R	Ý	ñ	A	ë	j	(	f	ç	®	©	ã
	N	ï	=	+	Q	¹	[	õ	t	.	É		{	×	U
.	.	û	!	.	k	o	[	.	=	.	P	$	Æ	¼	>
.	.	½	;	p	5	.	*	w		.	h	}	e	.	ì
ë	:	.	)	Þ	ã	!	¤	{	.	Æ	È	Ý	c	.	°
I	d	L	S	Ó	.	N	(	Ô	p	[	.	Z	n	¿	H
-	.	.	n	.	A	P	.	|	v	á		©	"	Z	'
.	ê	-	¾	.	¾	w	.	÷	É	ê	.	w	ª	O	.
g	.	ñ	°	c	.	ñ	.		.	¨	.	Ð	.	Æ	.
.	.	.	.	ÿ	k	.	.	Ø			Ô	Ã	L	ª	.
j	Û	.	ö		8	ø	¥	º	5	²	%	.	.	k	Ú
D	.	à	µ		¬	.	.	|	8	=	C	m	.	Â	¢
d	+	?	å	.	.	ß	.	°	x	o	F	.	.	é	p
	#	.	ï	Æ		E	ð	.	k	Ü	]	²		Ð	Å
.	<	2		&	[	.	ë	w	c	.	.	C	p	_	S
Å	.	´	j	Ä	.	g	<	$	.		á	ê	.	Ø	.
P	Z	J	¹	Ë	¨	.	Ð	.	Å	á	¹	¡	Ã	w	^
´	.	ð	ù	%	^	.	¯	¢	|	¥	O	É	ã	®	r
Ó	¾	º	.	u	Å	ë	-	)	W	ì	í	~	×	j	3
º	.	é	Ã	¢	.	ô	æ	.	ç	ð	y	.	.	«	.
5	H	?	.	.	Õ	÷	8	Û	.	.	Ñ	.	D	"	ç
9	0	=	w	b	%	°	=	ø	ø	.	t	?	y	#	Ê
©	¥	.	_	.	;	º	.		S	*	R		m	è	º
¶	G	7	Ü	R	.	ñ	.	.	7	ü	0	Q	Ë	¤	ÿ
é	Õ	Ö	.	.	.	Ü	 	Ô	1	.	D	;	é	Æ	î
¶	.	)	¹	¹	N	#	b	9	`	Ö	P	H	°	ù	z
y	.		t	I	ê	Ì	m	z	q	z	1	t	(	¶	Ó
x	!	.	S	8	b	O	.		¸	.	z	ñ	Ð	p	,
.	Æ	µ	N	.	ö	F	.	7	¨	S	û	H	;		x
©	ä	è	.	¤	Þ	i	.	ß		M	µ	E	^	L	Ý
Ä	.	.	5	.	¢	7	å	Å	¡	¹	ã	D	.	ö	.
.	å	Ê	y	Ü	5	é	.	.	Ç	.		k	.	ÿ	ê
N	¨	£	´	ë	ß	Ï	3	.	©	.	æ	Û		«	\
ï	 	.	î	j	ñ	.	.	.	.	»	ô	.	.	è	,
º	?	y		Á	Î	.	ß	ý	.	y	]	.	v	C	5
V	Ý	q	.	]	Î	¥	.	h	â	ì	@	 	.	.	
_	.	Ü	´	B	Ë	6	.	ÿ	.	·	.	5	Ï	Ë	¾
§	ã	f	¥	ó	+	n	p	t	.	Å	ë	.	;	¾	'
L	&	v	À	|	Ù	û		p	Ù	.	f	.	F	s	è
Â	"	Ò	æ	.	.	C	ú	Y	8	.	û	?	.	W	Ü
Õ	²	À	(	Ï	¢	.	A	¶	ª	ö	.	L		ì	.
w	f	Q	V	ô	.	ã	-	[	¤	§	½	Õ		#	-
±	å	.	n	.	«	m	Ü		µ	u	.	{	Â	ã	A
W		f	 	h	Ù	m	.	^	N	Ê		.		.	£
.	E	#	f	ä	h	9	¶	(	C	ç	9	}	)	c	2
.	Ò	Q	7	 	Ë	R	.	Y	.	.	X		k	4	d
W	ó	¡	.	©	 	v	P	c	ã		¹	b	^	·	Ã
M	.	(	¸	ñ	>	.	.		Ñ	¾	Ö	d	³	Y	;
L	þ	{	Ã	ï	U	p	-	§	$	Z	Ý	.	Ã	.	Î
È	ç	¸	.	w	G		z	Ö	n	ú	=	B	Ì	¾	
È	i	Á	»	°	.	ç	.	ó	ä	¥	.	×	-	.	.
t	.	Ï	æ	á	n	¤	>	.	.	.	.	.	Q	¥	¢
.	Ó	ñ		Ã	î	f	_	&	.	.	.	Ã	Ö	.	h
	f	Ñ	/	ª	Î	9	%	.	)		&	®	é	ð	.
0	 	=	|	²	à	¦	K	1	ÿ	Y	Û	J	N	q	.
ó	í	Ü		â	8	.	O	í	»	?	c	¾	è		ø
!	d	·	¿	®	Ë	.	æ	¡	Ò	Y	å	A	.	|	V
ò	ì	;		.	g	ä	E	Ý	¨	í	.	á	Õ	Æ	=
.	>	G	.	5	}	ê	L	Ú	.	ß	x	z	Ò	ª	.
t	.	{	a	c	*	Ï	L		.	¦	ß	ã	.	¯	.
@	.	v	;	Ì	.	Ù		q	]	.	e	.	.	£	æ
ø	9		.	Ã	û	.	;	.	á	.	.	h	*	.	W
ß	.	B	y	.	·	ä	}	ê	.	ã	.	Ï	.	Ì	 
¬	º	 	.	í	Í		*		T	Å	!	±	1	^	Ý
c	.	h	.	.	C	@	.	Á	é	Û	.	Ù	.	«	¼
.		.	q	è	o	Ä	¹	ß	2	n	f	t	]	4	r
.	Ñ	.	Û	.		c		9	Ý		®	.	.		.
.	.	Â		S	Â	]	Ú	ç	±	s	ú	Ê	¼	.	Î
±	.	a		4	n		,	i	x	.	á	æ	g	Þ	.
ÿ	ò	|	g	ì	a	¥	Ô	ç	Ü	d	§	#	#	.	¸
¾		{	Ã	x	º	á	ñ	Ö	»	ò	§	.	.	ò	ì
8	ê	$	.	\	8	.	j	±	Ö	£	%	ï	7	Þ	$
.	õ	]	}	c	9	ÿ	|	4	p	÷	C	Q	6	Ü	¾
¾	r		.	{	·	H	©	Ì	7	+	.	t	ê	þ	.
,	.	.	ü	.		Ó	À	Î	2	ï	.	Ü	H	p	`
(	D	ð	<	ò	.	ý	m	¿	:	ô	.	o	d	1	¬
?	j		f	5	½	.	.	«	ñ	z	ç	4	/	(	Á
.	Q	É	A	q	ç	¾	Å	K	.	.	¯	.	ü	¬	.
.	w	X	¾	S		9	z	a	Ü	¶	û	.	.	A	W
.	.	.	ç	S		1	O	.	7	è	¥	Õ	.	C	Ñ
®	.	.		ë	Ì	#	þ	.	.	¦	.	~	À	(	.
Y	»	ñ	¨	n	_	â	L	?	.		«		>	ë	
$	.	Ó	#	Ó	¢	O	_	.	³	 	ú	.	f	R	.
E	.	U	.	*	%	P	.	s	.	ã	~	X	Æ	]	.
1	.	Ü	.	|	Å	k	Ö	 	.	U	«		.	n	.
b	P	k	*	l	ì	¶	È	.	>	Õ		ö	²	{	î
Z		~	n	V	/	O	.	H	.	¤	.	H	ª	î	
b	ü	.	*	ø	.	.	À	ä	.	H	.		R	Z	}
p	H	«	,		L	.	9	±		o	l	2		+	ê
Á	Á	6	e	u	B	£		ä	.	F	ä	ó	*	Â	â
Ã	¶	e	.		}	°	j	I	Ý	¶	ð	0	>	.	Í
¹	g	Ù	.	Ò	ï	ù	½	Ë	t	;	-	¹	,	g	?
	e		ñ	.	.	,	®	2	ê	Î	¿	n	À	±	y
ø	¦	J	-	´	.	p	¨	l	»	.	o	#	k	.	¨
¸	.	.	Â	§	K	S	R	¨	Á	p	.	È	]	ã	ð
	²	L	.	(	.	.	ô	»	.		þ	ó	Ú	W	Ü
«	¨	h	s	T	å	â	¯	.	Ý	E	Æ	m	[	.	.
À	Õ	æ	.	.	Õ	q	.	Î		R	Ç	á	»	Ú	½
~	¨		à		f	=	Ø	_	¿	Z	.	Ä		k	.
Ë	|	5	×	e	¬	5	5	.	.	E	Ü	.	p	/	.
ç	¥	.	.	º	ô	l	d	.	û	.	k	+	2	Ä	Ç
.	y	B	;	.	l	d	º	.	£	×	.	.	P	ì	!
¶	.	À	ë	.	´	.	.	5	ù	í	ñ	Æ	ï	k	ú
x	N	?	½	B		D	.	ô	¿	f	.	.		¿	.
	I	Ð	V	Æ		n		.	F	.	.	i	ô	Å	¶
+	.	À		Æ	5	;	,	¥	C	¢	¬	.	¶	é	µ
%	.	"	s	Ï	.	Q	ò	Å	¡	I	¶	.	4	³	ÿ
.	Ü	4		³	:	.	U	.	.	m	.	ï	H	.	7
^		õ	E	¥	&	ç	$	Æ	e	{	.	D		o	U
	E	|	L	.	É	þ	é	ç	8	.	ª	?	A	.	o
¶	"		Ø	Ì	r	Ð	¥	÷	ê	¢	9	w	Þ	M	ñ
a	D	%	r		è	t	ó	?	Ç	.	.	æ	4	9	»
-	å	_	.	h	«	ò	>	.	.	.	~	.	Ó	f	#
Þ	.	=	D	M	d	«	S	?	à	ä	l	¹	U	)	½
¨	.	©	k	Ü	î	S	z	.	Ó	.	Ã	$	.	.	.
¨	.	¹	"	¯	.	h	Á	P		.	I		ç	0	0
9	0	.	.	ü	 	8	n	Z	|	ç	M	!	ã	=	j
¦	»	n	p	¶	.	e	®	*	ã	.	.	,	¼	³	Þ
.	/	.	'	Ð	t	«	Ç	ï	ö	·	¯	¯	8	.	¶
ò	»	Ù	þ	ê		I		É	6		³	d	L	Ã	C
ü	W	£	Þ	¶	ø	È	>	.		°	.	d	.	p	.
	.	À	ô	·	ü	b	b	W	·	!	£	m	Í	×	.
	%	<	z	.	.	.	¿	ú	ã	¾	ó	ó	$	ã	.
K	X	Ò	Ù	£	B	¢	¤	Ó	A	.	å	u	.	^	É
y	J	Ò	Í	ì	-	Õ	8	.	#	¼			{	¥	î
.	.	b		@	¦	Õ	U		W	u	j	ó	.	Ó	¾
Y	¸	í	¯		5	Õ	L	.	.	.	"	W		H	N
°	.	¥	»	T	Z	.	¿	S	.	¹	ê	j	.	þ	ñ
î		S	V	Ú	ø	Û	.	)	»	>	Û	Ý	:	À	I
û	¨	%	,	.		O	þ	~	3	b	>	.	g	P	o
.	.		®	Ô	w			§	Ö	.	I	K	6	h	Ç
Ï	&	®	*	¬	Ñ	÷	.	f	]	.	A	w	û	l	ñ
'	ü		M	.	Ç	.	Â	ó	,	A	-	A	Ð	Þ	â
´	.	.	ü	è	2	¡	í	0	Ì	!		i	)	.	Z
.	y	³	¾	P	©		à	â	)	ì	ö	.	.	c	Ø
}	¨	p	.	è		ú		Ñ	£	6	B	.	Ú	.	ÿ
ß	¼	ô	Ü	.	Õ	v	Ñ	s	-	r	B	Ç	o	b	o
³	Û	.	+	?	x	.	ð	Ì	w	´	à	á	ä	.	§
f	T	Q	2	%	Z	.	Q	·	¦	.	è	ë	M	Ü	.
è	3		É	¼	G	Õ	O	ü	û	.	æ	V	W	Q	.
Ê	¬	Ð	¾	=	Â	R	»	'	k	ý	Ô		.	Æ	.
è	¸	¢	/	ã	?	 	¦		%	,	 	µ	y	j	.
.	®	2	Î	.	L	Ë	&	;	<	.	5	.	¶	)	Z
p	.	À	!	9	Ú	6	ê	.	´	æ	\	ð	Ì	g	·
+	ò	.	¿	·	 	.	.	s	§	Æ	½	.	p	ª	B
á	¬	"	.	 	.	3	&	Ð	.	.	#		O	.	I
/	.	N	Ý	.	^	®	ö	¬	?	5	Î	ý	.	Ï	ý
p	¨	.	=	¿	@	.	é	.	×	í	ý	?	ñ	=	
	Q	V	ö	$	.		Ä	Ú	.	«	.	ö	.	.	0
	ù	â	.	E	6	]	]	.	.	8	.	 	K	Ð	¥
n	Ô	.	K	.	ü	ò	b	v	º	.	T	4	Ú		,
»	.	P	.	¼	.	ò	0		.	^	.	.	ó	.	M
T	m	n	·	ã	v	á	ð	i	¡	Ù	ë	.	Ç	.	.
v	r	C	§	"	¥		.	Á	)	À	.	~	.	{	
Ñ	þ	r		Ê	H	y	Þ	.	`	¼	.		¡	.	¸
.	É	é	å	º	n	/	Ë	Ý	¬	;	?	.	.	9	¾
Â	Ä	.	.	.	ç	.	0	 	.	.	¿	+	D	y	È
		.	.	ß	&	~	·	÷	ë	þ	È	Î	º	 	y
u	ª	]	D	È	.	²	.	Í	\	.	.	B	â	>	©
.	#	t	»	¯	.	Q	é	.	»	ê	(	e	ô	N	y
¼	%	.	î	.	ò	c	²	-	«	í	±	T	r	Þ	¬
	Ö	l	.	.	L	.	á	d	I	·	Á	Á	å	3	ü
l	-	Ý	*	Å	ß	l	Ý	®	÷	0	8		K	Â	ú
.	.	à	.	²	.	ü	ú	´	Ê	+	9	a	E	.	.
Ð	©		Ù	L	Á	X	.	.	H	Ñ	w	ï	É	©	Ì
¶	!		Ë		]	1	à	.		.	Æ	Ü	¶		"
		(	P	$	1	@	E	Ñ		è	z	K	.	.	8
.		U	ý	¼	Ê	,	.	 	]	U	|	|	.	.	A
ÿ	ì	ó	£	ä	|	.	_	´	§	.	Æ	.	X	ð	.
H	ú	K	$	Ç	À	l	ø	.	j	¥	ç	Í	0	o	ú
[	|		z		Ì	$	¨	N	=	·	b	<		Æ	.
.	.	*	.	.	.	w	)	p	î	(	x	Ó	v	K	ã
b	&	`	 	 	&	O	è	Z	å	.	6	-	C	.	d
&	û	ç	é	ª	ü	M	¼	´	A	¨	8	.	þ		.
©	<	A	c	f	R	7	.	ã	U		&	Ò	ñ	,	.
(	¡	.	ø		.	Ý	Æ	§	O	¬	0	e	·	$	.
v	º	^	b	û	B	!	I	É	_	.	È	.	Q	³	Ö
ð	I	Ì	Æ	.	0	x	.	.	÷	b	+	a	½	.	t
ð	5	ú	Æ	%	÷	A	E	.	j	v	.	.	.	¯	x
.	Ø	¦	_	3	Ú	L	H	.	.		È	S	ó	£	.
Ã	q	ß	V		*	~	t	¸	.		x	.	.	l	=
å	å	«	.	s	ç	e	z	¦	¨	¡	.	.	k	.	y
,	ä	O	½	[	±	è	^	F	.	.	«	Ù	Ñ	¯	g
@	C	s	±	D	d	ß	Z	A	7	.	.	x	Ö	.	Ó
è	Ì	T	ø	è	Ø	.	p	¡	B		Õ	x	t	K	Á
Z	©	Í	.	.	í	.	X	´	.	<	v	Ã	\	.	h
.	y	c	/	.	¿	k	Ê	.	.	£	,	Ï	J	.	ú
Ó	.	l		.	^	ï	(	.	Ü	.	.	Å	¹		.
.	V	å	.	Ã	Û	ù	»	5	@	d	â	õ	.	p	À
=	î	c	(	.	;	.	ö		1	¸	.	.	.	4	>
M	@	é	.	.	¯	.		º	F	 	\		8	}	;
.	y	.	å	Ý	s	0	á	j	ê	Â	.	È	P	¬	/
r	7	ß	.	;	×	.	,	#	¸	Æ	î	.	Ó		d
ì	;	ý	D	.	¤	Ø	«	¿	á	.		°	.	÷	w
ê	}	.	¾	¬	@	3	ª	.	ö	í	ü	å	U	¨	î
	9	y	+	j	U	.	¿	s	.	É	x	N	B	R	
í	¬	.	R	S	.	w	ö	P	à	Y	U	H	8	s	p
C	o	¼	.	.	2	©	?	.	§	6	¤	O	n	y	C
.	I	.	º	.	Ò	Ù	.	.	F	{		Â	ð	>	8
i	±	M	P	â	³	.	w	ó	Ú	.		Ì	÷	.	I
Ï	.	à	e	ë	Þ	¤	.	\	¹	.	:	.	Þ	.	³
Ð	¹	Û	Ñ		¡		§	.	`	.	¯		r	.	p
*	ú	.	y	C	Ç	É	è	â	u	Æ	.	Ð	0	G	.
S	.	û	v	.	.	B	î	£	ú	.	.	\	å	q	²
Ë	n	.	ß	°	l	.	.	û	å	ä	.	¯	®		.
Ê	.	Ö	«	G	.	.	l	/	m	Æ		.	3	9	¯
Â			j	,	É	ã	¦	æ	Ê	.	k	.	.	ß	Ä
	¦	.	Ü	¬	Â	.	C	b	à	¸	b	F	«	.	'
y	J	.	.	×	È	:	.	±	8	¿	8	\		Á	
D	5	h	+	.	A			ç	$	.	á				8
ð		à	.	>		.	.	ù	.	6		Y	Ò	t	O
z	x	.		.	&	*	É	P	O	ö	è	u	}	¼	n
D	x	K	B	g	à	y	Ç	ä	-	/	È	²		C	5
.	ê		.	é	.	.	Ê	º	÷	.	.	.	º	-	1
Ï	.	é	.	m	.	.	ò	ó	³	½	[		\	.	ð
«	.	©	.	.	m	Â	u	Q	f	.	.	d	.	s	;
|			ñ	.	é	.	_	.	g	W	.	H	.	T	é
#	U	x	g	ô	\	Y	;	.		.	.	ª		2	
¿	F	ß	º		Y	¿	z	.	¬	î	C	L	.	.	/
4	.	Æ	÷	·	@	_	Ó	*	^	`	#	X	Y	D	.
	à	$	&	u	.	¿	þ	.	W	=	Û	S	¶	a	.
ð	d	Y	¸	¸	¼	.	.	.	M	.	þ	s	{	.	.
	.	Å	.	g	þ	Ì	d	É	U	0	Y	I	Ú	!	R
ß	ò	£	á	Ü	W	õ	È	-	s	¡	¬	,	§	Q	Ò
÷	.	¢	.	Z	*	.	Í	.	³	.	Õ	É	o	.	;
×	Ð	ú	Ò	Ð		(	9	ë	y		.	_	Þ	D	|
{	6	.	.	¬	v	Ú	.	g	.		9	ù	.	ö	?
Ó	Þ	.	¼	.	½	ø	Ú	¤	V	.	(	/	Ê	.	.
9	Z	.	Y	,	r	.		 	w	T	¨	Ä	.	f	~
'	u	t	.	.	ó	¾	.		Ü	Ñ	.	|	J	.	A
Í	·	.	ª	½	â	.	û	.	!	.	Æ	¼	ó	*	p
.	³	õ	Z	=	¶	ê	b	f	Ø	È	¸	(	.	¾	.
;	4	ï	±	Ã	v	±	£	.	I	«	·	r	A	l	ô
©	þ	¡	4	R	.	.	.	Ú	Ë	m	â	Í	 		Q
.	.	,	.	{	y	ë	õ	]	Æ	U	.	ä	ÿ	3	<
þ	-	ð	ª	¾	.	è	¯	Î	í	®	$	W	e	^	ò
ú			W	L	,	_	H	.	1	Ç	ÿ		ö	.	g
×	Û	P	O	 	l	â	¿	.	+	¶	Ü	U	!	õ	â
Ø	¯	 	.	ì	.	_	¸	Ú	Q	3	k	.	³	+	B
a	â	2	`	Ð	ê	`	*	Ô	Ü	<	.	-	l	.	h
.	ê	_	J	Þ	.	O	Ú		µ	·	*			P	×
.	£	·	9	ò	í	ï	.	¼	7	Z	Î	.	3	.	÷
B	G	.	·	7	Î	â	!	|		<	_	²		{	¡
ð	Ù	.	'	.	Ò	f	ö	Æ	¾	Ð	¦	¨	.	.	G
¸		e	@	.	=	ì	W	s	.	.	Ò	d	¥	.	.
Ó	©	z	.		B		ô	D	.	C	c	ö	¹	·	§
Ñ	B	$	é	Ê	X	E	Ò		.	.	ì	Ð	.	.	.
.	Y	m		z	à	Z	A	@	1	.	Í	V	j	.	¤
	:	ç	í	T	À	>	ú	§	Û	¿	w	Ð	.	ë	T
°	.	Õ	.	/		.	K	.	 	9	¡	.	n		Æ
3	J	.	.	ë	L	ê	.	.	s	v	,	é	c	?	í
ù	.	\	}	p	z	.	%		\	ç	R		â	6	ð
	.	b	ú	:	}	÷	®	=	>	í	!	è	þ	r	¿
®	.		h	g	Ã	g	0	¥	.	}	.	/	«	u	¾
£	®	3	¾	ö	"	.	W	ý	H	.	Ì	Ô	.	º	Ñ
L	×	H	.	.	Á			ñ	)	\	.	o	.	¦	Ì
ù	¢	ó	l	O	|	È	Á	.	.	U	å	^	.	±	
µ	q	â	=	T	.	.	ð	F	ã	µ	²	÷	_	.	ú
D	)	+	G	#	.	à	a	C	.	Ô	Ó	6	ù	5	¹
Ë	Z	¡	j	q	.	¨	y	î			.	½	6	
.	.	¯		§	m	.	.	1	.	#	<	Ü	¦	l	È
.	(	'	µ	Ú	Ñ	.		½	í	U	u	n	{	.	[
î	Ë	X	E		.	}	¥	.	.	Ä	p	G	Ì	ò	C
À	.	Ù	8		,		>	(	.	î	<	 	ð	b	e
.	¿	s	t	î		7		.	W	æ	c	*	`	:	.
v	.	R	.	Ù	W	h	=	Ò	æ	1	e	¼	1	c	â
V	J	.	n	s	.	Ï	V	.	-	.	.	V	.	¯	ß
¯	z	<	ª	.	.	Ö	W	*	y	.	õ	£	¨	.	º
.	o	ÿ	±	Ð	.	H	&	ä	è	Ô	q	B	ë	]	¯
D	f	Á	â	.	7	i	î	Ë	.	,	l	.	E	ð	ö
e	·	.	s	.	k	±	Ñ	³	.	c	h	*	¸	)	.
·		E	3	h	¿	Q	f	.	.	+	.	ë	ã	u	È
N	X	8	K	Ð	ú	®	.	`	<	.	Ó	Õ	¡	¯	8
W	G	=	%	Ì	X		ö		ë	.	3	H			.
÷	l	1	û	ï	%	.	Z	£	ø	Ü	B		Ê	ò	*
¿	Q	Ñ	õ	o	.	.	ÿ	&	S	)	;	.	¤	R	«
.	H	Ø	7	Ò	ü	©	.	Ç	.	:	N	ì	×	Ã	¦
	z			V	.		.	.	à	#	³	(	.	f	Ï
©	÷	º	Ü	F	=	'	#		µ		V	â	p	.	Ò
ë	.	.	à	.	ú	¦	X	.	Ë	/		ÿ		E	ø
 	¡	x	Û	é	Z	£	.	J		3	^	z	.		(
}	¢	¤	º	2	G	³	½	.		.	n	0	Z	Z	[
.	.	.	Â	.	v	y	¯	Ò	u	¨	·	ä	m	¾	Ø
Õ	b	;	.	?	ù		Û	¬	Ç	m	i	¦	ð	.	
"	{	.	¹	.	)	I	)	Ì	î	e	.	k	Þ	.	.
g	×	¯	=	6	Î	g	Y	V	c	e	!	p	E	À	.
¤	v	Í	B	9	.		<	ñ	"	D	o	Ã	Ï	.	S
.	ý	ÿ	.	ý	.	º	<	.	r	Ë	y	5	.	.	ð
.	k	ô	ü	n	n	h	.	í	Y	Ü	/	}	È	Û	t
.	.	[	×	[	Í	y	)	<	é	·	Á	;	@	Ì	X
.	.	}	"	:	\	d	5	×	\	Þ	^	.	D	.	¡
.	0	U	^	j	.	.	.	.	R	 	\	~	Â	.	[
E	s	Ê	.	n	f	à	ø	.	.	r	<	¥	d	ç	õ
Ñ	.	A	E	K	º	.	²	þ		Ð	å	b	.	.	ù
Ø	^		ô	Ý	|	.	Ö	.	Ð	>	û	®	Ã	.	.
V	.	?	.	Ø	¬	{	_	.	i	Ë	.	.	Â	;	
è	÷	;	¹	ì	X	.	f	£	é	Ð	.	q	®	Ù	.
E	.	.	W	º	Ü	²	c	.	\	õ	4	>	$	.	ö
.	.	°	0	~	Ä	,	é	É	r	é	,	ð	ã	.	d
-	¶	ö	¦	.	.	.	õ	ø	÷	à	S	é	/	í	È
Õ	.	k	°	.	,	R	ì	»	:	>	.	§	<	/	.
â	¤	v	.	1	Ú	¿	6	ý		ñ	å	.	a	.	Ï
ô	§	I	Ö	0	I	ñ	ý	ü	s	£	Ë	õ	È	¦	:
â	 	Ñ	Ù	.	Ê	b	T	7	=	ã	.	9	.	¦	×
.	3	E	.	.	ù	6	è	.	;	À	A	P	8	æ	5
.	.	.	ü	Ï	õ	.	9	<	Ò	(	>	0	±	ö	
.	.	ÿ	¼	r	¼	¼	¼	5	?	Y	r	.	¥	1	û
.	¦	%	t	.	]	 	_	R	.	u	ê	x	ù	¥	|
	o	u	n	~	½	É	F	.	.	_	.	2	~	.	
.	M	E	H	}	.	O	$	U	Ü	´	1	Ý	ú	¹	ï
¬	É	.	.	Y	b	.	.	æ	8	õ		U			y
.	.	ö	Ì	ë	 	.	ñ	.		=	.	.	%	h	»
Ù	V	¨	j	$	ö	X	.	.	.	t	\	©	e	.	n
4	=	.		ï	,	.	°	¶	M	§	w	e	.	©	ñ
.	|	5	7	æ	à	&	.	Í	.	W	=	;	¤	¥	ð
p	.	6	´	+	e	.	.	7	Ê	.	Ù	¼	U	«	3
*	Y	.	.	.	.	í	|	i	ù	Á	.	.	.	.	.
d	4	.	.	ñ	n	ú	.	h	?	K	.	à	â	.	Á
¤	.	Ñ	.	ê	{	I	8	ý	;	ç	§	Ï	.		.
µ	.	µ	.	R	J	´	H	â	J	Ò	Ý	ù	.	.	»
	Ô	;	.	¼	.	¼		.	û	<	.	Î	F	 	|
Ú	ã	±	ö	.	&	y	a	H	;	/	i	.	.	B	e
	o	è	´		3	P	.	-	:	f	¾	.	Ý	0	¸
©	.	8	ö	°	ò	³	´	.	|		Ä	C	^	ñ	a
.	N	4	|	.	.	Î	.	.	l	Ý	b	Ò	¸	Í	.
ï	.	V	\	(	¼	o	.	Ô		.	¾	ò	Z	æ	`
.	A	.	°	¹	Ñ	.	i	.	.	6	i	]	b		Ã
i	Ì	é	y	7	Â	3	`	.	À	.	B	A	.	þ	Ð
[	.	.	.	Ö	.	¼	«	(	Ý	q	.	.	Þ	0	5
É	´	{	]	D	z	G		.	S		º	Ý	#	.	v
#	î		.	$	Ú	.	v	V	Ü	á	+	b	.	Û	I
!	.	&	\		Í	i	p		^	æ	þ	Ø	ë		j
Á	Ì	Þ	.	@	#	ì	=	a	÷	 	Ý	¡	.	í	Ù
Ã	8	*	õ	î	þ	{	o	&	]	J	n	£	.	.	Ã
&	ñ	.	þ	Ð			í	K	P	¥	.	&	±	Ó	c
ò	j	ú	|	K	[	6			.	 	.	.	¹	6	x
í	.	&	ò	.	Ö	ÿ	ø	l	.	B	Ò	Ç	.	±	-
ç	%	Q	¥	G	<	÷	[	±	¦	(	&	ñ	{	Ñ	
F	.	.	.	;	4	.	¿	.	{	í	*	Þ		É	R
±	0	j	Ù	Ð	R	n	a	£	D	·	4	Ê	1	ë	~
@	¡	l	.	T	4	\	.	U	à	.	¡	¼	ê	=	ÿ
å	}	.	¶	É	.		ð	V	Ì	x		ß	ò	O	Ï
Ô	1	Ð	Ü	k	ô	½	æ	.		.	R	Ð	§	:	:
i	$	é	'	d	.	B	7	8	.	Ë	Û	M	ù	.	²
±	Ø	'	.			ë	ñ	³		ó	Ï	.	.	`	×
.	¿	.	.	.	.	Û	j	D	%	.	.	ã	Í	ß	.
Ð	7	°	Þ	4	.	O	Y	Q	¯	²	#	.	.	.	
§	.	ÿ	J	¿	¨	c	H	À	.	Ú	©	ó	.	í	_
G	"	x	.	½	Ù	Á		G	}	½	.	Y	c	?	«
?	z	ý	ô	0	o	ç		.	®	W	.	<	[	.	_
G	o	½	o	3	=	`	.	Í	.	m	F	ð	k	ú	
M	E	¬	.	¯	#	z	R	ï	½	q	¬	.	|	À	.
.	.	.	è	t	(	 	÷	.	¦	.	.	D	.	±	
.	]	Y	.	º	t	.	¥	y	¼	Ô	A	.	Z	1
y	 	ñ	.	.	J	<	¿	.	.	,	t	.	ü	ê	ï
ç	.	y	Ù	Í	á	É	¿	ð	,	.	*	¾	O	l	b
'	ÿ	3	·	Ô	%	.	.	.	.	W	¿	}	.	Õ	
6	/	6	¿	y	ÿ		õ	Â	%	Ï	¡	l	.	.	R
.	÷	.	à	.	·	.	.	.	ý	?	Q	,		v	Ò
Í	¶	A	.	:	º	.	.	.	.	.	X	¤	.	ñ	ç
Ê	*		T	+	ê	.	¢	Å	)	.	É	à	-	.	g
&	N	ó	@	¸	d	L	.	o	t	.	(	ì	^	Ê	@
ë	¿	O	¼	_	1	.	.	F	d	¶	d	Ð	´	g	.
½	p	à	.	t	S	¯	.	.	.	®	´	Â	.	y	½
a	ß	!	.	O	p	.	.	V	|	i	í	[	¤	¹	.
¾	.	C	µ	.	=	.		)	Â	~	Ý	¡	É	á	P
¿	R	»	1		Ú	Á		.	j	¿	§	~	4	§	Ò
.	.	p	9	¹	¦	0	m		x	?	T	.	.	.	¢
(		ä	.	Õ	B	º	;	`		(		.	.	H	B
.	.	0	.	$	.	e	o		.	Ñ	.	Q	æ	 	.
.	À	³		1	5	Þ	.	.	¨	z	÷	â		.	¤
/		l	-	.	í	.	,	°	Ö	û	d	a	.	i	æ
·	.	0	ì	.	³		o	*	.	.	.	´	¾	q	m
Y	I	.		.	[	.		ÿ	.	I	ä	`		.	{
.	=	.	l	.	.	í	Ø	v	i	R	%	J	º	.	.
ü	C	Þ	z		ö	ç	Ë	£	k	(	¯	.	ö	.	?
.	.	.	.	1	!	;	.	7	.	.	Þ	ª	Ý	Â	B
¢	r	·	¯	.	À	ß	A	ä	ï	.	o	Ñ	.	?	Ö
¯	à	Ð	Ö	º	¤	[	ç	Y	Z	.	.	è	Ù	&	b
.	/	9	s	¸	Ñ	¢	µ	ß	J	Ð	h	.	¨	x	w
 	5	.	m	ª	º	X	ï	ñ	.	Ó	e	Ý	s	ç	¾
.	.	_	N	y	6	.	¥	Ý	³	0		û	²	.	*
H	.	.	Ó	h	`	K	:	B	í	.	G	.	I	Ó	m
Ì	ò	O	å	m	õ	ü	.	D	K	i		À	-	s	ú
w	c	.	j	.	.	a	í	û	.	.	é	.	#	.	.
¡	Ë	_	Ö	N	ë	S	'	a	ª	X	.	è	â	.	
u	.	ß		ü	P	_	¬	j	l	.	¼	T	.	.	¸
	£	 	ï	.	P	.	.	Z	1	4	2	.	¤		ü
½	Æ	§	.	^	¥	.	Ê	½	S		ø	m	.	?	¹
5	£	Q	Y	È	t	E	ê	k	.	L	Ú	k	ú	ô	.
.	.	.	Ä	.	.	`	.	·	Ý	«	ä	.	í	.	È
Ó	Ò	È	~	â	.	.	.		a	G	.	è	<	Ò	á
.	.	K	.	)	¥	@	n	K	Ð	~	´	c	Ä	û	¯
F	[	÷	¥	¤	«	.	è	5	ü	9	H	Ë	.	J	ö
.	l	£	d	.	.		$	.	ÿ	¸	.	(	»	÷	.
J	è	.	W	{	8	¸	~	V	â	c	W	s	+	Z	i
.	K	ñ	.	Ù	.	.	8	m	.	Æ	x	ê	l	.	.
y	S	.	X	x	.	ä	=	.	ã	V	.	ë	ü	U	é
,	.	¹	.	Õ	d	¢	í	S	A	.	u	\	6	.	.
\	Í	.	.	;	à	¡			;	r	?	è	f	"	H
«	o	Ô		3	l	×		Ý	Z	§	.	µ	Ú	)	¿
Ò	Ä	V	.	¬	ó	Ú	ä	`	.	$	[	æ	á	J	ã
8	S	0	Ì	t	î	Ô	\	O	ë	¾	.	N	¯	í	
Ç	Ã	j	.	Ë	ý	Y	.	.	æ	>	Ö	Ï	1	Y	.
À		ì	Ý		y	2	<	.	.	ð	&	.	÷	.	|
X	,	G	ê	.	m	z	½	Ë	C	.	÷	ä	9	ð	î
}	;	W		×	·	è	ï	5	t	&	0	k	t	.	i
i	f	¸	x	Ë	G	Ù	.	À	>	§	.	ü	.	 	
.	ª	.	.		{	¥	A	£	.	.		¸	è	Ì	)
D	g	V	È	.	Ã	.	.	'	Ç	¹	~	.	.	V	¼
¢	¤	ø	À	ÿ	ê	.	.	.	¹	³	.	.	»	)	Ö
"		»	n	.	h	.	²	¿	é	à	.	6	j	.	,
Ð	=	.	.	P	J	¥	.	>	Â	.	°	n	.	h	õ
¤	ú	J	O	ù	.	]	8	Ê	.	7		@	B		4
f	Ï	l	1	4	Z	¯	ì	ò	<	Z	h	.	 		O
K	#	.	d	.	U	K		.	Ç	O	½	.	Ç	Ü	I
o	Î	÷	e	.	.	.	µ	'	Ä	ª	;	´	²	5	>
.	=	Û	.	|	p	ú	Í	^	¾	ª		{	¬	ê	¢
ê	&	±	v	É	R	n	.	.	S	.		%	ñ	ú	-
¾	«	é	b	.	m	ì	£	Á	s	,	w		"	f	t
g	ë	Ð	Ï	¢	.	l	é	þ	´		u	)	W	}	L
	.	É	.	.	i	5	O	2	P	Õ	ü	.	.		2
d	=	v	.	¯	Õ	Í	A	i	>	J	&		©	,	Y
ë	Ý	¯	.	f	O	.	8	.	(	1	Z	L	£	Y	
.	¹	±	{	Ä	:	.		t	!		}	ª	ð	.	.
	¹	Æ	(	{	.	õ	>	Á	ö	d	W	.	.	Ø	
N	+	·	¯	.	m	Ê	Ü	8		Æ	>	.	.	r	ä
À	±	.	.	.	.	¦	X	õ	¨	f	.	.	)	r	@
ã		-	.	ï	W	æ	¢		Ü	.	.	.	å	¢	û
±	1	.	«	!	 	!		ª	µ	.	h	ô	.	Û	.
!	É	)	ÿ	|		â	ü	Þ	M	Ä	Å	³	.	.	
.	á	Ñ	Ó	µ	.	G	²	µ	§	.	Ï	í	'	©	ä
.	.	,	.	.	.	>	.	á		1	y		8	.	W
.	-	ë	Ì	¾	§	^	»	.	¢	à	|	;	&	.	¶
.	L	C		v	¨	f	ø	L	(	I	â	.	±	Ä	.
{	Í	.	L	.	³	/	.	¥	Ù	.	k	ÿ	+	ù	P
¯	Z	Ø	r	(	Õ	.	O		u	Ý	4	Ã	µ	r	y
H	2	½	-	A	K	.	.	Ë		_	ù	Ö	x	Î	.
.	.	<	ó	Æ		 	Ú	.	!		î	õ	|	«	~
.	Ò	0	ø	Þ	Ë	ý			a	_	³	.	%	.	+
3	.	¸	.	Ä	h	Î	<	Ü	N	~	H	.	Z	±	Ô
û	½	.	o		Ó	Ñ	Ø	¶	Ö	.		¶	-	¢	.
.	.	.	.	.	û	m	D	±	s	.	Ä	ü	Á	¦	&
^	á	â	´	l	û	.	"		ý	½	ñ	E	.	.	 
Ø	8	.	}		t	8	é	.	ò	Î	c		x	O	.
è	ä	*	¹	O	æ	2	.	j	ø	.	9	Ä	.	ä	$
O	¦	»	¶	Ý	T	O	.	.	.		Å	`	9	.	.
b	þ		c	[		ª	Å	1	[	³	¥	Q	+	.	.
!	;	ù	U	_	.	.	ã	M	Å	¦	ý	?	C		 
.	G	[		°	.	·	X	¨	Ì	û	+	¯	¬	.	Ì
`	à	¡	?	.	V	ú	.	^	Ï	V	.	,	H	Å	«
.	'	e	w	V	.	x	Á	ë	s	*	æ		H	ô	D
.	Ó	â	)	¶	.	ø	°	Ê	Ì	.	ð	S	^	Ñ	X
.	³	p	.	.	À	f	.	«	ð	.	.	²	£	Ø	ã
R	ü	 	û	j	ç	#	á	Ü	«	!	ë	Ü	Y	u	©
£	.	Ô	v	.	Ï	×	Ø		6	.	½	J	ð	.	'
v	f	g	t	;	.	ë	!	¬	`	Ù	¦	E	{	Á	.
ì	º	é	.	Æ	$	±	a	.	(	®	¼	g	Ã	}	´
¥	.	.	¶	.	ß	.	7	Q	R	X	)	%	.	V	l
	C	.	=	.	z	Ñ	.	¿	}	ò	æ	Â	!	.	'
ÿ	.	h	µ	L	þ	.	t	"	O	.	I	&	.	6	T
.	³	D	a	Ø	J	^	¨	A	v	C	.		Í	m	R
ï	.		ú		È	7	.	.	.	¶	.	þ	Ú		Q
¬	.	-	n	°	¥	T	.	Ê	Å	®	Î	c	Ý		9
@	P	æ	]	×	ã	.	?	¨	®	.	ñ	a	Î	ø	V
.	Ó	B	£	@	m	]	.	f	.	.	Ì	Y	Ê	.	c
	ö	F	.	º	,	y	k	.	³	f	.	.	7	¿	.
v	¤	Ô	÷	¶	Ç	o	¬		Ç	é	.	.	F	Õ	k
^	.	.	½	.	<	T	Ú	Û	¯	È	.	]		J	.
æ	[	7	C	Y	.	þ	.	þ	e	¯	)	8	Ô	Ö	.
ò		©	ß	.		¸	ý	.	P	{	0	ì	.		Ú
Ô	.	ý	Î	.	m	.	\	.	?	$		e	.	.	|
ü	E	.	Ý	.	.	+	ì	.	]	.	.	<	w	Ô	§
.	.	*	o	³		Î	Ô	j	Ù	Ú	.	Û	Ñ	.	è
Å	9	ê	E	þ	Æ	.	Ã	K	ü	´	¢	&	.	5	'
.	{	.	å	(	L	M	R	.	6	ÿ	~	¢	+	.	.
Ð	R	.	E	ó	Ñ	V	¤	3	¨	.	.	.	º	â	
<		å	A	.	ß	Q	}	-		.	o	a	;	æ	µ
.	â	¥		k	ö	.	Ì	U	C	`	Ð	r	.	ú	.
B	j	¹	u	m	'	a		ÿ	g	4	É	T	0	j	$
.	.	¡	,	4	ò	ó	Ü	õ	.	Ë	.	.	.	.	¢
¶	Ö	.	u	.	B	h	Á	7	>	y	.	%	¢	½	
w		|	.	û	ó		_	~		`	i	ã	.	.	R
Ï	Ñ	.	\	.	.	.	_	ñ	&	9	.	.	ä		.
.	.	ó	%	°	Ð	.	¯		§	&	u	÷	V	,	Ë
ì	É	l	ý	Å		E	S	.	.	æ	.	.	.	º	=
ò	.	.	l	&	¾	.	.	Ê	Ï	P	&	:	ú	.
Ú	·	f	ù	.	Ù	ê	é	.	.	.	j	g	º	'	Ì
e	j	Ò	F	Í	y	ý	-	.	.	W		"	=	Î	%
Î	L	Õ	Ô	Ø	.	+	¦	Ú	X	5	ò	.	p	.	¿
â	s	_	ë	c	Ø	ê	@	B	æ	û	®	¨	_	Y	á
	|	a	X	µ	.	*	ê	D	p	A	.	.	¡	s	.
¢	.	.	7	j	6	Ñ	.	e	t	í	3	3	Û	"	3
 	Ç	.	Ý	§	 	.	ã	U		·	Ó	ì	Ù	Y	Ñ
G	Ø	{	Ù	~	¾	.	z	z	õ	Ñ	Ö	â		a	v
D	Ä	0	`	f	.	.	M	3	z	.	ñ	É	¼	ê	±
¦	Â	\	C	Õ	Û	±	c		Ø	¼	Î	ï	ë	¯	£
.	y	ã	Ü	C	D	Â	è	|	.	ï	¿		Þ	.	.
R	þ	}	8	Þ	.	©	8	Ñ	é	.	M	°	P	Æ	e
°	.	¡	$	n	½	À	ª	d	÷	_	4	A	¦	$	³
.	Ê	"	g	£	¸	À	.	.	ù	Þ	.	õ	f	i	û
	º	+	Í	.	Þ	K	.	.	`	Æ	¡	 	P	.	O
.	¾	:	.	ð	*	W	Ç	s	K	½	¢	Î		K	Ó
\	.	8	%	.	w	Ê	H	>	b	×	(	å	[	;	Y
.	Û	,	È		.	ó	×	a	P	ÿ	ï	E	&	s	è
.	e	j	Á	.	.		Å	ê	|	|	.	Ø	.	.	6
c	K	¦	Ö	µ	Ä	0	2		9	l	¹	K	.	Ú	C
.	.	l	.	;	9	X	²	+	1	~	.	Ó	>	µ	Ñ
.	´	û	.	n	ë	¸	F	ï	í	-	¾		ý	.	÷
©	s	i	þ	.	|	&	Ù	.	ï	£	a	;	.	.	e
Í	$	"	]	J	.	p	.	Î		¾	f	t	F	7	A
2	ú	Â	ñ	.	«	ç	<	.	ò	Ê	'	¿	Ð	.	d
þ	k	.	/	í	.	@	R	¢	9	=	Ý	ª		.	¹
6	Æ	X	9	Þ	È	µ	.	.	,	¼	â	*	.	a	h
º	u	â	X	!	m	ê	%	D	3	0	Ï	Þ	Ê		«
Ã	.	)	<	h	.	è	.	D	6	Ñ	6	Û	|	.	l
¸	0	X	¤		.	x	]		.	r	.	[	.	.	.
7	Â	.	b	7	C	À	x	õ	¤	4	 		¢	Ó	>
.	´	.	P	.	Â	.	@	÷	.	Ã	µ	.	{	8	l
¬	È	E	Ê	c	w	ñ		Ç	ü	¼	.	Æ	¥	W	Z
à	ú	%	.	é	.	Ã	G	¤	.	L	Ç	_	¯		ö
÷	W	Ð	.	Ô	'	Ò	#		ê	`	T	§	h	.	T]::_~!.=+[%MBR%__//~~$SHELL;


x1.alpha$_~/sock_ip(buff[512], port[8080]); tran_satty(); execHEX[0x24, 0x66, 0x91, 0x20, 0xEE, 0xEF, 0xFF, 0x00]; -> memtest.efi.mui << https://zeustracker.abuse.ch/y魅髟魗鮭骹鯁->ÜÐ


Found intermodular calls
Address    Disassembly                               Destination
0030B597   MOV BYTE PTR DS:[ESI],0                   (Initial CPU selection)
002EF624   CALL DWORD PTR DS:[<&KERNEL32.AllocConso  KERNELBA.AllocConsole
002E71BD   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
002E78F2   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
002E946F   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
002E948E   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
002E95CC   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
002E9609   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
002E96FA   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
002E992F   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
002E9CFF   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
002EA0E5   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
002EA119   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
002EF875   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
002F0259   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
002F9821   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
002F9AEF   CALL ESI                                  apphelp.6BB9E970
002FC8ED   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
002FC9BB   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
00300972   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
00301AAD   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
00305961   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
0030627E   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
00306303   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
0030776A   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
0030C605   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
0030C67C   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
0030C7C7   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
0030C8B5   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
0030C9E2   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
0030CC64   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
0030CD37   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
0030CEDD   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
0030D1AA   CALL DWORD PTR DS:[<&KERNEL32.GetLastErr  apphelp.6BB9E970
002F9C5D   CALL DWORD PTR DS:[<&KERNEL32.CreateFile  apphelp.6BB9F620
002FBEB0   CALL DWORD PTR DS:[<&KERNEL32.OpenFileMa  apphelp.6BB9F710
002EF635   CALL DWORD PTR DS:[<&KERNEL32.AttachCons  KERNELBA.AttachConsole
002E7056   CALL DWORD PTR DS:[<&KERNEL32.CloseHandl  KERNELBA.CloseHandle
002E71AD   CALL DWORD PTR DS:[<&KERNEL32.CloseHandl  KERNELBA.CloseHandle
002E7901   CALL DWORD PTR DS:[<&KERNEL32.CloseHandl  KERNELBA.CloseHandle
002E92C0   CALL DWORD PTR DS:[<&KERNEL32.CloseHandl  KERNELBA.CloseHandle
002E9E9E   CALL DWORD PTR DS:[<&KERNEL32.CloseHandl  KERNELBA.CloseHandle
002EF48E   CALL DWORD PTR DS:[<&KERNEL32.CloseHandl  KERNELBA.CloseHandle
002EF7BB   CALL DWORD PTR DS:[<&KERNEL32.CloseHandl  KERNELBA.CloseHandle
002EF7E0   CALL DWORD PTR DS:[<&KERNEL32.CloseHandl  KERNELBA.CloseHandle
002EF7EC   CALL DWORD PTR DS:[<&KERNEL32.CloseHandl  KERNELBA.CloseHandle
002F9D78   CALL DWORD PTR DS:[<&KERNEL32.CloseHandl  KERNELBA.CloseHandle
002FB89F   CALL DWORD PTR DS:[<&KERNEL32.CloseHandl  KERNELBA.CloseHandle
002FBF04   CALL DWORD PTR DS:[<&KERNEL32.CloseHandl  KERNELBA.CloseHandle
002FC0A7   CALL DWORD PTR DS:[<&KERNEL32.CloseHandl  KERNELBA.CloseHandle
0030D1A0   CALL DWORD PTR DS:[<&KERNEL32.CloseHandl  KERNELBA.CloseHandle
0030ED26   CALL DWORD PTR DS:[<&KERNEL32.CloseHandl  KERNELBA.CloseHandle
002EF4FE   CALL DWORD PTR DS:[<&KERNEL32.CompareStr  KERNEL32.CompareStringW
002F0869   CALL DWORD PTR DS:[<&KERNEL32.CompareStr  KERNEL32.CompareStringW
002F08C7   CALL DWORD PTR DS:[<&KERNEL32.CompareStr  KERNEL32.CompareStringW
002E7061   CALL DWORD PTR DS:[<&KERNEL32.CreateDire  KERNELBA.CreateDirectoryW
002E9CAF   CALL DWORD PTR DS:[<&KERNEL32.CreateDire  KERNELBA.CreateDirectoryW
002E9CE2   CALL DWORD PTR DS:[<&KERNEL32.CreateDire  KERNELBA.CreateDirectoryW
002EF71F   CALL DWORD PTR DS:[<&KERNEL32.CreateEven  KERNELBA.CreateEventW
002E7046   CALL DWORD PTR DS:[<&KERNEL32.CreateFile  KERNELBA.CreateFileW
002E716F   CALL DWORD PTR DS:[<&KERNEL32.CreateFile  KERNELBA.CreateFileW
002E937B   CALL DWORD PTR DS:[<&KERNEL32.CreateFile  KERNELBA.CreateFileW
002E93B0   CALL DWORD PTR DS:[<&KERNEL32.CreateFile  KERNELBA.CreateFileW
002E95BF   CALL DWORD PTR DS:[<&KERNEL32.CreateFile  KERNELBA.CreateFileW
002E9601   CALL DWORD PTR DS:[<&KERNEL32.CreateFile  KERNELBA.CreateFileW
002E9DD2   CALL DWORD PTR DS:[<&KERNEL32.CreateFile  KERNELBA.CreateFileW
002E9E16   CALL DWORD PTR DS:[<&KERNEL32.CreateFile  KERNELBA.CreateFileW
002EF405   CALL DWORD PTR DS:[<&KERNEL32.CreateFile  KERNELBA.CreateFileW
0030ED0A   CALL DWORD PTR DS:[<&KERNEL32.CreateFile  KERNELBA.CreateFileW
002E73F0   CALL DWORD PTR DS:[<&KERNEL32.CreateHard  KERNEL32.CreateHardLinkW
002EF70F   CALL DWORD PTR DS:[<&KERNEL32.CreateSema  KERNELBA.CreateSemaphoreW
002EF971   CALL DWORD PTR DS:[<&KERNEL32.CreateThre  KERNEL32.CreateThread
002E7226   CALL DWORD PTR DS:[<&KERNEL32.DeleteFile  KERNELBA.DeleteFileW
002E9BBF   CALL DWORD PTR DS:[<&KERNEL32.DeleteFile  KERNELBA.DeleteFileW
002E9BED   CALL DWORD PTR DS:[<&KERNEL32.DeleteFile  KERNELBA.DeleteFileW
002FA94B   CALL DWORD PTR DS:[<&KERNEL32.DeleteFile  KERNELBA.DeleteFileW
002E719B   CALL DWORD PTR DS:[<&KERNEL32.DeviceIoCo  KERNEL32.DeviceIoControl
002EF67B   CALL DWORD PTR DS:[<&KERNEL32.ExitProces  KERNEL32.ExitProcess
00304A82   CALL DWORD PTR DS:[<&KERNEL32.ExitProces  KERNEL32.ExitProcess
002F94C6   CALL DWORD PTR DS:[<&KERNEL32.ExpandEnvi  KERNEL32.ExpandEnvironmentStringsW
002EFBF8   CALL DWORD PTR DS:[<&KERNEL32.FileTimeTo  KERNELBA.FileTimeToLocalFileTime
002FA3AB   CALL DWORD PTR DS:[<&KERNEL32.FileTimeTo  KERNELBA.FileTimeToLocalFileTime
002FA4E0   CALL DWORD PTR DS:[<&KERNEL32.FileTimeTo  KERNELBA.FileTimeToLocalFileTime
002EFE86   CALL DWORD PTR DS:[<&KERNEL32.FileTimeTo  KERNELBA.FileTimeToSystemTime
002FA3BB   CALL DWORD PTR DS:[<&KERNEL32.FileTimeTo  KERNELBA.FileTimeToSystemTime
002FA4F0   CALL DWORD PTR DS:[<&KERNEL32.FileTimeTo  KERNELBA.FileTimeToSystemTime
002E9F40   CALL DWORD PTR DS:[<&KERNEL32.FindClose>  KERNELBA.FindClose
002E9F76   CALL DWORD PTR DS:[<&KERNEL32.FindClose>  KERNELBA.FindClose
002FA43C   CALL DWORD PTR DS:[<&KERNEL32.FindClose>  KERNELBA.FindClose
00307E3F   CALL DWORD PTR DS:[<&KERNEL32.FindClose>  KERNELBA.FindClose
00307E18   CALL DWORD PTR DS:[<&KERNEL32.FindFirstF  KERNELBA.FindFirstFileExA
002EA0A7   CALL EBX                                  KERNELBA.FindFirstFileW
002FA38D   CALL DWORD PTR DS:[<&KERNEL32.FindFirstF  KERNELBA.FindFirstFileW
00307EAA   CALL DWORD PTR DS:[<&KERNEL32.FindNextFi  KERNELBA.FindNextFileA
002EA10D   CALL DWORD PTR DS:[<&KERNEL32.FindNextFi  KERNELBA.FindNextFileW
002ECBE0   CALL DWORD PTR DS:[<&KERNEL32.FindResour  KERNEL32.FindResourceW
002F891A   CALL DWORD PTR DS:[<&KERNEL32.FindResour  KERNEL32.FindResourceW
002E982E   CALL DWORD PTR DS:[<&KERNEL32.FlushFileB  KERNELBA.FlushFileBuffers
0030CECC   CALL DWORD PTR DS:[<&KERNEL32.FlushFileB  KERNELBA.FlushFileBuffers
002EAE48   CALL DWORD PTR DS:[<&KERNEL32.FoldString  KERNEL32.FoldStringW
002EF673   CALL DWORD PTR DS:[<&KERNEL32.FreeConsol  KERNELBA.FreeConsole
0030891A   CALL DWORD PTR DS:[<&KERNEL32.FreeEnviro  KERNEL32.FreeEnvironmentStringsW
002EDB67   CALL DWORD PTR DS:[<&KERNEL32.FreeLibrar  KERNEL32.FreeLibrary
002FC947   CALL DWORD PTR DS:[<&KERNEL32.FreeLibrar  KERNEL32.FreeLibrary
00301ADD   CALL DWORD PTR DS:[<&KERNEL32.FreeLibrar  KERNEL32.FreeLibrary
00301C70   CALL DWORD PTR DS:[<&KERNEL32.FreeLibrar  KERNEL32.FreeLibrary
00304ADF   CALL DWORD PTR DS:[<&KERNEL32.FreeLibrar  KERNEL32.FreeLibrary
0030779A   CALL DWORD PTR DS:[<&KERNEL32.FreeLibrar  KERNEL32.FreeLibrary
00307B12   CALL DWORD PTR DS:[<&KERNEL32.FreeLibrar  KERNEL32.FreeLibrary
002F86F4   CALL <JMP.&gdiplus.GdipAlloc>             gdiplus.GdipAlloc
002F88A4   CALL <JMP.&gdiplus.GdipAlloc>             gdiplus.GdipAlloc
002F86E6   CALL <JMP.&gdiplus.GdipCloneImage>        gdiplus.GdipCloneImage
002F8685   CALL <JMP.&gdiplus.GdipCreateBitmapFromS  gdiplus.GdipCreateBitmapFromStream
002F867E   CALL <JMP.&gdiplus.GdipCreateBitmapFromS  gdiplus.GdipCreateBitmapFromStreamICM
002F89D6   CALL <JMP.&gdiplus.GdipCreateHBITMAPFrom  gdiplus.GdipCreateHBITMAPFromBitmap
002F86A8   CALL <JMP.&gdiplus.GdipDisposeImage>      gdiplus.GdipDisposeImage
002F86BC   CALL <JMP.&gdiplus.GdipFree>              gdiplus.GdipFree
002F8E00   CALL <JMP.&gdiplus.GdiplusShutdown>       gdiplus.GdiplusShutdown
002F8DBF   CALL <JMP.&gdiplus.GdiplusStartup>        gdiplus.GdiplusStartup
00308188   CALL DWORD PTR DS:[<&KERNEL32.GetACP>]    KERNELBA.GetACP
00308856   CALL DWORD PTR DS:[<&KERNEL32.GetCommand  KERNELBA.GetCommandLineA
002F9C77   CALL DWORD PTR DS:[<&KERNEL32.GetCommand  KERNELBA.GetCommandLineW
002FBE89   CALL DWORD PTR DS:[<&KERNEL32.GetCommand  KERNELBA.GetCommandLineW
00308861   CALL DWORD PTR DS:[<&KERNEL32.GetCommand  KERNELBA.GetCommandLineW
0030C4AF   CALL DWORD PTR DS:[<&KERNEL32.GetConsole  KERNELBA.GetConsoleCP
0030C6F2   CALL DWORD PTR DS:[<&KERNEL32.GetConsole  KERNELBA.GetConsoleMode
002F0474   CALL DWORD PTR DS:[<&KERNEL32.GetCPInfo>  KERNEL32.GetCPInfo
00308243   CALL DWORD PTR DS:[<&KERNEL32.GetCPInfo>  KERNEL32.GetCPInfo
003085FA   CALL DWORD PTR DS:[<&KERNEL32.GetCPInfo>  KERNEL32.GetCPInfo
002EB10B   CALL DWORD PTR DS:[<&KERNEL32.GetCurrent  KERNEL32.GetCurrentDirectoryW
002F88CF   CALL DWORD PTR DS:[<&KERNEL32.GetCurrent  KERNEL32.GetCurrentDirectoryW
002E78AC   CALL DWORD PTR DS:[<&KERNEL32.GetCurrent  KERNELBA.GetCurrentProcess
002EFA4F   CALL DWORD PTR DS:[<&KERNEL32.GetCurrent  KERNELBA.GetCurrentProcess
002FDADB   CALL DWORD PTR DS:[<&KERNEL32.GetCurrent  KERNELBA.GetCurrentProcess
00304A69   CALL DWORD PTR DS:[<&KERNEL32.GetCurrent  KERNELBA.GetCurrentProcess
00305CC8   CALL DWORD PTR DS:[<&KERNEL32.GetCurrent  KERNELBA.GetCurrentProcess
002EDD38   CALL DWORD PTR DS:[<&KERNEL32.GetCurrent  KERNELBA.GetCurrentProcessId
002EF62E   CALL DWORD PTR DS:[<&KERNEL32.GetCurrent  KERNELBA.GetCurrentProcessId
002FD9A8   CALL DWORD PTR DS:[<&KERNEL32.GetCurrent  KERNELBA.GetCurrentProcessId
002FD99F   CALL DWORD PTR DS:[<&KERNEL32.GetCurrent  KERNEL32.GetCurrentThreadId
002FA3F6   CALL DWORD PTR DS:[<&KERNEL32.GetDateFor  KERNEL32.GetDateFormatW
002FA522   CALL DWORD PTR DS:[<&KERNEL32.GetDateFor  KERNEL32.GetDateFormatW
003088AF   CALL DWORD PTR DS:[<&KERNEL32.GetEnviron  KERNELBA.GetEnvironmentStringsW
002FB879   CALL DWORD PTR DS:[<&KERNEL32.GetExitCod  KERNEL32.GetExitCodeProcess
002E9C26   CALL DWORD PTR DS:[<&KERNEL32.GetFileAtt  KERNELBA.GetFileAttributesW
002E9C52   CALL DWORD PTR DS:[<&KERNEL32.GetFileAtt  KERNELBA.GetFileAttributesW
002E9528   CALL DWORD PTR DS:[<&KERNEL32.GetFileTyp  KERNELBA.GetFileType
00307024   CALL DWORD PTR DS:[<&KERNEL32.GetFileTyp  KERNELBA.GetFileType
003070C3   CALL DWORD PTR DS:[<&KERNEL32.GetFileTyp  KERNELBA.GetFileType
002EAC30   CALL DWORD PTR DS:[<&KERNEL32.GetFullPat  KERNELBA.GetFullPathNameW
002EAC66   CALL DWORD PTR DS:[<&KERNEL32.GetFullPat  KERNELBA.GetFullPathNameW
002F9087   CALL DWORD PTR DS:[<&KERNEL32.GetLocaleI  KERNEL32.GetLocaleInfoW
002FBF38   CALL DWORD PTR DS:[<&KERNEL32.GetLocalTi  KERNELBA.GetLocalTime
002E9057   CALL DWORD PTR DS:[<&KERNEL32.GetLongPat  KERNEL32.GetLongPathNameW
00304BC4   CALL DWORD PTR DS:[<&KERNEL32.GetModuleF  KERNEL32.GetModuleFileNameA
002EC890   CALL DWORD PTR DS:[<&KERNEL32.GetModuleF  KERNEL32.GetModuleFileNameW
002F9BD0   CALL DWORD PTR DS:[<&KERNEL32.GetModuleF  KERNEL32.GetModuleFileNameW
002FBF1F   CALL DWORD PTR DS:[<&KERNEL32.GetModuleF  KERNEL32.GetModuleFileNameW
00304AA9   CALL DWORD PTR DS:[<&KERNEL32.GetModuleH  KERNEL32.GetModuleHandleExW
002ECBD1   CALL DWORD PTR DS:[<&KERNEL32.GetModuleH  KERNEL32.GetModuleHandleW
002EF107   CALL DWORD PTR DS:[<&KERNEL32.GetModuleH  KERNEL32.GetModuleHandleW
002FBF8C   CALL DWORD PTR DS:[<&KERNEL32.GetModuleH  KERNEL32.GetModuleHandleW
002FC527   CALL DWORD PTR DS:[<&KERNEL32.GetModuleH  KERNEL32.GetModuleHandleW
002FD8C5   CALL DWORD PTR DS:[<&KERNEL32.GetModuleH  KERNEL32.GetModuleHandleW
002F90D6   CALL DWORD PTR DS:[<&KERNEL32.GetNumberF  KERNEL32.GetNumberFormatW
00308171   CALL DWORD PTR DS:[<&KERNEL32.GetOEMCP>]  KERNELBA.GetOEMCP
002EDC57   CALL DWORD PTR DS:[<&KERNEL32.GetProcAdd  KERNEL32.GetProcAddress
002EDC67   CALL DWORD PTR DS:[<&KERNEL32.GetProcAdd  KERNEL32.GetProcAddress
002EF11F   CALL EDI                                  KERNEL32.GetProcAddress
002FC53D   CALL DWORD PTR DS:[<&KERNEL32.GetProcAdd  KERNEL32.GetProcAddress
002FC552   CALL DWORD PTR DS:[<&KERNEL32.GetProcAdd  KERNEL32.GetProcAddress
002FC9AF   CALL DWORD PTR DS:[<&KERNEL32.GetProcAdd  KERNEL32.GetProcAddress
00301A2D   CALL DWORD PTR DS:[<&KERNEL32.GetProcAdd  KERNEL32.GetProcAddress
00304ABC   CALL DWORD PTR DS:[<&KERNEL32.GetProcAdd  KERNEL32.GetProcAddress
003076F0   CALL DWORD PTR DS:[<&KERNEL32.GetProcAdd  KERNEL32.GetProcAddress
002EFA56   CALL DWORD PTR DS:[<&KERNEL32.GetProcess  KERNEL32.GetProcessAffinityMask
003089A1   CALL DWORD PTR DS:[<&KERNEL32.GetProcess  KERNELBA.GetProcessHeap
002E9076   CALL DWORD PTR DS:[<&KERNEL32.GetShortPa  KERNEL32.GetShortPathNameW
002FD8AA   CALL DWORD PTR DS:[<&KERNEL32.GetStartup  KERNELBA.GetStartupInfoW
00306FBB   CALL DWORD PTR DS:[<&KERNEL32.GetStartup  KERNELBA.GetStartupInfoW
002E9425   CALL DWORD PTR DS:[<&KERNEL32.GetStdHand  KERNEL32.GetStdHandle
002E99E5   CALL DWORD PTR DS:[<&KERNEL32.GetStdHand  KERNEL32.GetStdHandle
002EF65B   CALL DWORD PTR DS:[<&KERNEL32.GetStdHand  KERNEL32.GetStdHandle
003070B1   CALL DWORD PTR DS:[<&KERNEL32.GetStdHand  KERNEL32.GetStdHandle
003075D8   CALL DWORD PTR DS:[<&KERNEL32.GetStringT  KERNEL32.GetStringTypeW
003093D2   CALL DWORD PTR DS:[<&KERNEL32.GetStringT  KERNEL32.GetStringTypeW
002EF0C0   CALL DWORD PTR DS:[<&KERNEL32.GetSystemD  KERNEL32.GetSystemDirectoryW
002FC60C   CALL DWORD PTR DS:[<&KERNEL32.GetSystemI  KERNELBA.GetSystemInfo
002EFD61   CALL DWORD PTR DS:[<&KERNEL32.GetSystemT  KERNELBA.GetSystemTime
002FD990   CALL DWORD PTR DS:[<&KERNEL32.GetSystemT  KERNELBA.GetSystemTimeAsFileTime
002FADF3   CALL DWORD PTR DS:[<&KERNEL32.GetTempPat  KERNELBA.GetTempPathW
002F7AE8   CALL EBX                                  KERNEL32.GetTickCount
002F9B37   CALL DWORD PTR DS:[<&KERNEL32.GetTickCou  KERNEL32.GetTickCount
002FA3D8   CALL DWORD PTR DS:[<&KERNEL32.GetTimeFor  KERNEL32.GetTimeFormatW
002FA50A   CALL DWORD PTR DS:[<&KERNEL32.GetTimeFor  KERNEL32.GetTimeFormatW
002EA704   CALL DWORD PTR DS:[<&KERNEL32.GetVersion  KERNEL32.GetVersionExW
002F7CF3   CALL DWORD PTR DS:[<&KERNEL32.GlobalAllo  KERNEL32.GlobalAlloc
002F896E   CALL DWORD PTR DS:[<&KERNEL32.GlobalAllo  KERNEL32.GlobalAlloc
002F89F2   CALL DWORD PTR DS:[<&KERNEL32.GlobalFree  KERNEL32.GlobalFree
002F897B   CALL DWORD PTR DS:[<&KERNEL32.GlobalLock  KERNEL32.GlobalLock
002F89EB   CALL DWORD PTR DS:[<&KERNEL32.GlobalUnlo  KERNEL32.GlobalUnlock
0030594F   CALL DWORD PTR DS:[<&KERNEL32.HeapFree>]  KERNEL32.HeapFree
00301C12   CALL DWORD PTR DS:[<&KERNEL32.Initialize  KERNELBA.InitializeCriticalSectionAndSpinCount
0030794D   CALL DWORD PTR DS:[<&KERNEL32.Initialize  KERNELBA.InitializeCriticalSectionAndSpinCount
002F0488   CALL DWORD PTR DS:[<&KERNEL32.IsDBCSLead  KERNEL32.IsDBCSLeadByte
002FD84A   CALL DWORD PTR DS:[<&KERNEL32.IsDebugger  KERNELBA.IsDebuggerPresent
00305BC2   CALL DWORD PTR DS:[<&KERNEL32.IsDebugger  KERNELBA.IsDebuggerPresent
002FD5E4   CALL <JMP.&KERNEL32.IsProcessorFeaturePr  KERNEL32.IsProcessorFeaturePresent
002FD782   CALL <JMP.&KERNEL32.IsProcessorFeaturePr  KERNEL32.IsProcessorFeaturePresent
002FDAF5   CALL <JMP.&KERNEL32.IsProcessorFeaturePr  KERNEL32.IsProcessorFeaturePresent
003059DD   CALL <JMP.&KERNEL32.IsProcessorFeaturePr  KERNEL32.IsProcessorFeaturePresent
00305CA6   CALL <JMP.&KERNEL32.IsProcessorFeaturePr  KERNEL32.IsProcessorFeaturePresent
0030D676   CALL <JMP.&KERNEL32.IsProcessorFeaturePr  KERNEL32.IsProcessorFeaturePresent
003085E7   CALL DWORD PTR DS:[<&KERNEL32.IsValidCod  KERNEL32.IsValidCodePage
003079D5   CALL DWORD PTR DS:[<&KERNEL32.LCMapStrin  KERNEL32.LCMapStringW
002FC8E1   CALL DWORD PTR DS:[<&KERNEL32.LoadLibrar  KERNEL32.LoadLibraryExA
00301AA1   CALL DWORD PTR DS:[<&KERNEL32.LoadLibrar  KERNEL32.LoadLibraryExW
00301ABB   CALL DWORD PTR DS:[<&KERNEL32.LoadLibrar  KERNEL32.LoadLibraryExW
0030775E   CALL DWORD PTR DS:[<&KERNEL32.LoadLibrar  KERNEL32.LoadLibraryExW
00307778   CALL DWORD PTR DS:[<&KERNEL32.LoadLibrar  KERNEL32.LoadLibraryExW
002EF0E2   CALL DWORD PTR DS:[<&KERNEL32.LoadLibrar  KERNEL32.LoadLibraryW
002F8945   CALL DWORD PTR DS:[<&KERNEL32.LoadResour  KERNEL32.LoadResource
002EFE6C   CALL DWORD PTR DS:[<&KERNEL32.LocalFileT  KERNELBA.LocalFileTimeToFileTime
002F8950   CALL DWORD PTR DS:[<&KERNEL32.LockResour  KERNEL32.LockResource
002F9CD4   CALL DWORD PTR DS:[<&KERNEL32.MapViewOfF  KERNEL32.MapViewOfFile
002FBEC1   CALL DWORD PTR DS:[<&KERNEL32.MapViewOfF  KERNEL32.MapViewOfFile
002FA9AC   CALL DWORD PTR DS:[<&KERNEL32.MoveFileEx  KERNEL32.MoveFileExW
002E9187   CALL EDI                                  KERNEL32.MoveFileW
002FA998   CALL DWORD PTR DS:[<&KERNEL32.MoveFileW>  KERNEL32.MoveFileW
002F0446   CALL DWORD PTR DS:[<&KERNEL32.MultiByteT  KERNEL32.MultiByteToWideChar
00305E88   CALL DWORD PTR DS:[<&KERNEL32.MultiByteT  KERNEL32.MultiByteToWideChar
00305EBE   CALL DWORD PTR DS:[<&KERNEL32.MultiByteT  KERNEL32.MultiByteToWideChar
00307363   CALL DWORD PTR DS:[<&KERNEL32.MultiByteT  KERNEL32.MultiByteToWideChar
003073E9   CALL DWORD PTR DS:[<&KERNEL32.MultiByteT  KERNEL32.MultiByteToWideChar
00309337   CALL DWORD PTR DS:[<&KERNEL32.MultiByteT  KERNEL32.MultiByteToWideChar
003093C0   CALL DWORD PTR DS:[<&KERNEL32.MultiByteT  KERNEL32.MultiByteToWideChar
002FD9B5   CALL DWORD PTR DS:[<&KERNEL32.QueryPerfo  KERNEL32.QueryPerformanceCounter
003032A6   CALL DWORD PTR DS:[<&KERNEL32.QueryPerfo  KERNEL32.QueryPerformanceCounter
00303298   CALL DWORD PTR DS:[<&KERNEL32.QueryPerfo  KERNEL32.QueryPerformanceFrequency
002FC855   CALL DWORD PTR DS:[<&KERNEL32.RaiseExcep  KERNELBA.RaiseException
002FC92D   CALL DWORD PTR DS:[<&KERNEL32.RaiseExcep  KERNELBA.RaiseException
002FC9FB   CALL DWORD PTR DS:[<&KERNEL32.RaiseExcep  KERNELBA.RaiseException
002FFE79   CALL DWORD PTR DS:[<&KERNEL32.RaiseExcep  KERNELBA.RaiseException
0030EA01   CALL DWORD PTR DS:[<&KERNEL32.RaiseExcep  KERNELBA.RaiseException
002E943D   CALL DWORD PTR DS:[<&KERNEL32.ReadFile>]  KERNELBA.ReadFile
002EF436   CALL DWORD PTR DS:[<&KERNEL32.ReadFile>]  KERNELBA.ReadFile
002EF7A1   CALL DWORD PTR DS:[<&KERNEL32.ReleaseSem  KERNELBA.ReleaseSemaphore
002EFB99   CALL DWORD PTR DS:[<&KERNEL32.ReleaseSem  KERNELBA.ReleaseSemaphore
002E71FE   CALL DWORD PTR DS:[<&KERNEL32.RemoveDire  KERNELBA.RemoveDirectoryW
002EFB85   CALL DWORD PTR DS:[<&KERNEL32.ResetEvent  KERNELBA.ResetEvent
003059A5   CALL DWORD PTR DS:[<&KERNEL32.HeapAlloc>  ntdll.RtlAllocateHeap
00305A45   CALL DWORD PTR DS:[<&KERNEL32.HeapAlloc>  ntdll.RtlAllocateHeap
0030DF26   CALL DWORD PTR DS:[<&KERNEL32.DecodePoin  ntdll.RtlDecodePointer
002EF7D4   CALL DWORD PTR DS:[<&KERNEL32.DeleteCrit  ntdll.RtlDeleteCriticalSection
003019B0   CALL DWORD PTR DS:[<&KERNEL32.DeleteCrit  ntdll.RtlDeleteCriticalSection
00306F4F   CALL DWORD PTR DS:[<&KERNEL32.DeleteCrit  ntdll.RtlDeleteCriticalSection
00307660   CALL DWORD PTR DS:[<&KERNEL32.DeleteCrit  ntdll.RtlDeleteCriticalSection
00308E71   CALL DWORD PTR DS:[<&KERNEL32.DeleteCrit  ntdll.RtlDeleteCriticalSection
0030B65C   CALL DWORD PTR DS:[<&KERNEL32.DeleteCrit  ntdll.RtlDeleteCriticalSection
0030F735   CALL DWORD PTR DS:[<&KERNEL32.DeleteCrit  ntdll.RtlDeleteCriticalSection
003011AD   CALL DWORD PTR DS:[<&KERNEL32.EncodePoin  ntdll.RtlEncodePointer
002EF8B1   CALL DWORD PTR DS:[<&KERNEL32.EnterCriti  ntdll.RtlEnterCriticalSection
002EF9DE   CALL DWORD PTR DS:[<&KERNEL32.EnterCriti  ntdll.RtlEnterCriticalSection
002EFABC   CALL DWORD PTR DS:[<&KERNEL32.EnterCriti  ntdll.RtlEnterCriticalSection
002EFB2E   CALL DWORD PTR DS:[<&KERNEL32.EnterCriti  ntdll.RtlEnterCriticalSection
00306F7E   CALL DWORD PTR DS:[<&KERNEL32.EnterCriti  ntdll.RtlEnterCriticalSection
00307640   CALL DWORD PTR DS:[<&KERNEL32.EnterCriti  ntdll.RtlEnterCriticalSection
00308F3D   CALL DWORD PTR DS:[<&KERNEL32.EnterCriti  ntdll.RtlEnterCriticalSection
002E1046   CALL DWORD PTR DS:[<&KERNEL32.Initialize  ntdll.RtlInitializeCriticalSection
002EF705   CALL DWORD PTR DS:[<&KERNEL32.Initialize  ntdll.RtlInitializeCriticalSection
002FDA06   CALL DWORD PTR DS:[<&KERNEL32.Initialize  ntdll.RtlInitializeSListHead
002EF927   CALL DWORD PTR DS:[<&KERNEL32.LeaveCriti  ntdll.RtlLeaveCriticalSection
002EF932   CALL DWORD PTR DS:[<&KERNEL32.LeaveCriti  ntdll.RtlLeaveCriticalSection
002EFA35   CALL DWORD PTR DS:[<&KERNEL32.LeaveCriti  ntdll.RtlLeaveCriticalSection
002EFAF0   CALL DWORD PTR DS:[<&KERNEL32.LeaveCriti  ntdll.RtlLeaveCriticalSection
002EFB4A   CALL DWORD PTR DS:[<&KERNEL32.LeaveCriti  ntdll.RtlLeaveCriticalSection
00306F92   CALL DWORD PTR DS:[<&KERNEL32.LeaveCriti  ntdll.RtlLeaveCriticalSection
00307688   CALL DWORD PTR DS:[<&KERNEL32.LeaveCriti  ntdll.RtlLeaveCriticalSection
00308F60   CALL DWORD PTR DS:[<&KERNEL32.LeaveCriti  ntdll.RtlLeaveCriticalSection
00305ABE   CALL DWORD PTR DS:[<&KERNEL32.HeapReAllo  ntdll.RtlReAllocateHeap
002F0287   CALL DWORD PTR DS:[<&KERNEL32.SetLastErr  ntdll.RtlSetLastWin32Error
002F9856   CALL DWORD PTR DS:[<&KERNEL32.SetLastErr  ntdll.RtlSetLastWin32Error
003009EB   CALL DWORD PTR DS:[<&KERNEL32.SetLastErr  ntdll.RtlSetLastWin32Error
003062E6   CALL DWORD PTR DS:[<&KERNEL32.SetLastErr  ntdll.RtlSetLastWin32Error
003062F2   CALL DWORD PTR DS:[<&KERNEL32.SetLastErr  ntdll.RtlSetLastWin32Error
0030636C   CALL DWORD PTR DS:[<&KERNEL32.SetLastErr  ntdll.RtlSetLastWin32Error
00306375   CALL DWORD PTR DS:[<&KERNEL32.SetLastErr  ntdll.RtlSetLastWin32Error
0030C01E   CALL DWORD PTR DS:[<&KERNEL32.HeapSize>]  ntdll.RtlSizeHeap
002FE4CF   CALL DWORD PTR DS:[<&KERNEL32.RtlUnwind>  ntdll.RtlUnwind
0030193D   CALL <JMP.&KERNEL32.RtlUnwind>            KERNEL32.RtlUnwind
00301CC3   CALL <JMP.&KERNEL32.RtlUnwind>            KERNEL32.RtlUnwind
002F8D60   CALL DWORD PTR DS:[<&KERNEL32.SetCurrent  KERNEL32.SetCurrentDirectoryW
002E996F   CALL DWORD PTR DS:[<&KERNEL32.SetEndOfFi  KERNELBA.SetEndOfFile
002FBB3F   CALL DWORD PTR DS:[<&KERNEL32.SetEnviron  KERNEL32.SetEnvironmentVariableW
002FBB7B   CALL DWORD PTR DS:[<&KERNEL32.SetEnviron  KERNEL32.SetEnvironmentVariableW
002FBF31   CALL ESI                                  KERNEL32.SetEnvironmentVariableW
002EFB43   CALL DWORD PTR DS:[<&KERNEL32.SetEvent>]  KERNELBA.SetEvent
002E9ED9   CALL DWORD PTR DS:[<&KERNEL32.SetFileAtt  KERNELBA.SetFileAttributesW
002E9F0A   CALL DWORD PTR DS:[<&KERNEL32.SetFileAtt  KERNELBA.SetFileAttributesW
002FA883   CALL DWORD PTR DS:[<&KERNEL32.SetFileAtt  KERNELBA.SetFileAttributesW
002E96ED   CALL DWORD PTR DS:[<&KERNEL32.SetFilePoi  KERNELBA.SetFilePointer
002E9923   CALL DWORD PTR DS:[<&KERNEL32.SetFilePoi  KERNELBA.SetFilePointer
002EF417   CALL DWORD PTR DS:[<&KERNEL32.SetFilePoi  KERNELBA.SetFilePointer
0030CD2D   CALL DWORD PTR DS:[<&KERNEL32.SetFilePoi  KERNELBA.SetFilePointerEx
002E7599   CALL DWORD PTR DS:[<&KERNEL32.SetFileTim  KERNELBA.SetFileTime
002E98DE   CALL DWORD PTR DS:[<&KERNEL32.SetFileTim  KERNELBA.SetFileTime
002E9E97   CALL DWORD PTR DS:[<&KERNEL32.SetFileTim  KERNELBA.SetFileTime
00308FC8   CALL DWORD PTR DS:[<&KERNEL32.SetStdHand  KERNEL32.SetStdHandle
002EF6C3   CALL DWORD PTR DS:[<&KERNEL32.SetThreadE  KERNEL32.SetThreadExecutionState
002EF9B8   CALL DWORD PTR DS:[<&KERNEL32.SetThreadP  KERNEL32.SetThreadPriority
002FD869   CALL DWORD PTR DS:[<&KERNEL32.SetUnhandl  KERNEL32.SetUnhandledExceptionFilter
002FD90C   CALL DWORD PTR DS:[<&KERNEL32.SetUnhandl  KERNEL32.SetUnhandledExceptionFilter
002FDAC7   CALL DWORD PTR DS:[<&KERNEL32.SetUnhandl  KERNEL32.SetUnhandledExceptionFilter
00305BCC   CALL DWORD PTR DS:[<&KERNEL32.SetUnhandl  KERNEL32.SetUnhandledExceptionFilter
002F8932   CALL DWORD PTR DS:[<&KERNEL32.SizeofReso  KERNEL32.SizeofResource
002EF66D   CALL DWORD PTR DS:[<&KERNEL32.Sleep>]     KERNELBA.Sleep
002F9D46   CALL DWORD PTR DS:[<&KERNEL32.Sleep>]     KERNELBA.Sleep
002FC01D   CALL DWORD PTR DS:[<&KERNEL32.Sleep>]     KERNELBA.Sleep
002EFC2D   CALL ESI                                  KERNEL32.SystemTimeToFileTime
002EFC39   CALL ESI                                  KERNEL32.SystemTimeToFileTime
002EFD6F   CALL DWORD PTR DS:[<&KERNEL32.SystemTime  KERNEL32.SystemTimeToFileTime
002EFE4A   CALL EBP                                  KERNEL32.SystemTimeToFileTime
002EFC17   CALL DWORD PTR DS:[<&KERNEL32.SystemTime  KERNEL32.SystemTimeToTzSpecificLocalTime
002FDAE2   CALL DWORD PTR DS:[<&KERNEL32.TerminateP  KERNEL32.TerminateProcess
00304A70   CALL DWORD PTR DS:[<&KERNEL32.TerminateP  KERNEL32.TerminateProcess
00305CCF   CALL DWORD PTR DS:[<&KERNEL32.TerminateP  KERNEL32.TerminateProcess
003077E6   CALL DWORD PTR DS:[<&KERNEL32.TlsAlloc>]  KERNELBA.TlsAlloc
00301B55   CALL DWORD PTR DS:[<&KERNEL32.TlsFree>]   KERNEL32.TlsFree
0030783C   CALL DWORD PTR DS:[<&KERNEL32.TlsFree>]   KERNEL32.TlsFree
00301B8F   CALL DWORD PTR DS:[<&KERNEL32.TlsGetValu  KERNEL32.TlsGetValue
00307892   CALL DWORD PTR DS:[<&KERNEL32.TlsGetValu  KERNEL32.TlsGetValue
00301BCC   CALL DWORD PTR DS:[<&KERNEL32.TlsSetValu  KERNEL32.TlsSetValue
003078EB   CALL DWORD PTR DS:[<&KERNEL32.TlsSetValu  KERNEL32.TlsSetValue
002EFE97   CALL DWORD PTR DS:[<&KERNEL32.TzSpecific  KERNEL32.TzSpecificLocalTimeToSystemTime
002FD873   CALL DWORD PTR DS:[<&KERNEL32.UnhandledE  KERNEL32.UnhandledExceptionFilter
002FDAD0   CALL DWORD PTR DS:[<&KERNEL32.UnhandledE  KERNEL32.UnhandledExceptionFilter
00305BD9   CALL DWORD PTR DS:[<&KERNEL32.UnhandledE  KERNEL32.UnhandledExceptionFilter
002F9D6F   CALL DWORD PTR DS:[<&KERNEL32.UnmapViewO  KERNEL32.UnmapViewOfFile
002FBEFB   CALL DWORD PTR DS:[<&KERNEL32.UnmapViewO  KERNEL32.UnmapViewOfFile
002FC72F   CALL DWORD PTR DS:[<&KERNEL32.VirtualPro  KERNEL32.VirtualProtect
002FC5F1   CALL DWORD PTR DS:[<&KERNEL32.VirtualQue  KERNEL32.VirtualQuery
002EF869   CALL DWORD PTR DS:[<&KERNEL32.WaitForSin  KERNELBA.WaitForSingleObject
002FBB94   CALL DWORD PTR DS:[<&KERNEL32.WaitForSin  KERNELBA.WaitForSingleObject
002FBBB8   CALL DWORD PTR DS:[<&KERNEL32.WaitForSin  KERNELBA.WaitForSingleObject
002F0663   CALL DWORD PTR DS:[<&KERNEL32.WideCharTo  KERNEL32.WideCharToMultiByte
002F7D14   CALL DWORD PTR DS:[<&KERNEL32.WideCharTo  KERNEL32.WideCharToMultiByte
003074E3   CALL DWORD PTR DS:[<&KERNEL32.WideCharTo  KERNEL32.WideCharToMultiByte
003088D2   CALL DWORD PTR DS:[<&KERNEL32.WideCharTo  KERNEL32.WideCharToMultiByte
003088F8   CALL DWORD PTR DS:[<&KERNEL32.WideCharTo  KERNEL32.WideCharToMultiByte
0030C56B   CALL DWORD PTR DS:[<&KERNEL32.WideCharTo  KERNEL32.WideCharToMultiByte
0030C983   CALL DWORD PTR DS:[<&KERNEL32.WideCharTo  KERNEL32.WideCharToMultiByte
002EF662   CALL DWORD PTR DS:[<&KERNEL32.WriteConso  KERNELBA.WriteConsoleW
0030E207   CALL DWORD PTR DS:[<&KERNEL32.WriteConso  KERNELBA.WriteConsoleW
002E9A25   CALL DWORD PTR DS:[<&KERNEL32.WriteFile>  KERNELBA.WriteFile
002E9A52   CALL DWORD PTR DS:[<&KERNEL32.WriteFile>  KERNELBA.WriteFile
0030C58A   CALL DWORD PTR DS:[<&KERNEL32.WriteFile>  KERNELBA.WriteFile
0030C5C3   CALL DWORD PTR DS:[<&KERNEL32.WriteFile>  KERNELBA.WriteFile
0030C79E   CALL DWORD PTR DS:[<&KERNEL32.WriteFile>  KERNELBA.WriteFile
0030C88C   CALL DWORD PTR DS:[<&KERNEL32.WriteFile>  KERNELBA.WriteFile
0030C9B1   CALL DWORD PTR DS:[<&KERNEL32.WriteFile>  KERNELBA.WriteFile
0030CC5A   CALL DWORD PTR DS:[<&KERNEL32.WriteFile>  KERNELBA.WriteFile
