0030C6E6  |. 50             PUSH EAX                                 ; /pMode
0030C6E7  |. 8B04BD 109F330>MOV EAX,DWORD PTR DS:[EDI*4+339F10]      ; |
0030C6EE  |. FF7430 18      PUSH DWORD PTR DS:[EAX+ESI+18]           ; |hConsole
0030C6F2  |. FF15 7C013100  CALL DWORD PTR DS:[<&KERNEL32.GetConsole>; \GetConsoleMode
0030C6F8  |. 85C0           TEST EAX,EAX
0030C6FA  |. 0F95C0         SETNE AL
0030C6FD  |> 5F             POP EDI
0030C6FE  |> 5E             POP ESI
0030C6FF  |. 8BE5           MOV ESP,EBP
0030C701  |. 5D             POP EBP
0030C702  \. C3             RETN
0030C703  /$ 8BFF           MOV EDI,EDI
0030C705  |. 55             PUSH EBP
0030C706  |. 8BEC           MOV EBP,ESP
0030C708  |. B8 10140000    MOV EAX,1410
0030C70D  |. E8 BE04FFFF    CALL PentestB.002FCBD0
0030C712  |. A1 C8A13100    MOV EAX,DWORD PTR DS:[31A1C8]
0030C717  |. 33C5           XOR EAX,EBP
0030C719  |. 8945 FC        MOV DWORD PTR SS:[EBP-4],EAX
0030C71C  |. 8B4D 0C        MOV ECX,DWORD PTR SS:[EBP+C]
0030C71F  |. 8BC1           MOV EAX,ECX
0030C721  |. C1F8 06        SAR EAX,6
0030C724  |. 83E1 3F        AND ECX,3F
0030C727  |. 6BC9 30        IMUL ECX,ECX,30
0030C72A  |. 53             PUSH EBX
0030C72B  |. 8B5D 10        MOV EBX,DWORD PTR SS:[EBP+10]
0030C72E  |. 8B0485 109F330>MOV EAX,DWORD PTR DS:[EAX*4+339F10]
0030C735  |. 56             PUSH ESI
0030C736  |. 8B75 08        MOV ESI,DWORD PTR SS:[EBP+8]
0030C739  |. 57             PUSH EDI
0030C73A  |. 8B4C08 18      MOV ECX,DWORD PTR DS:[EAX+ECX+18]
0030C73E  |. 8B45 14        MOV EAX,DWORD PTR SS:[EBP+14]
0030C741  |. 8326 00        AND DWORD PTR DS:[ESI],0
0030C744  |. 03C3           ADD EAX,EBX
0030C746  |. 8366 04 00     AND DWORD PTR DS:[ESI+4],0
0030C74A  |. 8366 08 00     AND DWORD PTR DS:[ESI+8],0
0030C74E  |. 898D F0EBFFFF  MOV DWORD PTR SS:[EBP-1410],ECX
0030C754  |. 8985 F8EBFFFF  MOV DWORD PTR SS:[EBP-1408],EAX
