unit xrtl_crypt_DDP;

{$INCLUDE xrtl.inc}

interface

function  XRTLDDPR3Fixed(Data: Cardinal): Cardinal; register;
function  XRTLDDPR3Layer(Data, V: Cardinal): Cardinal; register;
function  XRTLDDPR3Direct(Data, V1, V2, V3: Cardinal): Cardinal;
function  XRTLDDPR3Reverse(Data, V1, V2, V3: Cardinal): Cardinal;
function  XRTLDDP3296ExpandLayerKey(V: Cardinal): Cardinal; register;
function  XRTLDDP3296Fixed(Data: Cardinal): Cardinal; register;
function  XRTLDDP3296Direct(Data: Cardinal; V1, V2, V3, V4, V5, V6: Word): Cardinal;
function  XRTLDDP3296Reverse(Data: Cardinal; V1, V2, V3, V4, V5, V6: Word): Cardinal;
function  XRTLDDP3296DirectExpandedKeys(Data, V1, V2, V3, V4, V5, V6: Cardinal): Cardinal;
function  XRTLDDP3296ReverseExpandedKeys(Data, V1, V2, V3, V4, V5, V6: Cardinal): Cardinal;

implementation

uses
  xrtl_util_CPUUtils;

function  XRTLDDPR3Fixed(Data: Cardinal): Cardinal; register;
asm
// EAX = Data
        MOV     ECX,EAX
        MOV     EDX,EAX
        AND     EAX,$99999999
// Swap bits (Ds): bit transform xABxxCDx -> xBAxxDCx
        AND     ECX,$44444444
        SHR     ECX,1
        OR      EAX,ECX
        AND     EDX,$22222222
        SHL     EDX,1
        OR      EAX,EDX
end;

function  XRTLDDPR3Layer(Data, V: Cardinal): Cardinal; register;
asm
// EAX = Data
// EDX = V
        PUSH    EBX
// ExpandV (Vx): bit transform 0000abcd -> aabbccdd
        XOR     ECX,ECX
        MOV     EBX,EDX
        AND     EBX,$01010101
        OR      ECX,EBX
        MOV     EBX,EDX
        AND     EBX,$02020202
        SHL     EBX,1
        OR      ECX,EBX
        MOV     EBX,EDX
        AND     EBX,$04040404
        SHL     EBX,2
        OR      ECX,EBX
        MOV     EBX,EDX
        AND     EBX,$08080808
        SHL     EBX,3
        OR      ECX,EBX
        MOV     EBX,ECX
        SHL     EBX,1
        OR      ECX,EBX
// ECX = Vx
// Swap bits (Ds): bit transform abcdefgh -> badcfehg
        MOV     EBX,EAX
        MOV     EDX,EAX
        AND     EBX,$AAAAAAAA
        SHR     EBX,1
        AND     EDX,$55555555
        SHL     EDX,1
        OR      EBX,EDX
// Result:= (Data and not Vx) or (Ds and Vx);
        AND     EBX,ECX
        NOT     ECX
        AND     EAX,ECX
        OR      EAX,EBX
        POP     EBX
end;

function  XRTLDDPR3DirectFixed(Data: Cardinal): Cardinal; register;
asm
// EAX = Data
        MOV     ECX,EAX
        MOV     EDX,EAX
        AND     EAX,$99999999
// Swap bits (Ds): bit transform xABxxCDx -> xBAxxDCx
        AND     ECX,$44444444
        SHR     ECX,1
        OR      EAX,ECX
        AND     EDX,$22222222
        SHL     EDX,1
        OR      EAX,EDX
        MOV     ECX,EAX
        MOV     EDX,EAX
        AND     EAX,$C3C3C3C3
// Swap bits (Ds): bit transform xxABCDxx -> xxCDABxx
        AND     ECX,$30303030
        SHR     ECX,2
        OR      EAX,ECX
        AND     EDX,$0C0C0C0C
        SHL     EDX,2
        OR      EAX,EDX
end;

function  XRTLDDPR3Direct(Data, V1, V2, V3: Cardinal): Cardinal;
begin
// 1st layer permutation
  Result:= XRTLDDPR3Layer(Data, V1);
// 1st fixed permutation
  Result:= XRTLDDPR3DirectFixed(Result);
// 2nd layer permutation
  Result:= XRTLDDPR3Layer(Result, V2);
// 2nd fixed permutation
  Result:= XRTLDDPR3Fixed(Result);
// 3rd layer permutation
  Result:= XRTLDDPR3Layer(Result, V3);
end;

function  XRTLDDPR3ReverseFixed(Data: Cardinal): Cardinal; register;
asm
// EAX = Data
        MOV     ECX,EAX
        MOV     EDX,EAX
        AND     EAX,$99999999
// Swap bits (Ds): bit transform xABxxCDx -> xBAxxDCx
        AND     ECX,$44444444
        SHR     ECX,1
        OR      EAX,ECX
        AND     EDX,$22222222
        SHL     EDX,1
        OR      EAX,EDX
        MOV     ECX,EAX
        MOV     EDX,EAX
        AND     EAX,$A5A5A5A5
// Swap bits (Ds): bit transform xAxBCxDx -> xCxDAxBx
        AND     ECX,$50505050
        SHR     ECX,3
        OR      EAX,ECX
        AND     EDX,$0A0A0A0A
        SHL     EDX,3
        OR      EAX,EDX
end;

function  XRTLDDPR3Reverse(Data, V1, V2, V3: Cardinal): Cardinal;

begin
// 1st layer permutation
  Result:= XRTLDDPR3Layer(Data, V3);
// 1st fixed permutation
  Result:= XRTLDDPR3Fixed(Result);
// 2nd layer permutation
  Result:= XRTLDDPR3Layer(Result, V2);
// 2nd fixed permutation
  Result:= XRTLDDPR3ReverseFixed(Result);
// 3rd layer permutation
  Result:= XRTLDDPR3Layer(Result, V1);
end;

function  XRTLDDP3296Fixed(Data: Cardinal): Cardinal; register;
begin
  Result:= XRTLSwapBits(Data,    1,  8);
  Result:= XRTLSwapBits(Result,  2, 16);
  Result:= XRTLSwapBits(Result,  3, 24);
  Result:= XRTLSwapBits(Result,  5, 12);
  Result:= XRTLSwapBits(Result,  6, 20);
  Result:= XRTLSwapBits(Result,  7, 28);
  Result:= XRTLSwapBits(Result, 10, 17);
  Result:= XRTLSwapBits(Result, 11, 25);
  Result:= XRTLSwapBits(Result, 14, 21);
  Result:= XRTLSwapBits(Result, 15, 29);
  Result:= XRTLSwapBits(Result, 19, 26);
  Result:= XRTLSwapBits(Result, 23, 30);
end;

function  XRTLDDP3296ExpandLayerKey(V: Cardinal): Cardinal; register;
asm
// EAX = V
// 0000abcd -> 0a0b0c0d 
        MOV     EDX,EAX
        AND     EAX,$000F
        MOV     ECX,EDX
        AND     ECX,$00F0
        SHL     ECX,4
        OR      EAX,ECX
        MOV     ECX,EDX
        AND     ECX,$0F00
        SHL     ECX,8
        OR      EAX,ECX
        MOV     ECX,EDX
        AND     ECX,$F000
        SHL     ECX,12
        OR      EAX,ECX
end;

function  XRTLDDP3296Direct(Data: Cardinal; V1, V2, V3, V4, V5, V6: Word): Cardinal;
begin
// 1st layer permutation (R3)
// R3 *******************
// 1st layer permutation
  Result:= XRTLDDPR3Layer(Data, XRTLDDP3296ExpandLayerKey(V1));
// 1st fixed permutation
  Result:= XRTLDDPR3DirectFixed(Result);
// 2nd layer permutation
  Result:= XRTLDDPR3Layer(Result, XRTLDDP3296ExpandLayerKey(V2));
// 2nd fixed permutation
  Result:= XRTLDDPR3Fixed(Result);
// 3rd layer permutation
  Result:= XRTLDDPR3Layer(Result, XRTLDDP3296ExpandLayerKey(V3));
// R3 *******************
  Result:= XRTLDDP3296Fixed(Result);
// 2nd layer permutation (R3)
// R3 *******************
// 1st layer permutation
  Result:= XRTLDDPR3Layer(Result, XRTLDDP3296ExpandLayerKey(V4));
// 1st fixed permutation
  Result:= XRTLDDPR3DirectFixed(Result);
// 2nd layer permutation
  Result:= XRTLDDPR3Layer(Result, XRTLDDP3296ExpandLayerKey(V5));
// 2nd fixed permutation
  Result:= XRTLDDPR3Fixed(Result);
// 3rd layer permutation
  Result:= XRTLDDPR3Layer(Result, XRTLDDP3296ExpandLayerKey(V6));
// R3 *******************
end;

function  XRTLDDP3296Reverse(Data: Cardinal; V1, V2, V3, V4, V5, V6: Word): Cardinal;
begin
// 1st layer permutation (R3)
// R3 *******************
// 1st layer permutation
  Result:= XRTLDDPR3Layer(Data, XRTLDDP3296ExpandLayerKey(V6));
// 1st fixed permutation
  Result:= XRTLDDPR3Fixed(Result);
// 2nd layer permutation
  Result:= XRTLDDPR3Layer(Result, XRTLDDP3296ExpandLayerKey(V5));
// 2nd fixed permutation
  Result:= XRTLDDPR3ReverseFixed(Result);
// 3rd layer permutation
  Result:= XRTLDDPR3Layer(Result, XRTLDDP3296ExpandLayerKey(V4));
// R3 *******************
  Result:= XRTLDDP3296Fixed(Result);
// 2nd layer permutation (R3)
// R3 *******************
// 1st layer permutation
  Result:= XRTLDDPR3Layer(Result, XRTLDDP3296ExpandLayerKey(V3));
// 1st fixed permutation
  Result:= XRTLDDPR3Fixed(Result);
// 2nd layer permutation
  Result:= XRTLDDPR3Layer(Result, XRTLDDP3296ExpandLayerKey(V2));
// 2nd fixed permutation
  Result:= XRTLDDPR3ReverseFixed(Result);
// 3rd layer permutation
  Result:= XRTLDDPR3Layer(Result, XRTLDDP3296ExpandLayerKey(V1));
// R3 *******************
end;

function  XRTLDDP3296DirectExpandedKeys(Data, V1, V2, V3, V4, V5, V6: Cardinal): Cardinal;
begin
// 1st layer permutation (R3)
// R3 *******************
// 1st layer permutation
  Result:= XRTLDDPR3Layer(Data, V1);
// 1st fixed permutation
  Result:= XRTLDDPR3DirectFixed(Result);
// 2nd layer permutation
  Result:= XRTLDDPR3Layer(Result, V2);
// 2nd fixed permutation
  Result:= XRTLDDPR3Fixed(Result);
// 3rd layer permutation
  Result:= XRTLDDPR3Layer(Result, V3);
// R3 *******************
  Result:= XRTLDDP3296Fixed(Result);
// 2nd layer permutation (R3)
// R3 *******************
// 1st layer permutation
  Result:= XRTLDDPR3Layer(Result, V4);
// 1st fixed permutation
  Result:= XRTLDDPR3DirectFixed(Result);
// 2nd layer permutation
  Result:= XRTLDDPR3Layer(Result, V5);
// 2nd fixed permutation
  Result:= XRTLDDPR3Fixed(Result);
// 3rd layer permutation
  Result:= XRTLDDPR3Layer(Result, V6);
// R3 *******************
end;

function  XRTLDDP3296ReverseExpandedKeys(Data, V1, V2, V3, V4, V5, V6: Cardinal): Cardinal;
begin
// 1st layer permutation (R3)
// R3 *******************
// 1st layer permutation
  Result:= XRTLDDPR3Layer(Data, V6);
// 1st fixed permutation
  Result:= XRTLDDPR3Fixed(Result);
// 2nd layer permutation
  Result:= XRTLDDPR3Layer(Result, V5);
// 2nd fixed permutation
  Result:= XRTLDDPR3ReverseFixed(Result);
// 3rd layer permutation
  Result:= XRTLDDPR3Layer(Result, V4);
// R3 *******************
  Result:= XRTLDDP3296Fixed(Result);
// 2nd layer permutation (R3)
// R3 *******************
// 1st layer permutation
  Result:= XRTLDDPR3Layer(Result, V3);
// 1st fixed permutation
  Result:= XRTLDDPR3Fixed(Result);
// 2nd layer permutation
  Result:= XRTLDDPR3Layer(Result, V2);
// 2nd fixed permutation
  Result:= XRTLDDPR3ReverseFixed(Result);
// 3rd layer permutation
  Result:= XRTLDDPR3Layer(Result, V1);
// R3 *******************
end;

end.
