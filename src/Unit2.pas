unit Unit2;

interface

type
  TLib = class
  public
    class function CurrentFileInfo(NameApp: string): string;
  end;

var
  DEBUG_LEVEL: Integer = 0;

type
  TApplicationType = (atGame, atEditor);
  TPatchVersion = (pvNone, pvMastermind, pvThisPatch);
  TProc = (prGetClass = $01, prGetWindow = $02);
  TSearchDirection = (sdDown, sdUp);
  TCall = record
    Offset: Integer;
    Proc: TProc;
    Address: Integer;
  end;
  TCallDynArray = array of TCall;
  TSearchInfo = record
    PatchVersion: TPatchVersion;
    FileOffset: Integer;
    Calls: TCallDynArray;
  end;
  TSearchInfoDynArray = array of TSearchInfo;

const
  Patterns: array[TPatchVersion] of string =
    (
    'Civ2', //pvNone
    'Civ2MM', //pvMastermind
    'Civ2FA' //pvThisPatch
    );

implementation

uses Windows, SysUtils;

class function TLib.CurrentFileInfo(NameApp: string): string;
var
  dump: DWORD;
  size: Integer;
  buffer: PChar;
  VersionPointer, TransBuffer: PChar;
  Temp: Integer;
  CalcLangCharSet: string;
begin
  size := GetFileVersionInfoSize(PChar(NameApp), dump);
  buffer := StrAlloc(size + 1);
  try
    GetFileVersionInfo(PChar(NameApp), 0, size, buffer);

    VerQueryValue(buffer, '\VarFileInfo\Translation', pointer(TransBuffer),
      dump);
    if dump >= 4 then
    begin
      Temp := 0;
      StrLCopy(@Temp, TransBuffer, 2);
      CalcLangCharSet := IntToHex(Temp, 4);
      StrLCopy(@Temp, TransBuffer + 2, 2);
      CalcLangCharSet := CalcLangCharSet + IntToHex(Temp, 4);
    end;

    VerQueryValue(buffer, PChar('\StringFileInfo\' + CalcLangCharSet +
      '\' + 'FileVersion'), pointer(VersionPointer), dump);
    if (dump > 1) then
    begin
      SetLength(Result, dump);
      StrLCopy(PChar(Result), VersionPointer, dump);
    end
    else
      Result := '0.0.0.0';
  finally
    StrDispose(buffer);
  end;
end;

end.
