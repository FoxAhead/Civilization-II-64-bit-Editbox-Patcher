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
  TVersion = (verMGE, verTOT);
  TApplicationType = (atGame, atEditor);
  TPatchVersion = (pvNone, pvMastermind, pvThisPatch);
  TPattern = record
    PatternResourceName: string;
    ReplaceResourceName: string;
    Version: TVersion;
    ApplicationType: TApplicationType;
    PatchVersion: TPatchVersion;
  end;
  TSearchInfo = record
    PatternIndex: Integer;
    FileOffset: Integer;
    Matches: Integer;
  end;
const
  VersionName: array[TVersion] of string =
    (
    'Civilization II Multiplayer Gold Edition',
    'Civilization II: Test of Time'
    );
const
  Patterns: array[0..8] of TPattern =
    (
    // MGE - Game
    (
    PatternResourceName: 'MGEGameFA';
    ReplaceResourceName: '';
    Version: verMGE;
    ApplicationType: atGame;
    PatchVersion: pvThisPatch;
    ),
    (
    PatternResourceName: 'MGEGame';
    ReplaceResourceName: 'MGEGameFA';
    Version: verMGE;
    ApplicationType: atGame;
    PatchVersion: pvNone;
    ),
    (
    PatternResourceName: 'MGEGameMM';
    ReplaceResourceName: 'MGEGameFA';
    Version: verMGE;
    ApplicationType: atGame;
    PatchVersion: pvMastermind;
    ),
    // MGE - Editor
    (
    PatternResourceName: 'MGEEditFA';
    ReplaceResourceName: '';
    Version: verMGE;
    ApplicationType: atEditor;
    PatchVersion: pvThisPatch;
    ),
    (
    PatternResourceName: 'MGEEdit';
    ReplaceResourceName: 'MGEEditFA';
    Version: verMGE;
    ApplicationType: atEditor;
    PatchVersion: pvNone;
    ),
    (
    PatternResourceName: 'MGEEditMM';
    ReplaceResourceName: 'MGEEditFA';
    Version: verMGE;
    ApplicationType: atEditor;
    PatchVersion: pvMastermind;
    ),
    // TOT - Game
    (
    PatternResourceName: 'TOTGameFA';
    ReplaceResourceName: '';
    Version: verTOT;
    ApplicationType: atGame;
    PatchVersion: pvThisPatch;
    ),
    (
    PatternResourceName: 'TOTGame';
    ReplaceResourceName: 'TOTGameFA';
    Version: verTOT;
    ApplicationType: atGame;
    PatchVersion: pvNone;
    ),
    (
    PatternResourceName: 'TOTGameMM';
    ReplaceResourceName: 'TOTGameFA';
    Version: verTOT;
    ApplicationType: atGame;
    PatchVersion: pvMastermind;
    )
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
