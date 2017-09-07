unit Unit1;

interface

uses
  Classes,
  Controls,
  Dialogs,
  Forms,
  ShellAPI,
  StdCtrls,
  SysUtils,
  Types,
  Unit2,
  Windows, Math;

type
  TForm1 = class(TForm)
    ButtonPatch: TButton;
    EditFileName: TEdit;
    ButtonBrowse: TButton;
    OpenDialog1: TOpenDialog;
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelVersion: TLabel;
    procedure ButtonBrowseClick(Sender: TObject);
    procedure ButtonPatchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OpenGitHubLink(Sender: TObject);
  private
    { Private declarations }
    function CheckFile(FileName: string): Boolean;
    function PatchFile(FileName: string): Boolean;
    function IsReadyToPatch: Boolean;
    procedure SetReadyToPatch(ready: Boolean);
    procedure LoadFromFile(FileName: string; var FileBuffer: TByteDynArray);
    procedure LoadFromResource(ResourceName: string; var ResourceBuffer: TByteDynArray);
    procedure Log(Text: string = ''); overload;
    procedure Log(Level: Integer; Text: string); overload;
    function SearchForPatterns(var FileBuffer: TByteDynArray): TSearchInfoDynArray;
    function BackupFile(FileName: string): string;
    procedure MaskCalls(var PatternBuffer, PatternMask: TByteDynArray; var Calls: TCallDynArray);
    function GetVersionName(var FileBuffer: TByteDynArray): string;
    function GetApplicationType(var FileBuffer: TByteDynArray): TApplicationType;
    function FindPos(var Where: array of Byte; What: array of Byte; Offset: Integer = 0; SearchDirection: TSearchDirection = sdDown): Integer;
    procedure SetCallAddresses(var PatternBuffer: TByteDynArray; var Calls: TCallDynArray);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ButtonBrowseClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    EditFileName.Text := OpenDialog1.FileName;
    Log;
    SetReadyToPatch(CheckFile(EditFileName.Text));
    if IsReadyToPatch() then
    begin
      Log('READY TO PATCH!');
      Log('You can click ''Patch!'' now (backup file will be created).')
    end
    else
    begin
      Log('CAN NOT PATCH!');
    end;
  end;
end;

function TForm1.CheckFile(FileName: string): Boolean;
var
  FileBuffer: TByteDynArray;
  SearchInfo: TSearchInfoDynArray;
begin
  Result := False;
  try
    Log(1, 'Analyzing file ' + FileName + '...');
    LoadFromFile(FileName, FileBuffer);
    SearchInfo := SearchForPatterns(FileBuffer);
    Log(1, 'Analyzing done.');
    if (Length(SearchInfo) <> 1) then
    begin
      Log('Wrong file.');
      Result := False;
    end
    else
    begin

      Log('Version ''' + GetVersionName(FileBuffer) + ''' detected.');

      if GetApplicationType(FileBuffer) = atEditor then
        Log('It is Map Editor.');
      if SearchInfo[0].PatchVersion = pvMastermind then
        Log('Mastermind patch detected. Will be replaced.');
      if SearchInfo[0].PatchVersion = pvThisPatch then
      begin
        Log('The file is already patched.');
        Result := False;
      end
      else
      begin
        Result := True;
      end;
    end;
  except
    on E: EStreamError do
    begin
      Log('File read error!');
      Log(E.Message);
    end;
    on E: EResNotFound do
    begin
      Log('Resource read error!');
      Log(E.Message);
    end;
  end;
end;

function TForm1.IsReadyToPatch: Boolean;
begin
  Result := ButtonPatch.Enabled;
end;

procedure TForm1.LoadFromFile(FileName: string; var FileBuffer: TByteDynArray);
var
  FileStream: TFileStream;
  BytesRead: Integer;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  Log(2, 'Allocating ' + IntToStr(FileStream.Size) + ' bytes...');
  SetLength(FileBuffer, FileStream.Size);
  Log(2, 'Allocated.');
  Log(2, 'Reading...');
  BytesRead := FileStream.Read(Pointer(FileBuffer)^, FileStream.Size);
  FileStream.Free;
  Log(2, 'Reading of ' + IntToStr(BytesRead) + ' bytes done.');
end;

procedure TForm1.LoadFromResource(ResourceName: string; var ResourceBuffer: TByteDynArray);
var
  ResourceStream: TResourceStream;
  SomeStream: TStream;
  BytesRead: Integer;
begin
  ResourceStream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
  Log(2, 'Allocating ' + IntToStr(ResourceStream.Size) + ' bytes...');
  SetLength(ResourceBuffer, ResourceStream.Size);
  Log(2, 'Allocated.');
  Log(2, 'Reading...');
  BytesRead := ResourceStream.Read(Pointer(ResourceBuffer)^, ResourceStream.Size);
  ResourceStream.Free;
  Log(2, 'Reading of ' + IntToStr(BytesRead) + ' bytes done.');
end;

procedure TForm1.Log(Text: string);
begin
  Log(0, Text);
end;

procedure TForm1.Log(Level: Integer; Text: string);
begin
  if Text = '' then
    Memo1.Lines.Clear()
  else if Level <= DEBUG_LEVEL then
    Memo1.Lines.Append(Text);
end;

procedure TForm1.SetReadyToPatch(ready: Boolean);
begin
  ButtonPatch.Enabled := ready;
end;

procedure TForm1.ButtonPatchClick(Sender: TObject);
begin
  if MessageDlg('Apply patch?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    SetReadyToPatch(False);
    if PatchFile(EditFileName.Text) then
      Log('Patching succeed.')
    else
      Log('Patching failed.');
  end
  else
  begin
    Log('Patching canceled.');
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if FindCmdLineSwitch('debug') then
    DEBUG_LEVEL := 2;
  LabelVersion.Caption := 'Version ' + Tlib.CurrentFileInfo(Application.ExeName);
end;

function TForm1.PatchFile(FileName: string): Boolean;
var
  FileBuffer: TByteDynArray;
  PatternBuffer: TByteDynArray;
  SearchInfo: TSearchInfoDynArray;
  FileStream: TFileStream;
  BytesRead: Integer;
begin
  Result := False;
  try
    FileStream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite);
    Log(2, 'Allocating ' + IntToStr(FileStream.Size) + ' bytes...');
    SetLength(FileBuffer, FileStream.Size);
    Log(2, 'Allocated.');
    Log(2, 'Reading...');
    BytesRead := FileStream.Read(Pointer(FileBuffer)^, FileStream.Size);
    Log(2, 'Reading of ' + IntToStr(BytesRead) + ' bytes done.');
    SearchInfo := SearchForPatterns(FileBuffer);
    if Length(SearchInfo) = 1 then
    begin
      if (SearchInfo[0].PatchVersion <> pvThisPatch) then
      begin
        LoadFromResource(Patterns[pvThisPatch], PatternBuffer);
        SetCallAddresses(PatternBuffer, SearchInfo[0].Calls);

        FileStream.Seek(SearchInfo[0].FileOffset, soFromBeginning);
        Log('Created backup file ''' + BackupFile(FileName) + '''');
        Log('Patching...');
        Log(2, 'Writing...');
        FileStream.Write(Pointer(PatternBuffer)^, Length(PatternBuffer));
        Log(2, 'Writing of ' + IntToStr(BytesRead) + ' bytes done.');
        FileStream.Free;
        Result := True;
      end;
    end;
  except
    on E: Exception do
      Log('Error: ' + E.Message);
  end;
end;

function TForm1.BackupFile(FileName: string): string;
var
  BackupFileName: string;
  FileNumber: Integer;
begin
  Result := '';
  BackupFileName := FileName + '.bakFA';
  FileNumber := 1;
  while FileExists(BackupFileName) do
  begin
    Inc(FileNumber);
    BackupFileName := FileName + '.bakFA(' + IntToStr(FileNumber) + ')';
    if FileNumber > 1000 then
      raise Exception.Create('Failed creating backup file');
  end;
  if CopyFile(PAnsiChar(FileName), PAnsiChar(BackupFileName), True) then
    Result := BackupFileName
  else
    raise Exception.Create('Failed creating backup file');
end;

procedure TForm1.OpenGitHubLink(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open', 'https://github.com/FoxAhead/Civilization-II-64-bit-Editbox-Patcher',
    nil, nil, SW_SHOW);
end;

function TForm1.SearchForPatterns(var FileBuffer: TByteDynArray): TSearchInfoDynArray;
var
  PatternBuffer: TByteDynArray;
  PatternMask: TByteDynArray;
  Calls: TCallDynArray;
  FileSize: Integer;
  PatternSize: Integer;
  p: TPatchVersion;
  i, j, k: Integer;
begin
  Result := nil;
  Log(1, 'Searching...');
  FileSize := Length(FileBuffer);

  for p := Low(Patterns) to High(Patterns) do
  begin
    Log(1, 'Searching pattern ' + Patterns[p] + '...');
    LoadFromResource(Patterns[p], PatternBuffer);
    MaskCalls(PatternBuffer, PatternMask, Calls);
    PatternSize := Length(PatternBuffer);
    for i := 0 to FileSize - PatternSize do
    begin
      j := 0;
      while ((FileBuffer[i + j] = PatternBuffer[j]) or (PatternMask[j] = 1)) and (j < PatternSize) do
        Inc(j);
      if j = PatternSize then //FOUND
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)].PatchVersion := p;
        Result[High(Result)].FileOffset := i;
        Log(1, 'FOUND pattern at #' + IntToHex(i, 4) + '!');
        for j := Low(Calls) to High(Calls) do
        begin
          Calls[j].Address := PInteger(@FileBuffer[i + Calls[j].Offset])^;
        end;
        Result[High(Result)].Calls := Copy(Calls, 0, Length(Calls));
      end;
    end;
  end;
  Log(1, 'Searching done.');

end;

procedure TForm1.MaskCalls(var PatternBuffer, PatternMask: TByteDynArray; var Calls: TCallDynArray);
var
  i, j: Integer;
begin
  Calls := nil;
  PatternMask := nil;
  SetLength(PatternMask, Length(PatternBuffer));
  j := 0;

  i := Low(PatternBuffer);
  repeat
    i := FindPos(PatternBuffer, [$FF, $15, $FF, $FF, $FF], i);
    if i >= 0 then
    begin
      SetLength(Calls, j + 1);
      Calls[j].Offset := i + 2;
      PatternMask[i + 2] := 1;
      PatternMask[i + 3] := 1;
      PatternMask[i + 4] := 1;
      PatternMask[i + 5] := 1;
      Calls[j].Proc := TProc(PatternBuffer[i + 5]);
      Inc(j);
      Inc(i, 6);
    end;
  until (i < 0) or (i > High(PatternBuffer));
end;

function TForm1.GetVersionName(var FileBuffer: TByteDynArray): string;
var
  i, j: Integer;
  AList: TStringList;
  LStr: PAnsiChar;
begin
  Result := '';
  i := FindPos(
    FileBuffer,
    [$00, $54, $55, $54, $4F, $52, $49, $41, $4C, $00] //#00 + TUTORIAL + #00
    );
  if i < 0 then
    Exit;
  i := FindPos(
    FileBuffer,
    [$00, $4C, $41, $42, $45, $4C, $53, $00], //#00 + LABEL + #00
    i - 1,
    sdUp
    );
  if i < 0 then
    Exit;

  AList := TStringList.Create;
  for j := 0 to 1 do
  begin
    while FileBuffer[i] = $00 do
      Dec(i);
    while FileBuffer[i] <> $00 do
      Dec(i);
    LStr := PAnsiChar(@FileBuffer[i + 1]);
    AList.Add(LStr);
  end;
  Result := AList.Strings[0];
  if Pos('Patch', Result) > 0 then
    Result := AList.Strings[1] + ' ' + Result;
  AList.Free;
  //
end;

function TForm1.FindPos(var Where: array of Byte; What: array of Byte; Offset: Integer = 0; SearchDirection: TSearchDirection = sdDown): Integer;
var
  i, j: Integer;
begin
  Result := -1;
  case SearchDirection of
    sdDown:
      begin
        for i := Low(Where) + Offset to High(Where) - High(What) do
        begin
          j := Low(What);
          while (Where[i + j] = What[j]) and (j < High(What)) do
            Inc(j);
          if j = High(What) then
          begin
            Result := i - Low(Where);
            Exit;
          end;
        end;
      end;
    sdUp:
      begin
        for i := Low(Where) + Offset downto Low(Where) do
        begin
          j := Low(What);
          while (Where[i + j] = What[j]) and (j < High(What)) do
            Inc(j);
          if j = High(What) then
          begin
            Result := i - Low(Where);
            Exit;
          end;
        end;
      end;
  end;
end;

function TForm1.GetApplicationType(var FileBuffer: TByteDynArray): TApplicationType;
var
  i: Integer;
begin
  Result := atGame;
  i := FindPos(
    FileBuffer,
    [$00, $4D, $41, $50, $00, $42, $52, $55, $53, $48, $00] //.MAP.BRUSH.
    );
  if i >= 0 then
    Result := atEditor;
end;

procedure TForm1.SetCallAddresses(var PatternBuffer: TByteDynArray; var Calls: TCallDynArray);
var
  i, j: Integer;
  FoundAddress: Boolean;
begin
  i := Low(PatternBuffer);
  repeat
    i := FindPos(PatternBuffer, [$FF, $15, $FF, $FF, $FF], i);
    if i >= 0 then
    begin
      FoundAddress := False;
      for j := Low(Calls) to High(Calls) do
      begin
        if Calls[j].Proc = TProc(PatternBuffer[i + 5]) then
        begin
          PInteger(@PatternBuffer[i + 2])^ := Calls[j].Address;
          FoundAddress := True;
          Break;
        end;
      end;
      if FoundAddress = False then
        raise Exception.Create('Procedure call address not found');
      Inc(i, 6);
    end;
  until (i < 0) or (i > High(PatternBuffer));
end;

end.
