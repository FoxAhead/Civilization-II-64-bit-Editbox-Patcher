unit Unit1;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Types,
  Unit2;

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
    Label4: TLabel;
    procedure ButtonBrowseClick(Sender: TObject);
    procedure ButtonPatchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
    function SearchForPatterns(var FileBuffer: TByteDynArray): TSearchInfo;
    function BackupFile(FileName: string): string;
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
  SearchInfo: TSearchInfo;
begin
  Result := False;
  try
    Log(1, 'Analyzing file ' + FileName + '...');
    LoadFromFile(FileName, FileBuffer);
    SearchInfo := SearchForPatterns(FileBuffer);
    Log(1, 'Analyzing done.');
    if (SearchInfo.PatternIndex < 0) or (SearchInfo.Matches <> 1) then
    begin
      Log('Wrong file.');
      Result := False;
    end
    else
    begin
      Log('Version ''' + VersionName[Patterns[SearchInfo.PatternIndex].Version] + ''' detected.');
      if Patterns[SearchInfo.PatternIndex].ApplicationType = atEditor then
        Log('It is Map Editor.');
      if Patterns[SearchInfo.PatternIndex].PatchVersion = pvMastermind then
        Log('Mastermind patch detected. Will be replaced.');
      if Patterns[SearchInfo.PatternIndex].PatchVersion = pvThisPatch then
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

function TForm1.SearchForPatterns(var FileBuffer: TByteDynArray): TSearchInfo;
var
  PatternBuffer: TByteDynArray;
  FileSize: Integer;
  PatternSize: Integer;
  p, i, j, k: Integer;
begin
  Log(1, 'Searching...');
  Result.PatternIndex := -1;
  Result.FileOffset := -1;
  Result.Matches := 0;
  FileSize := Length(FileBuffer);
  for p := Low(Patterns) to High(Patterns) do
  begin
    Log(1, 'Searching pattern ' + Patterns[p].PatternResourceName + '...');
    LoadFromResource(Patterns[p].PatternResourceName, PatternBuffer);
    PatternSize := Length(PatternBuffer);
    for i := 0 to FileSize - PatternSize do
    begin
      j := 0;
      while (FileBuffer[i + j] = PatternBuffer[j]) and (j < PatternSize) do
        Inc(j);
      if j = PatternSize then
      begin
        Inc(Result.Matches);
        Result.PatternIndex := p;
        Result.FileOffset := i;
        Log(1, 'FOUND pattern at ' + IntToHex(i, 4) + '!');
      end;
    end;
  end;
  Log(1, 'Searching done.');
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
end;

function TForm1.PatchFile(FileName: string): Boolean;
var
  FileBuffer: TByteDynArray;
  PatternBuffer: TByteDynArray;
  SearchInfo: TSearchInfo;
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
    if (SearchInfo.PatternIndex >= 0) and
      (SearchInfo.Matches = 1) and
      (Patterns[SearchInfo.PatternIndex].PatchVersion <> pvThisPatch) then
    begin
      LoadFromResource(Patterns[SearchInfo.PatternIndex].ReplaceResourceName, PatternBuffer);
      FileStream.Seek(SearchInfo.FileOffset, soFromBeginning);
      Log('Created backup file ''' + BackupFile(FileName) + '''');
      Log('Patching...');
      Log(2, 'Writing...');
      FileStream.Write(Pointer(PatternBuffer)^, Length(PatternBuffer));
      Log(2, 'Writing of ' + IntToStr(BytesRead) + ' bytes done.');
      FileStream.Free;
      Result := True;
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

end.
