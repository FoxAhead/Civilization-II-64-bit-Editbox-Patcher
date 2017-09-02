program Civ2EditboxPatcher;

{$R 'patterns.res' 'patterns.rc'}

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Civilization II 64-bit Editbox Patcher';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
