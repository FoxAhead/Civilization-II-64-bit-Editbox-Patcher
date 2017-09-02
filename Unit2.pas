unit Unit2;

interface
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
  Patterns: array[0..5] of TPattern =
    (
    // MGE - Game
    (
    PatternResourceName: 'MGEGameReplace';
    ReplaceResourceName: '';
    Version: verMGE;
    ApplicationType: atGame;
    PatchVersion: pvThisPatch;
    ),
    (
    PatternResourceName: 'MGEGamePattern';
    ReplaceResourceName: 'MGEGameReplace';
    Version: verMGE;
    ApplicationType: atGame;
    PatchVersion: pvNone;
    ),
    (
    PatternResourceName: 'MGEGamePatternMastermind';
    ReplaceResourceName: 'MGEGameReplace';
    Version: verMGE;
    ApplicationType: atGame;
    PatchVersion: pvMastermind;
    ),
    // MGE - Editor
    (
    PatternResourceName: 'MGEEditorReplace';
    ReplaceResourceName: '';
    Version: verMGE;
    ApplicationType: atEditor;
    PatchVersion: pvThisPatch;
    ),
    (
    PatternResourceName: 'MGEEditorPattern';
    ReplaceResourceName: 'MGEEditorReplace';
    Version: verMGE;
    ApplicationType: atEditor;
    PatchVersion: pvNone;
    ),
    (
    PatternResourceName: 'MGEEditorPatternMastermind';
    ReplaceResourceName: 'MGEEditorReplace';
    Version: verMGE;
    ApplicationType: atEditor;
    PatchVersion: pvMastermind;
    )
    // TOT - Game
    );

implementation

end.
