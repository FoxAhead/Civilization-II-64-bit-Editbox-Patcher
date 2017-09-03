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

end.
