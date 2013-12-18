{**********************************************************************
 Package pl_VirtualTreeExtra.pkg
 This unit is part of CodeTyphon Studio  (http://www.pilotlogic.com/)
***********************************************************************}

unit sogrid_register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LCLProc, LCLType, LMessages, LResources,
  LazIDEIntf, PropEdits, ComponentEditors,TypInfo,
  sogrid;

type

  { TSOGridComponentEditor }

  TSOGridComponentEditor = class(TComponentEditor)
  protected
    procedure DoShowColumnsEditor;
    procedure DoShowEditor;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function  GetVerb(Index: Integer): String; override;
    function  GetVerbCount: Integer; override;
  end;

  { TSODatasourceComponentEditor }

  TSODatasourceComponentEditor = class(TComponentEditor)
  protected
  public
    procedure ExecuteVerb(Index: Integer); override;
    function  GetVerb(Index: Integer): String; override;
    function  GetVerbCount: Integer; override;
  end;

  { TSuperObjectPropertyEditor }
  TSuperObjectPropertyEditor = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;


procedure Register;

implementation

uses sogrideditor,forms,windows,controls,Dialogs,soutils,clipbrd,superobject;

procedure Register;
begin
  RegisterComponents('SuperObject Controls', [TSOGrid,TSODataSource,TSOConnection]);
  RegisterComponentEditor(TSOGrid,TSOGridComponentEditor);
  RegisterComponentEditor(TSODataSource,TSODatasourceComponentEditor);
  //RegisterPropertyEditor(TypeInfo(ISuperObject), nil, '', TSuperObjectPropertyEditor);
end;

{ TSuperObjectPropertyEditor }

function TSuperObjectPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paRevertable];
end;

function TSuperObjectPropertyEditor.GetValue: string;
var
  soval:ISuperObject ;
begin
  soval := GetIntfValue as ISuperObject;
  if SOVAL<>Nil then
    Result:=soval.AsString
  else
    Result:='null';
end;

procedure TSuperObjectPropertyEditor.SetValue(const Value: string);
var
  soval:ISuperObject ;
begin
  if Value='null' then
    SetIntfValue(Nil)
  else
    SetIntfValue(SO(Value));
end;


{ TSODatasourceComponentEditor }

procedure TSODatasourceComponentEditor.ExecuteVerb(Index: Integer);
var
  src:TSODataSource;
  url:String;
begin
  src := Component as TSODataSource;
  case index of
    0:src.LoadDataset;
    1:src.Emptydataset;
    2:src.PasteFromClipboard;
    3:With TOpenDialog.Create(Application) do
      try
        Title:='Chosse a JSON file';
        Filter:='All files|*.*|JSon files|*.json';
        DefaultExt := 'json';
        if Execute then
          src.LoadFromFile(FileName);
      finally
        Free;
      end;
    4:begin
        src.Data := CSV2SO(Clipboard.Astext);
      end;
  end;
end;

function TSODatasourceComponentEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := 'Load/Refresh data';
    1: Result := 'Empty data';
    2: Result := 'Load JSON from clipboard';
    3: Result := 'Load JSON from file';
    4: Result := 'Load CSV from clipboard';
  end;

end;

function TSODatasourceComponentEditor.GetVerbCount: Integer;
begin
  Result := 5;
end;

procedure TSOGridComponentEditor.DoShowColumnsEditor;
begin
  EditCollection( Component, (Component as TSOGrid).Header.Columns,'Header.Columns');
end;

procedure TSOGridComponentEditor.DoShowEditor;
var
  i:Integer;
  col : TSOGridColumn;
  target : TSOGrid;
begin
  With TSOGridEditor.Create(Application) do
  try
      target := (Component as TSOGrid);
      for i:=0 to target.Header.Columns.count-1 do
      begin
         col := ASOGrid.Header.Columns.Add as TSOGridColumn;
         col.Assign(target.Header.Columns[i]);
      end;
      asogrid.Settings := target.Settings;
      ASOGrid.Datasource := target.Datasource;
      if ASOGrid.data = Nil then
        ASOGrid.Data := TSuperObject.Create(stArray);
      if ASOGrid.Data.AsArray.Length=0 then
      begin
        ASOGrid.Data.AsArray.Add(TSuperObject.Create);
        ASOGrid.LoadData;
      end;
      if ShowModal = mrOK then
      begin
        target.Header.Columns.Clear;
        for i:=0 to asogrid.Header.Columns.count-1 do
        begin
           col := target.Header.Columns.Add as TSOGridColumn;
           col.Assign(asogrid.Header.Columns[i]);
        end;
        target.Settings := asogrid.Settings;
      end;
  finally
    Free;
  end;
end;

procedure TSOGridComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: DoShowEditor;
    1: DoShowColumnsEditor;
    2: (Component as TSOGrid).CreateColumnsFromData(False);
  end;
end;

function TSOGridComponentEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := 'Edit grid...';
    1: Result := 'Edit columns...';
    2: Result := 'Create missing columns from sample data';
  end;

end;

function TSOGridComponentEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

initialization
  {$i sogrid_register.lrs}

finalization

end.

