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

procedure Register;

implementation

uses sogrideditor,forms,windows,controls;

procedure Register;
begin
  RegisterComponents('Virtual Controls', [TSOGrid,TSODataSource]);
  RegisterComponentEditor(TSOGrid,TSOGridComponentEditor);
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
    0: DoShowColumnsEditor;
    1: DoShowEditor;
  end;
end;

function TSOGridComponentEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := 'Edit columns...';
    1: Result := 'Edit grid...';
  end;

end;

function TSOGridComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

initialization
  {$i sogrid_register.lrs}

finalization

end.

