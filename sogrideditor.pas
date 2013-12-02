unit sogrideditor;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
    ButtonPanel, ExtCtrls, StdCtrls, ActnList, Menus, sogrid, VirtualTrees;

type

    { TSOGridEditor }

    TSOGridEditor = class(TForm)
        ActAddColumn: TAction;
        ActDelColumn: TAction;
        ActCopySettings: TAction;
        ActAddColumns: TAction;
        ActRemoveAllColumns: TAction;
        ActpasteCSV: TAction;
        ActUpdateColumn: TAction;
        ActPasteJsontemplate: TAction;
        ActLoadData: TAction;
        ActionList1: TActionList;
        Button1: TButton;
        Button2: TButton;
        Button3: TButton;
        Button4: TButton;
        Button5: TButton;
        ButtonPanel1: TButtonPanel;
        cbEditorType: TComboBox;
        EdColumnTitle: TLabeledEdit;
        EdColumnProperty: TLabeledEdit;
        EdColumnIndex: TLabeledEdit;
        EdJSONUrl: TLabeledEdit;
        LstEditorType: TLabel;
        MenuItem1: TMenuItem;
        MenuItem2: TMenuItem;
        MenuItem3: TMenuItem;
        MenuItem4: TMenuItem;
        MenuItem5: TMenuItem;
        MenuItem6: TMenuItem;
        MenuItem7: TMenuItem;
        Panel1: TPanel;
        ASOGrid: TSOGrid;
        Panel2: TPanel;
        PopupMenu1: TPopupMenu;
        procedure ActAddColumnExecute(Sender: TObject);
        procedure ActAddColumnsExecute(Sender: TObject);
        procedure ActDelColumnExecute(Sender: TObject);
        procedure ActLoadDataExecute(Sender: TObject);
        procedure ActLoadDataUpdate(Sender: TObject);
        procedure ActpasteCSVExecute(Sender: TObject);
        procedure ActPasteJsontemplateExecute(Sender: TObject);
        procedure ActRemoveAllColumnsExecute(Sender: TObject);
        procedure ActUpdateColumnExecute(Sender: TObject);
        procedure ActUpdateColumnUpdate(Sender: TObject);
        procedure ASOGridFocusChanged(Sender: TBaseVirtualTree;
            Node: PVirtualNode; Column: TColumnIndex);
        procedure EdColumnPropertyKeyPress(Sender: TObject; var Key: char);
        procedure EdColumnTitleKeyPress(Sender: TObject; var Key: char);
        procedure FormCreate(Sender: TObject);
    private
        { private declarations }
    public
        { public declarations }
    end;

implementation

uses Clipbrd,superobject,soutils,tishttp;

{$R *.lfm}

{ TSOGridEditor }
const
    sampleJsonData = '[{''id'':0}]';

procedure TSOGridEditor.ActLoadDataUpdate(Sender: TObject);
begin
    ActLoadData.Enabled := EdJSONUrl.text<>'';
end;

procedure TSOGridEditor.ActpasteCSVExecute(Sender: TObject);
begin
  ASOGrid.Data := CSV2SO(Clipboard.Astext);
end;

procedure TSOGridEditor.ActAddColumnExecute(Sender: TObject);
var
  col : TSOGridColumn;
begin
    col :=  TSOGridColumn(ASOGrid.Header.Columns.Add);
    col.Text := 'Col '+IntToStr(col.Index);
    col.PropertyName := 'column'+IntToStr(col.Index);
    ASOGrid.FocusedColumn:=col.Index;
end;

procedure TSOGridEditor.ActAddColumnsExecute(Sender: TObject);
begin
  ASOGrid.CreateColumnsFromData(false);
end;

procedure TSOGridEditor.ActDelColumnExecute(Sender: TObject);
var
    newidx : TColumnIndex;
begin
    newidx:=ASOGrid.Header.Columns.GetNextVisibleColumn(ASOGrid.FocusedColumn);
    if not ASOGrid.Header.Columns.IsValidColumn(newidx) then
        newidx:=ASOGrid.Header.Columns.GetPreviousVisibleColumn(ASOGrid.FocusedColumn);

    ASOGrid.Header.Columns.Delete(ASOGrid.FocusedColumn);
    if ASOGrid.Header.Columns.IsValidColumn(newidx) then
        ASOGrid.FocusedColumn:=newidx
    else
        ASOGrid.FocusedColumn:=ASOGrid.Header.Columns.GetFirstVisibleColumn;

end;

procedure TSOGridEditor.ActLoadDataExecute(Sender: TObject);
begin
  ASOGrid.Data := SO(httpGetString(EdJSONUrl.Text));
end;

procedure TSOGridEditor.ActPasteJsontemplateExecute(Sender: TObject);
var
  newData:ISuperObject;
begin
    try
      if Clipboard.HasFormat(ClipbrdJson) then
        newData :=ClipboardSOData
      else
        newData := SO(Clipboard.AsText);
      if (newData.DataType = stArray) and (newData.AsArray.Length>0) and (newData.AsArray[0].DataType=stObject) then
      begin
        ASOGrid.Data := newData;
        ASOGrid.LoadData;
        ASOGrid.CreateColumnsFromData(True);
      end
      else
      if (newData.DataType = stObject) then
      begin
        ASOGrid.Data := TSuperObject.Create(stArray);
        ASOGrid.Data.AsArray.Add(SO(Clipboard.AsText));
        ASOGrid.LoadData;
        ASOGrid.CreateColumnsFromData(True);
      end
      else
        ShowMessage('Clipboard content is not a valid json Array of records');
    except
      ASOGrid.Data := SO(sampleJsonData);
      ShowMessage('Clipboard content is not a valid json Array of records');
    end;
end;

procedure TSOGridEditor.ActRemoveAllColumnsExecute(Sender: TObject);
begin
  ASOGrid.Header.Columns.Clear;
end;

procedure TSOGridEditor.ActUpdateColumnExecute(Sender: TObject);
var
  col : TSOGridColumn;
begin
    col := ASOGrid.FocusedColumnObject;
    if col <>Nil then
    begin
        col.Text := EdColumnTitle.Text;
        col.PropertyName:=EdColumnProperty.Text;
    end;
    ASOGrid.Invalidate;
end;

procedure TSOGridEditor.ActUpdateColumnUpdate(Sender: TObject);
begin
    (Sender as TAction).Enabled := ASOGrid.FocusedColumnObject<>Nil;
end;

procedure TSOGridEditor.ASOGridFocusChanged(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Column: TColumnIndex);
var
  col : TSOGridColumn;
begin
    col := ASOGrid.FocusedColumnObject;
    if col <>Nil then
    begin
        EdColumnIndex.Text := IntToStr(col.Index);
        EdColumnTitle.Text := col.Text;
        EdColumnProperty.Text := col.PropertyName;
    end
    else
    begin
        EdColumnIndex.Text := '';
        EdColumnTitle.Text := '';
        EdColumnProperty.Text := '';
    end;
end;

procedure TSOGridEditor.EdColumnPropertyKeyPress(Sender: TObject; var Key: char
    );
begin
    if (key=#13) and (ASOGrid.FocusedColumnObject <>Nil)  then
    begin
        ASOGrid.FocusedColumnObject.PropertyName := EdColumnProperty.Text;
        ASOGrid.Invalidate;
        Key := #0;
    end;
end;

procedure TSOGridEditor.EdColumnTitleKeyPress(Sender: TObject; var Key: char);
begin
    if (key=#13) and (ASOGrid.FocusedColumnObject <>Nil)  then
    begin
        ASOGrid.FocusedColumnObject.Text := EdColumnTitle.Text;
        ASOGrid.Invalidate;
        Key := #0;
    end;
end;

procedure TSOGridEditor.FormCreate(Sender: TObject);
begin
    ASOGrid.Data := SO(sampleJsonData);
    ButtonPanel1.OKButton.Default:=False;
end;

end.

