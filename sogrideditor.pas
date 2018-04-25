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
        EdPosition: TEdit;
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
        procedure ASOGridHeaderDragged(Sender: TVTHeader; Column: TColumnIndex;
          OldPosition: Integer);
        procedure Button6Click(Sender: TObject);
        procedure EdColumnPropertyExit(Sender: TObject);
        procedure EdColumnPropertyKeyPress(Sender: TObject; var Key: char);
        procedure EdColumnTitleExit(Sender: TObject);
        procedure EdColumnTitleKeyPress(Sender: TObject; var Key: char);
        procedure FormCreate(Sender: TObject);
    private
        { private declarations }
    public
        { public declarations }
    end;

implementation

uses

{$ifdef windows}
 tiswinhttp
{$endif}
 IdHTTP,
 Clipbrd,
 superobject,
 soutils,
 soclipbrd;

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
    delcol : TColumnIndex;
begin
  delcol := ASOGrid.FocusedColumn;
  ASOGrid.Header.Columns.Delete(delcol);
  if ASOGrid.Header.Columns.IsValidColumn(delcol) then
    ASOGrid.FocusedColumn:=delcol
  else
    ASOGrid.FocusedColumn:=ASOGrid.Header.Columns.GetLastVisibleColumn;

end;

procedure TSOGridEditor.ActLoadDataExecute(Sender: TObject);
label
  LBL_FAIL;
const
    HTTP_TIMEOUT_SECONDS : integer = 3;
var
    newdata:ISuperObject;
    http : TIdHTTP;
    s : String;
begin
  http := nil;

  http := TIdHTTP.Create;
  http.HandleRedirects := true;
  http.ConnectTimeout := HTTP_TIMEOUT_SECONDS * 1000;
  http.ReadTimeout := HTTP_TIMEOUT_SECONDS * 1000;
  try
    s := http.Get( EdJSONUrl.Text );
  except
  end;
  if http.Connected then
    http.DisconnectNotifyPeer;
  if http.ResponseCode <> 200 then
     goto LBL_FAIL;
  http.free;
  http := nil;

  newData := SO( s );
  if (newdata<>Nil) and (newdata.DataType=stObject) and (newdata.AsObject.Exists('content')) then
    newdata := newdata.AsObject['content'];
  ASOGrid.Data := newdata;
  exit;

LBL_FAIL:
  if http <> nil then
     http.free;
end;

procedure TSOGridEditor.ActPasteJsontemplateExecute(Sender: TObject);
var
  newData,row,samplerows:ISuperObject;
  s:String;
begin
    try
      if Clipboard.HasFormat(ClipbrdJson) then
        newData :=ClipboardSOData
      else
        newData := SO(Clipboard.AsText);
      if (newData.DataType = stArray) and (newData.AsArray.Length>0) then
      begin
        if (newData.AsArray[0].DataType=stObject) then
          ASOGrid.Data := newData
        else
        begin
          sampleRows := TSuperObject.Create(stArray);
          for row in newData do
            sampleRows.AsArray.Add(SO(['unknowncolumn',row]));
          ASOGrid.Data := samplerows;
        end;
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
        EdPosition.Text:=inttostr(col.Position);
    end
    else
    begin
        EdColumnIndex.Text := '';
        EdColumnTitle.Text := '';
        EdColumnProperty.Text := '';
    end;
end;

procedure TSOGridEditor.ASOGridHeaderDragged(Sender: TVTHeader;
  Column: TColumnIndex; OldPosition: Integer);
begin
  ASOGrid.ReorderColumns;
end;

function colsort(c1,c2:TCollectionItem):integer;
begin
  if TSOGridColumn(c1).position<TSOGridColumn(c2).position then
    result := -1
  else
  if TSOGridColumn(c1).position>TSOGridColumn(c2).position then
    result := 1
  else
    Result := 0;
end;

procedure TSOGridEditor.Button6Click(Sender: TObject);
begin
  ASOGrid.ReorderColumns;
end;

procedure TSOGridEditor.EdColumnPropertyExit(Sender: TObject);
begin
  if ASOGrid.FocusedColumnObject<>Nil then
  begin
    ASOGrid.FocusedColumnObject.PropertyName := EdColumnProperty.Text;
    ASOGrid.Invalidate;
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

procedure TSOGridEditor.EdColumnTitleExit(Sender: TObject);
begin
  if ASOGrid.FocusedColumnObject<>Nil then
  begin
    ASOGrid.FocusedColumnObject.Text := EdColumnTitle.Text;
    ASOGrid.Invalidate;
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

