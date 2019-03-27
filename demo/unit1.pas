unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, vte_stringlist, vte_treedata, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ExtCtrls, sogrid, IdHTTP,
  VirtualTrees, VTHeaderPopup, LCLType, ComCtrls, types,
  ActiveX;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Edit1: TEdit;
    EdURL: TEdit;
    IdHTTP1: TIdHTTP;
    ListBox1: TListBox;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    SODataSource1: TSODataSource;
    SOGrid1: TSOGrid;
    SOGrid2: TSOGrid;
    VirtualList1: TVirtualList;
    VirtualStringTreeData1: TVirtualStringTreeData;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ListBox1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListBox1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SOGrid1Change(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure SOGrid1DragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      const Pt: TPoint; var Effect: DWORD; Mode: TDropMode);
    procedure SOGrid1HeaderDraggedOut(Sender: TVTHeader; Column: TColumnIndex;
      const DropPosition: TPoint);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
uses tisstrings,superobject, sogrideditor, tishttp;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  SODataSource1.Data :=  SO(IdHTTP1.Get(EdURL.Text));
  //sogrid1.CreateColumnsFromData(True);
  //sogrid1.Header.AutoFitColumns(False);

  //sogrid2.Data := SOGrid1.Data;
  //sogrid2.Header.AutoFitColumns(False);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo1.Lines.Text := SODataSource1.Data.AsJSon(True);
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  i:Integer;
  col : TSOGridColumn;
  target : TSOGrid;
begin

  target := (SOGrid1 as TSOGrid);

  With TSOGridEditor.Create(Application) do
  try
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

procedure TForm1.ListBox1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  ListBox1.AddItem( (Source as TVirtualTreeColumn).Text,Source );
end;

procedure TForm1.ListBox1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := Source is TVirtualTreeColumn;
end;

procedure TForm1.SOGrid1Change(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  if Node<>Nil then
    Edit1.Text:= SOGrid1.GetCellData(Node,'host.computer_fqdn',SO('')).AsString
  else
   Edit1.Text:='';;

end;

procedure TForm1.SOGrid1DragDrop(Sender: TBaseVirtualTree; Source: TObject;
  DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
  const Pt: TPoint; var Effect: DWORD; Mode: TDropMode);
begin
    showmessage('Dropped');
end;

procedure TForm1.SOGrid1HeaderDraggedOut(Sender: TVTHeader;
  Column: TColumnIndex; const DropPosition: TPoint);
begin
  if ListBox1.Items.IndexOfObject(Sender.Columns[Column])<0 then
    ListBox1.AddItem(Sender.Columns[Column].Text, Sender.Columns[Column]);
  Sender.Columns[Column].Options:=Sender.Columns[Column].Options - [coVisible] ;
end;

{$R *.lfm}

end.

