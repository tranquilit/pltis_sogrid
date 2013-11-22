unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, vte_stringlist, vte_treedata, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ExtCtrls, sogrid, IdHTTP, VirtualTrees,
  VTHeaderPopup, LCLType, types, ActiveX;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit1: TEdit;
    EdURL: TEdit;
    IdHTTP1: TIdHTTP;
    ListBox1: TListBox;
    Memo1: TMemo;
    Panel1: TPanel;
    SOGrid1: TSOGrid;
    SOGrid2: TSOGrid;
    VirtualList1: TVirtualList;
    VirtualStringTreeData1: TVirtualStringTreeData;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ListBox1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListBox1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SOGrid1DragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      const Pt: TPoint; var Effect: DWORD; Mode: TDropMode);
    procedure SOGrid1FocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure SOGrid1HeaderDraggedOut(Sender: TVTHeader; Column: TColumnIndex;
      const DropPosition: TPoint);
    procedure SOGrid1KeyAction(Sender: TBaseVirtualTree; var CharCode: Word;
      var Shift: TShiftState; var DoDefault: Boolean);
    procedure SOGrid1NewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const NewText: String);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
uses tisstrings,superobject;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin

  sogrid1.Data := SO(IdHTTP1.Get(EdURL.Text));
  sogrid1.CreateColumnsFromData;
  //sogrid1.Header.AutoFitColumns(False);

  sogrid2.Data := SOGrid1.Data;
  sogrid2.Header.AutoFitColumns(False);

end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo1.Lines.Text := SOGrid1.JSONdata;
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

procedure TForm1.SOGrid1DragDrop(Sender: TBaseVirtualTree; Source: TObject;
  DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
  const Pt: TPoint; var Effect: DWORD; Mode: TDropMode);
begin
    showmessage('toto');

end;

procedure TForm1.SOGrid1FocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  if Node<>Nil then
    Edit1.Text:= SOGrid1.GetCellData(Node,'host.computer_fqdn').AsString
  else
   Edit1.Text:='';;
end;

procedure TForm1.SOGrid1HeaderDraggedOut(Sender: TVTHeader;
  Column: TColumnIndex; const DropPosition: TPoint);
begin
  if ListBox1.Items.IndexOfObject(Sender.Columns[Column])<0 then
    ListBox1.AddItem(Sender.Columns[Column].Text, Sender.Columns[Column]);
  Sender.Columns[Column].Options:=Sender.Columns[Column].Options - [coVisible] ;
end;

procedure TForm1.SOGrid1KeyAction(Sender: TBaseVirtualTree; var CharCode: Word;
  var Shift: TShiftState; var DoDefault: Boolean);
var
  foc,newfoc:PVirtualNode;
begin
  if (ssCtrl in Shift) and (CharCode=VK_DELETE) then
  begin
    foc := SOGrid1.FocusedNode;
    newfoc := SOGrid1.GetNext(SOGrid1.FocusedNode);
    if newfoc = Nil then
      newfoc := SOGrid1.GetPrevious(SOGrid1.FocusedNode);
    SOGrid1.DeleteNode(foc);
    if newfoc<>Nil then
    begin
      sogrid1.FocusedNode:=newfoc;
      sogrid1.Selected[newfoc] := True;
    end;
    DoDefault:=False;
  end;
end;

procedure TForm1.SOGrid1NewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; const NewText: String);
var
   row:ISuperObject;
begin
  row := SOGrid1.GetData(Node);
  SOGrid2.InvalidateFordata(row);
end;

{$R *.lfm}

end.

