{**********************************************************************
 Package pltis_sogrid.pkg
 This unit is based on package virtualtreesextra work from CodeTyphon Studio  (http://www.pilotlogic.com/)
***********************************************************************}

unit sogrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, Controls, SuperObject,Menus;

type

  TSOGrid = class;
  { TSOGridColumn }

  TSOGridColumn = class(TVirtualTreeColumn)
  private
    FPropertyName: String;
    procedure SetPropertyName(const Value: String);
  published
    property PropertyName: String read FPropertyName write SetPropertyName;
  end;

  { TSOGrid }

  TSOGridGetText = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; RowData,CellData: ISuperObject; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: String) of object;

  TSOGrid = class(TCustomVirtualStringTree)
  private
    FData: ISuperObject;
    FItemDataOffset: Integer;
    FOnGetText: TSOGridGetText;
    FDefaultPopupMemu:TPopupMenu;
    function GetJSONdata: String;
    procedure SetData(const Value: ISuperObject);
    procedure SetJSONdata(AValue: String);
    procedure SetOptions(const Value: TStringTreeOptions);
    function GetOptions: TStringTreeOptions;
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String); override;
    procedure DoInitNode(ParentNode, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates); override;
    function GetColumnClass: TVirtualTreeColumnClass; override;
    function GetOptionsClass: TTreeOptionsClass; override;
    function GetItemData(Node: PVirtualNode): Pointer;
    procedure DoFreeNode(Node: PVirtualNode); override;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;
    procedure DoPopupMenu(Node: PVirtualNode; Column: TColumnIndex; const Position: TPoint
      ); override;
    procedure Clear; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Node: PVirtualNode): ISuperObject;
    procedure LoadData;
    property Data: ISuperObject read FData write SetData;
    function GetCellData(N: PVirtualNode; FieldName: string; Default: ISuperObject=
      Nil): ISuperObject;
    function GetCellStrValue(N: PVirtualNode; FieldName: string; Default: string=
      ''): string;
  published
    property OnGetText: TSOGridGetText read FOnGetText write FOnGetText;
    property JSONdata:String read GetJSONdata write SetJSONdata;
    //inherited properties
    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AnimationDuration;
    property AutoExpandDelay;
    property AutoScrollDelay;
    property AutoScrollInterval;
    property Background;
    property BackgroundOffsetX;
    property BackgroundOffsetY;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle default bsSingle;
    property BottomSpace;
    property ButtonFillMode;
    property ButtonStyle;
    property BorderWidth;
    property ChangeDelay;
    property CheckImageKind;
    property ClipboardFormats;
    property Color;
    property Colors;
    property Constraints;
    property DefaultNodeHeight;
    property DefaultPasteMode;
    //property DefaultText;
    property DragCursor;
    property DragHeight;
    property DragKind;
    property DragImageKind;
    property DragMode;
    property DragOperations;
    property DragType;
    property DragWidth;
    property DrawSelectionMode;
    property EditDelay;
    property Enabled;
    property Font;
    property Header;
    property HintMode;
    property HotCursor;
    property Images;
    property IncrementalSearch;
    property IncrementalSearchDirection;
    property IncrementalSearchStart;
    property IncrementalSearchTimeout;
    property Indent;
    property LineMode;
    property LineStyle;
    property Margin;
    property NodeAlignment;
    property NodeDataSize;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RootNodeCount;
    property ScrollBarOptions;
    property SelectionBlendFactor;
    property SelectionCurveRadius;
    property ShowHint;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property TextMargin;
    property TreeOptions: TStringTreeOptions read GetOptions write SetOptions;
    property Visible;
    property WantTabs;

    property OnAdvancedHeaderDraw;
    property OnAfterAutoFitColumns;
    property OnAfterCellPaint;
    property OnAfterGetMaxColumnWidth;
    property OnAfterItemErase;
    property OnAfterItemPaint;
    property OnAfterPaint;
    property OnBeforeAutoFitColumns;
    property OnBeforeCellPaint;
    property OnBeforeGetMaxColumnWidth;
    property OnBeforeItemErase;
    property OnBeforeItemPaint;
    property OnBeforePaint;
    property OnCanSplitterResizeColumn;
    property OnChange;
    property OnChecked;
    property OnChecking;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnColumnClick;
    property OnColumnDblClick;
    property OnColumnResize;
    property OnCompareNodes;
    property OnContextPopup;
    property OnCreateDataObject;
    property OnCreateDragManager;
    property OnCreateEditor;
    property OnDblClick;
    property OnDragAllowed;
    property OnDragOver;
    property OnDragDrop;
    property OnDrawText;
    property OnEditCancelled;
    property OnEdited;
    //property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnFocusChanged;
    property OnFocusChanging;
    property OnFreeNode;
    property OnGetCellIsEmpty;
    property OnGetCursor;
    property OnGetHeaderCursor;
    property OnPaintText;
    property OnGetHelpContext;
    property OnGetImageIndex;
    property OnGetImageIndexEx;
    property OnGetImageText;
    property OnGetHint;
    property OnGetLineStyle;
    property OnGetNodeDataSize;
    property OnGetPopupMenu;
    property OnGetUserClipboardFormats;
    property OnHeaderClick;
    property OnHeaderDblClick;
    property OnHeaderDragged;
    property OnHeaderDraggedOut;
    property OnHeaderDragging;
    property OnHeaderDraw;
    property OnHeaderDrawQueryElements;
    property OnHeaderMouseDown;
    property OnHeaderMouseMove;
    property OnHeaderMouseUp;
    property OnHotChange;
    property OnIncrementalSearch;
    property OnInitChildren;
    property OnInitNode;
    property OnKeyAction;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLoadNode;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnNewText;
    property OnNodeCopied;
    property OnNodeCopying;
    property OnNodeMoved;
    property OnNodeMoving;
    property OnPaintBackground;
    property OnRenderOLEData;
    property OnResetNode;
    property OnResize;
    property OnSaveNode;
    property OnScroll;
    property OnShortenString;
    property OnShowScrollbar;
    property OnStartDock;
    property OnStartDrag;
    property OnStateChange;
    property OnStructureChange;
    property OnUpdating;
  end;


implementation
uses soutils;

type
  TSOItemData = record
    JSONData: ISuperObject;
  end;
  PSOItemData = ^TSOItemData;


{ TSOGridColumn }
procedure TSOGridColumn.SetPropertyName(const Value: String);
begin
  if FPropertyName = Value then exit;
  if (Text = '') or (Text = FPropertyName) then
    Text := Value;
  FPropertyName := Value;
end;


{ TSOGrid }

function TSOGrid.GetItemData(Node: PVirtualNode): Pointer;
begin
  if (Node = nil) or (FItemDataOffset<=0) then
    Result := nil
  else
    Result := PByte(Node) + FItemDataOffset;
end;

procedure TSOGrid.DoFreeNode(Node: PVirtualNode);
var
  idata:PSOItemData;
begin
  idata := GetItemData(Node);
  //Be sure to release the interface ...
  if idata<>Nil then
    idata^.JSONData := Nil;
  inherited DoFreeNode(Node);
end;

function TSOGrid.GetOptions: TStringTreeOptions;
begin
  Result := TStringTreeOptions(inherited TreeOptions);
end;

procedure TSOGrid.LoadData;
var
  row:ISuperObject;
  Node: PVirtualNode;
  ItemData:PSOItemData;
begin
  if (FData = nil) or (FData.AsArray=Nil) then
    inherited Clear
  else
  begin
    //todo handle object
    BeginUpdate;
    try
      inherited Clear;
      RootNodeCount := FData.AsArray.Length;
    finally
      EndUpdate;
    end;
  end;
end;

function TSOGrid.GetCellData(N: PVirtualNode; FieldName: string;
  Default: ISuperObject): ISuperObject;
var
  pdata:  PSOItemData;
  idata : ISuperObject;
begin
  pdata := GetItemData(N);
  if pdata<>Nil then
  begin
    idata := pdata^.JSONData[FieldName];
    if idata<>Nil then
      Result := idata
    else
      Result := Default;
  end
  else
    Result := Default;
end;

procedure TSOGrid.SetData(const Value: ISuperObject);
begin
  if FData = Value then exit;
  FData := Value;
  LoadData;
end;

function TSOGrid.GetJSONdata: String;
begin
  if FData<>Nil then
    Result := Data.AsJSon(True)
  else
    Result :='';
end;

procedure TSOGrid.SetJSONdata(AValue: String);
begin
  Data := SO(AValue);
end;

procedure TSOGrid.SetOptions(const Value: TStringTreeOptions);
begin
  TreeOptions.Assign(Value);
end;

procedure TSOGrid.DoGetText(Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  ItemData: PSOItemData;
  RowData,CellData: ISuperObject;
begin
  RowData := Nil;
  CellData:=Nil;
  if Node<>Nil then
  begin
    ItemData := GetItemData(Node);
    if ItemData<>Nil then
    begin
      RowData := ItemData^.JSONData;
      if RowData <> Nil then
      begin
        if Column>=0 then
          CellData := RowData[TSOGridColumn(Header.Columns.Items[Column]).PropertyName]
        else
          CellData := RowData[DefaultText];

        if CellData <> Nil then
          CellText := UTF8Encode(CellData.AsString)
        else
          CellText:=DefaultText;
      end;
    end
    else
      CellText:='uninitialized';
  end
  else
    CellText:='';
  if Assigned(FOnGetText) then
    FOnGetText(Self, Node, RowData,CellData,Column, TextType, CellText);
end;

function TSOGrid.GetColumnClass: TVirtualTreeColumnClass;
begin
  Result := TSOGridColumn;
end;

function TSOGrid.GetOptionsClass: TTreeOptionsClass;
begin
  Result := TStringTreeOptions;
end;

constructor TSOGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DefaultText := '';
  FItemDataOffset := AllocateInternalDataArea(SizeOf(TSOItemData));
  with TreeOptions do
  begin
    PaintOptions := PaintOptions - [toShowRoot] +
      [toShowHorzGridLines, toShowVertGridLines, toPopupMode, toHideFocusRect];
    SelectionOptions := SelectionOptions + [toExtendedFocus, toFullRowSelect];
    MiscOptions := MiscOptions + [toEditable, toGridExtensions,toEditOnClick];
  end;

  Header.Options := [hoColumnResize, hoDblClickResize, hoDrag, hoShowSortGlyphs, hoVisible];
  Header.Style := hsFlatButtons;

  FDefaultPopupMemu := TPopupMenu.Create(Self);
  with FDefaultPopupMemu do
  begin

  end;

end;

destructor TSOGrid.Destroy;
begin
  inherited Destroy;
end;

function TSOGrid.GetData(Node: PVirtualNode): ISuperObject;
var
  ItemData : PSOItemData;
begin
  if Node <> nil then
  begin
    ItemData := GetItemData(Node);
    if ItemData<>Nil then
      Result := ItemData^.JSONData
    else
      Result := Nil;
  end
  else
    Result := nil;
end;

procedure TSOGrid.DoInitNode(ParentNode, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  ItemData: PSOItemData;
begin
  ItemData := GetItemData(Node);
  if ItemData<>Nil then
  begin
    //This increment the refcount of the interface
    ItemData^.JSONData := FData.AsArray[Node^.Index];
    Node^.CheckType := ctCheckBox;
  end;
  inherited DoInitNode(ParentNode, Node, InitStates);
end;

function TSOGrid.GetCellStrValue(N: PVirtualNode;
  FieldName: string; Default: string = ''): string;
var
   idata:ISuperObject;
begin
  idata := GetCellData(N,FieldName);
  if iData=Nil then
     Result := Default
  else if iData.DataType=stArray then
       Result := join(',',idata)
  else
       Result:=UTF8Encode(idata.AsString);
end;

function TSOGrid.DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer;
var
  n1,n2,d1,d2: ISuperObject;
  propname:String;
begin
  Result := 0;
  if Assigned(OnCompareNodes) then
    OnCompareNodes(Self, Node1, Node2, Column, Result)
  else
  begin
    n1 := GetData(Node1);
    n2 := GetData(Node2);

    if (Column>=0) and (n1<>Nil) and (n2<>Nil) then
    begin
      propname:=TSOGridColumn(Header.Columns[column]).PropertyName;
      d1 := n1[propname];
      d2 := n2[propname];
      if (d1<>Nil) and (d2<>Nil) then
        Result := CompareText(d1.AsString,d2.AsString)
      else
        Result:=0;
    end
    else
      Result := 0;
  end;
end;

procedure TSOGrid.DoHeaderClick(HitInfo: TVTHeaderHitInfo);
var
  Direction : TSortDirection;
begin
  if Assigned(OnHeaderClick) then
    OnHeaderClick(Header, HitInfo)
  else
  begin
    // Descending order with pressed Shift, otherwise Ascending
    // Or you can save Direction or use
    // MyTree.Header.SortDirection and MyTree.Header.SortColumn
    // to get automatically Descending/Ascending sorting
    // by only clicking on header

    if ssShift in HitInfo.Shift
    then
      Direction := sdDescending
    else
      Direction := sdAscending;

    // Sort all columns except the second
    Header.SortColumn := HitInfo.Column;
    Header.SortDirection := Direction;
    SortTree(HitInfo.Column, Direction);
  end;
end;

procedure TSOGrid.DoPopupMenu(Node: PVirtualNode; Column: TColumnIndex;
  const Position: TPoint);
begin
  inherited DoPopupMenu(Node, Column, Position);
end;

procedure TSOGrid.Clear;
begin
  inherited Clear;
  FData := Nil;
end;

end.

