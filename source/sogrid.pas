{**********************************************************************
 Package pltis_sogrid.pkg
 This unit is based on package virtualtreesextra work from CodeTyphon Studio  (http://www.pilotlogic.com/)
***********************************************************************}

unit sogrid;

{$mode delphi}{$H+}

//{$packset 1}

//{$mode objfpc}{$H+}
//{$mode delphi}

interface

uses
  Classes, SysUtils, VirtualTrees, Controls,
  Windows,SuperObject, Menus, Graphics, Clipbrd, LCLType, Dialogs,LMessages,StdCtrls,Types;

type

  TSOGrid = class;

  TSODataEvent = (deFieldChange, deDataSetChange,
    deUpdateRecord, deUpdateState,deFieldListChange);

  TSODataChangeEvent = procedure (EventType:TSODataEvent;Row:ISuperObject;PropertyName:String;OldData,Newdata:ISuperObject) of object;
  TSuperObjectRowEvent = procedure (ARow:ISuperObject) of object;

  TSOUpdateStatus = (usUnmodified, usModified, usInserted, usDeleted);
  TSOUpdateStatusSet = SET OF TSOUpdateStatus;

  TSOUpdateMode = (upWhereAll, upWhereChanged, upWhereKeyOnly);
  TSOResolverResponse = (rrSkip, rrAbort, rrMerge, rrApply, rrIgnore);

  ISODataView = interface
    ['{2DF865FF-684D-453E-A9F0-7D7307DD0BDD}']
    procedure NotifyChange(EventType:TSODataEvent;Row:ISuperObject;PropertyName:String;OldData,Newdata:ISuperObject);
  end;

  { TSODataSource }

  TSODataSource = class(TComponent)
  private
    FData: ISuperObject;
    FDataViews: TList;
    FEnabled: Boolean;
    FOnDataChange: TSODataChangeEvent;
    FOnNewRecord: TSuperObjectRowEvent;
    FOnStateChange: TNotifyEvent;
    procedure DistributeEvent(Event: TSODataEvent; Info: Ptrint);
    Procedure ProcessEvent(Event : TSODataEvent; Info : Ptrint);
    procedure SetData(AData: ISuperObject);
    procedure SetEnabled(Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // List of visual components to be notified of data changes
    property DataViews:TList read FDataViews write FDataViews;

    // Data store
    property Data:ISuperObject read FData write SetData;

    // add a component to the list of notified objects
    procedure RegisterView(AComponent:TComponent);
    procedure UnregisterView(AComponent:TComponent);
    procedure NotifyChange(EventType:TSODataEvent;Row:ISuperObject=Nil;PropertyName:String = '';OldData:ISuperObject=Nil;Newdata:ISuperObject=Nil);

    procedure LoadDataset; Virtual;
    procedure Emptydataset; Virtual;

    function  AppendRecord:ISuperObject;Virtual;
    procedure DeleteRecord(row:ISuperObject); Virtual;
    procedure UpdateRecord(row:ISuperObject;PropertyName:String;NewValue:ISuperObject);Virtual;

    procedure LoadFromFile(Filename:String); Virtual;
    procedure SaveToFile(Filename:String); Virtual;

    function ChangeCount:Integer; virtual;
    procedure UndolastChange; virtual;
    function ApplyUpdates:Integer; virtual;

    procedure PasteFromClipboard;

  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property OnDataChange: TSODataChangeEvent read FOnDataChange write FOnDataChange;

    property OnNewRecord: TSuperObjectRowEvent read FOnNewRecord write FOnNewRecord;
  end;

  { TSOGridColumn }
  TSOGridColumn = class(TVirtualTreeColumn)
  private
    FPropertyName: string;
    procedure SetPropertyName(const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property PropertyName: string read FPropertyName write SetPropertyName;
  end;

  TSOHeaderPopupOption = (
    poOriginalOrder, // Show menu items in original column order as they were added to the tree.
    poAllowHideAll   // Allows to hide all columns, including the last one.
  );
  TSOHeaderPopupOptions = set of TSOHeaderPopupOption;

  TAddPopupItemType = (
    apNormal,
    apDisabled,
    apHidden
  );

  TAddHeaderPopupItemEvent = procedure(const Sender: TBaseVirtualTree; const Column: TColumnIndex;
    var Cmd: TAddPopupItemType) of object;
  TColumnChangeEvent = procedure(const Sender: TBaseVirtualTree; const Column: TColumnIndex; Visible: Boolean) of object;

  TSOMenuItem = TMenuItem;

  TSOHeaderPopupMenu = class(TPopupMenu)
  private
    FOptions: TSOHeaderPopupOptions;

    FOnAddHeaderPopupItem: TAddHeaderPopupItemEvent;
    FOnColumnChange: TColumnChangeEvent;
  protected
    procedure DoAddHeaderPopupItem(const Column: TColumnIndex; out Cmd: TAddPopupItemType); virtual;
    procedure DoColumnChange(Column: TColumnIndex; Visible: Boolean); virtual;
    procedure OnMenuItemClick(Sender: TObject);
  public
    procedure Popup(x, y: Integer); override;
  published
    property Options: TSOHeaderPopupOptions read FOptions write FOptions default [];

    property OnAddHeaderPopupItem: TAddHeaderPopupItemEvent read FOnAddHeaderPopupItem write FOnAddHeaderPopupItem;
    property OnColumnChange: TColumnChangeEvent read FOnColumnChange write FOnColumnChange;
  end;

  { TSOStringEditLink }

  // Edit support classes.
  TSOStringEditLink = class;

  TSOEdit = class(TCustomEdit)
  private
    procedure CMAutoAdjust(var Message: TLMessage); message CM_AUTOADJUST;
    procedure CMExit(var Message: TLMessage); message CM_EXIT;
    procedure CMRelease(var Message: TLMessage); message CM_RELEASE;
    procedure CNCommand(var Message: TLMCommand); message CN_COMMAND;
    procedure WMChar(var Message: TLMChar); message LM_CHAR;
    procedure WMDestroy(var Message: TLMDestroy); message LM_DESTROY;
    procedure WMGetDlgCode(var Message: TLMNoParams); message LM_GETDLGCODE;
    procedure WMKeyDown(var Message: TLMKeyDown); message LM_KEYDOWN;
  protected
    FRefLink: IVTEditLink;
    FLink: TSOStringEditLink;
    procedure AutoAdjustSize; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(Link: TSOStringEditLink); reintroduce;

    procedure Release; virtual;

    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    //property HideSelection;
    property MaxLength;
    //property OEMConvert;
    property PasswordChar;
  end;

  TSOStringEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FEdit: TSOEdit;                  // A normal custom edit control.
    procedure SetEdit(const Value: TSOEdit);
  protected
    FTree: TSOGrid; // A back reference to the tree calling.
    FNode: PVirtualNode;             // The node to be edited.
    FColumn: TColumnIndex;           // The column of the node.
    FAlignment: TAlignment;
    FTextBounds: TRect;              // Smallest rectangle around the text.
    FStopping: Boolean;              // Set to True when the edit link requests stopping the edit action.
  public
    constructor Create;
    destructor Destroy; override;

    function BeginEdit: Boolean; virtual; stdcall;
    function CancelEdit: Boolean; virtual; stdcall;
    property Edit: TSOEdit read FEdit write SetEdit;
    function EndEdit: Boolean; virtual; stdcall;
    function GetBounds: TRect; virtual; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; virtual; stdcall;
    procedure ProcessMessage(var Message: TLMessage); virtual; stdcall;
    procedure SetBounds(R: TRect); virtual; stdcall;
  end;

  { TSOGrid }
  TSOGridGetText = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode;
    RowData, CellData: ISuperObject; Column: TColumnIndex; TextType: TVSTTextType;
    var CellText: string) of object;

  TSOGrid = class(TCustomVirtualStringTree,ISODataView)
  private
    FTextFound: boolean;
    FindDlg: TFindDialog;
    ReplaceDialog: TReplaceDialog;

    FColumnToFind: integer;
    FStartSearchNode: PVirtualNode;
    FTextToFind: string;

    FData: ISuperObject;
    FItemDataOffset: integer;
    FOnGetText: TSOGridGetText;

    FDatasource: TSODataSource;

    FDefaultPopupMenu: TPopupMenu;
    FMenuFilled: boolean;
    HMUndo, HMRevert: HMENU;
    HMFind, HMFindNext, HMReplace: HMENU;
    HMCut, HMCopy, HMPast, HMFindReplace: HMENU;
    HMInsert, HMDelete, HMSelAll: HMENU;
    HMExcel, HMPrint: HMENU;
    HMCollAll, HMExpAll: HMENU;
    HMCustomize: HMENU;

    function FocusedPropertyName: String;
    function GetData: ISuperObject;
    function GetSettings: ISuperObject;

    procedure SetColumnToFind(AValue: integer);
    procedure SetData(const Value: ISuperObject);
    procedure SetDatasource(AValue: TSODataSource);
    procedure SetOptions(const Value: TStringTreeOptions);
    function GetOptions: TStringTreeOptions;
    procedure SetSettings(AValue: ISuperObject);

    procedure WMKeyDown(var Message: TLMKeyDown); message LM_KEYDOWN;


  protected
    procedure WndProc(var Message: TLMessage); override;

    // after cell editing to set data in Superobject
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex;
      const AText: string); override;

    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string); override;
    procedure DoInitNode(ParentNode, Node: PVirtualNode;
      var InitStates: TVirtualNodeInitStates); override;
    function GetColumnClass: TVirtualTreeColumnClass; override;
    function GetOptionsClass: TTreeOptionsClass; override;
    function GetItemData(Node: PVirtualNode): Pointer;
    procedure DoFreeNode(Node: PVirtualNode); override;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): integer;
      override;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;

    function DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex
      ): IVTEditLink; override;

    //Gestion menu standard
    procedure FillMenu(LocalMenu: TPopupMenu);
    procedure DoEnter; override;

    procedure PrepareCell(var PaintInfo: TVTPaintInfo;
      WindowOrgX, MaxWidth: integer); override;

    //gestion affichage multiselection
    procedure DoBeforeCellPaint(ACanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      var ContentRect: TRect); override;
    procedure DoTextDrawing(var PaintInfo: TVTPaintInfo; const AText: string;
      CellRect: TRect; DrawFormat: cardinal); override;


    function FindText(Txt: string): PVirtualNode;
    procedure FindDlgFind(Sender: TObject);

    procedure DoFindText(Sender: TObject);
    procedure DoFindNext(Sender: TObject);
    procedure DoFindReplace(Sender: TObject);

    procedure DoUndoLastUpdate(Sender: TObject); virtual;
    procedure DoRevertRecord(Sender: TObject); virtual;
    procedure DoExportExcel(Sender: TObject); virtual;
    procedure DoCopyToClipBoard(Sender: TObject); virtual;
    procedure DoCutToClipBoard(Sender: TObject); virtual;
    procedure DoDeleteRows(Sender: TObject); virtual;
    procedure DoPaste(Sender: TObject); virtual;
    procedure DoSelectAllRows(Sender: TObject); virtual;
    procedure DoPrint(Sender: TObject); virtual;
    procedure DoCustomizeColumns(Sender: TObject); virtual;
    procedure DoExpandAll(Sender: TObject); virtual;
    procedure DoCollapseAll(Sender: TObject); virtual;


    property ColumnToFind: integer read FColumnToFind write SetColumnToFind;
    property TextToFind: string read FTextToFind write FTextToFind;
    property TextFound: boolean read FTextFound write FTextFound;

    function DoKeyAction(var CharCode: Word; var Shift: TShiftState): Boolean; override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetNodeSOData(Node: PVirtualNode): ISuperObject;
    procedure LoadData;
    property Data: ISuperObject read GetData write SetData;
    function GetCellData(N: PVirtualNode; FieldName: string;
      Default: ISuperObject = nil): ISuperObject;
    function GetCellStrValue(N: PVirtualNode; FieldName: string;
      Default: string = ''): string;
    function SelectedRows: ISuperObject;
    function NodesForData(sodata: ISuperObject): TNodeArray;
    procedure InvalidateFordata(sodata: ISuperObject);
    procedure Clear; override;

    // SODatasource events handling
    procedure NotifyChange(EventType:TSODataEvent;Row:ISuperObject;PropertyName:String;OldData,Newdata:ISuperObject);
    function FocusedColumnObject:TSOGridColumn;

    // sauvegarde et restauration des customisations utilisateurs
    property Settings: ISuperObject read GetSettings write SetSettings;
    procedure SaveSettingsToIni(inifilename: string);
    procedure LoadSettingsFromIni(inifilename: string);

    function FindColumnByPropertyName(propertyname: string): TSOGridColumn;

    //Ajouter les colonnes en s'inspirant du contenu Data
    procedure CreateColumnsFromData(FitWidth:Boolean);

    function ContentAsCSV(Source: TVSTTextSourceType; const Separator: String
      ): Utf8String;

  published
    property OnGetText: TSOGridGetText read FOnGetText write FOnGetText;

    property Datasource: TSODataSource read FDataSource write SetDatasource;

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
    property OnEditing;
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

uses soutils, base64, IniFiles,LCLIntf,messages,forms;

resourcestring
  GSConst_NoRecordFind = 'Pas d''enregistrement trouvé';
  GSConst_PrintOn = 'Imprimé le';
  GSConst_Page = 'Page';

  GSConst_Confirmation = 'Confirmation';
  GSConst_UndoLastUpdate = 'Défaire la dernière modification';
  GSConst_RevertRecord = 'Revenir à la version initiale de la ligne';
  GSConst_Find = 'Rechercher...';
  GSConst_FindNext = 'Rechercher le suivant';
  GSConst_FindReplace = 'Remplacer...';
  GSConst_Copy = 'Copier';
  GSConst_Cut = 'Couper';
  GSConst_Paste = 'Coller';
  GSConst_Insert = 'Insérer';
  GSConst_Delete = 'Effacer';
  GSConst_DeleteRows = 'Effacer les lignes sélectionnées';
  GSConst_ConfDeleteRow = 'Confirmer la suppression des lignes ?';
  GSConst_SelectAll = 'Selectionner toutes les lignes';
  GSConst_ExportSelectedExcel = 'Exporter la sélection vers un fichier Excel...';
  GSConst_ExportAllExcel = 'Exporter toutes les lignes vers un fichier Excel...';
  GSConst_Print = 'Imprimer...';
  GSConst_ExpandAll = 'Tout déplier';
  GSConst_CollapseAll = 'Tour replier';
  GSConst_CustomizeColumns = 'Personnaliser les colonnes affichées...';

type
  TSOItemData = record
    JSONData: ISuperObject;
  end;
  PSOItemData = ^TSOItemData;


{ TSODataSource }

procedure TSODataSource.DistributeEvent(Event: TSODataEvent; Info: Ptrint);
begin

end;

procedure TSODataSource.ProcessEvent(Event: TSODataEvent; Info: Ptrint);
begin

end;

procedure TSODataSource.SetData(AData: ISuperObject);
begin
  if Adata = FData then
    Exit;
  FData := AData;
  NotifyChange(deDataSetChange,Nil,'',Nil,AData);
end;

procedure TSODataSource.SetEnabled(Value: Boolean);
begin
  if Value = FEnabled then
      Exit;
  NotifyChange(deUpdateState ,Nil,'',Nil,Nil);
end;

constructor TSODataSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataViews := TList.Create;
  FEnabled:=True;
end;

destructor TSODataSource.Destroy;
begin
  if Assigned(FDataViews) then
    FreeAndNil(FDataViews);
  inherited Destroy;
end;

procedure TSODataSource.RegisterView(AComponent: TComponent);
begin
  if DataViews.IndexOf(AComponent)<0 then
    DataViews.Add(AComponent);
end;

procedure TSODataSource.UnregisterView(AComponent: TComponent);
begin
  if Assigned(FDataviews) then
    FDataViews.Remove(AComponent);
end;

procedure TSODataSource.NotifyChange(EventType: TSODataEvent;
  Row: ISuperObject; PropertyName: String; OldData: ISuperObject;
  Newdata: ISuperObject);
var
  i:integer;
begin
  if Enabled then
    for i:=0 to DataViews.Count-1 do
      (TInterfacedObject(DataViews[i]) as ISODataView).NotifyChange(EventType,Row,PropertyName,OldData,Newdata);
end;

procedure TSODataSource.LoadDataset;
begin
  Data := TSuperObject.Create(stArray);
end;

procedure TSODataSource.Emptydataset;
begin
  Data := TSuperObject.Create(stArray);
end;

function TSODataSource.AppendRecord: ISuperObject;
begin
  if not Assigned(Data) then
      Emptydataset;

  result := TSuperObject.Create;
  if Assigned(OnNewRecord) then
      OnNewRecord(result);
  data.AsArray.Add(result);
  NotifyChange(deDataSetChange,result);
end;

procedure TSODataSource.DeleteRecord(row: ISuperObject);
var
  i:Integer;
begin
  for i:=data.AsArray.Length-1 downto 0 do
    if Data.AsArray[i]=row then
      Data.AsArray.Delete(i);
  NotifyChange(deDataSetChange);
end;

procedure TSODataSource.UpdateRecord(row: ISuperObject; PropertyName: String;
  NewValue: ISuperObject);
var
  oldvalue:ISuperObject;
begin
  oldValue := Row[PropertyName];
  Row[PropertyName] := NewValue;
  NotifyChange(deFieldChange,Row,PropertyName,oldvalue,NewValue);
end;

procedure TSODataSource.LoadFromFile(Filename: String);
begin
  data := TSuperObject.ParseFile(Filename,False);
  NotifyChange(deDataSetChange);
end;

procedure TSODataSource.SaveToFile(Filename: String);
begin
  if Assigned(Data) then
      Data.SaveTo(Filename);
end;

function TSODataSource.ChangeCount: Integer;
begin
  Result := 0;
end;

procedure TSODataSource.UndolastChange;
begin
  raise Exception.Create('Not implemented');;
end;

// return count of not applied records.
function TSODataSource.ApplyUpdates: Integer;
begin
  raise Exception.Create('Not implemented');;
end;

procedure TSODataSource.PasteFromClipboard;
var
  newData:ISuperObject;
begin
    try
      if Clipboard.HasFormat(ClipbrdJson) then
        newData :=ClipboardSOData
      else
        newData := SO(Clipboard.AsText);
      if (newData.DataType = stArray) and (newData.AsArray.Length>0) and (newData.AsArray[0].DataType=stObject) then
        Data := newData
      else
      if (newData.DataType = stObject) then
      begin
        Data := TSuperObject.Create(stArray);
        Data.AsArray.Add(SO(Clipboard.AsText));
      end;
      NotifyChange(deDataSetChange);
    except
      Data := TSuperObject.Create(stArray);
      raise;
    end;
end;

{ TSOGridColumn }
procedure TSOGridColumn.SetPropertyName(const Value: string);
begin
  if FPropertyName = Value then
    exit;
  if (Text = '') or (Text = FPropertyName) then
    Text := Value;
  FPropertyName := Value;
end;

procedure TSOGridColumn.Assign(Source: TPersistent);

var
  OldOptions: TVTColumnOptions;

begin
  if Source is TSOGridColumn then
  begin
    inherited Assign(Source);
    PropertyName:=TSOGridColumn(Source).PropertyName;
  end
  else
    inherited Assign(Source);
end;

type
  TVirtualTreeCast = class(TBaseVirtualTree); // Necessary to make the header accessible.

//----------------- TSOHeaderPopupMenu. ---------------------------------------------------------------------------------

procedure TSOHeaderPopupMenu.DoAddHeaderPopupItem(const Column: TColumnIndex; out Cmd: TAddPopupItemType);

begin
  Cmd := apNormal;
  if Assigned(FOnAddHeaderPopupItem) then
    FOnAddHeaderPopupItem(TVirtualTreeCast(PopupComponent), Column, Cmd);
end;


procedure TSOHeaderPopupMenu.DoColumnChange(Column: TColumnIndex; Visible: Boolean);

begin
  if Assigned(FOnColumnChange) then
    FOnColumnChange(TVirtualTreeCast(PopupComponent), Column, Visible);
end;


procedure TSOHeaderPopupMenu.OnMenuItemClick(Sender: TObject);

begin
  if Assigned(PopupComponent) and (PopupComponent is TBaseVirtualTree) then
    with TSOMenuItem(Sender),
      TVirtualTreeCast(PopupComponent).Header.Columns.Items[Tag] do
    begin
      if Checked then
        Options := Options - [coVisible]
      else
        Options := Options + [coVisible];

       DoColumnChange(TSOMenuItem(Sender).Tag, not Checked);
    end;
end;


procedure TSOHeaderPopupMenu.Popup(x, y: Integer);

var
  I: Integer;
  ColPos: TColumnPosition;
  ColIdx: TColumnIndex;

  NewMenuItem: TSOMenuItem;
  Cmd: TAddPopupItemType;

  VisibleCounter: Cardinal;
  VisibleItem: TSOMenuItem;

begin
  if Assigned(PopupComponent) and (PopupComponent is TBaseVirtualTree) then
  begin
    // Delete existing menu items.
    I := Items.Count;
    while I > 0 do
    begin
      Dec(I);
      Items[I].Free;
    end;

    // Add column menu items.
    with TVirtualTreeCast(PopupComponent).Header do
    begin
      if hoShowImages in Options then
        Self.Images := Images
      else
        // Remove a possible reference to image list of another tree previously assigned.
        Self.Images := nil;
      VisibleItem := nil;
      VisibleCounter := 0;
      for ColPos := 0 to Columns.Count - 1 do
      begin
        if poOriginalOrder in FOptions then
          ColIdx := ColPos
        else
          ColIdx := Columns.ColumnFromPosition(ColPos);

        with Columns[ColIdx] as TSOGridColumn do
        begin
          if coVisible in Options then
            Inc(VisibleCounter);
          DoAddHeaderPopupItem(ColIdx, Cmd);
          if Cmd <> apHidden then
          begin
            NewMenuItem := TSOMenuItem.Create(Self);
            NewMenuItem.Tag := ColIdx;
            NewMenuItem.Caption := Text+' ('+PropertyName+')';
            NewMenuItem.Hint := Hint;
            NewMenuItem.ImageIndex := ImageIndex;
            NewMenuItem.Checked := coVisible in Options;
            NewMenuItem.OnClick := OnMenuItemClick;
            if Cmd = apDisabled then
              NewMenuItem.Enabled := False
            else
              if coVisible in Options then
                VisibleItem := NewMenuItem;
            Items.Add(NewMenuItem);
          end;
        end;
      end;

      // Conditionally disable menu item of last enabled column.
      if (VisibleCounter = 1) and (VisibleItem <> nil) and not (poAllowHideAll in FOptions) then
        VisibleItem.Enabled := False;
    end;
  end;

  inherited;
end;

{ TSOGrid }
function TSOGrid.GetItemData(Node: PVirtualNode): Pointer;
begin
  if (Node = nil) or (FItemDataOffset <= 0) then
    Result := nil
  else
    Result := PByte(Node) + FItemDataOffset;
end;

procedure TSOGrid.DoFreeNode(Node: PVirtualNode);
var
  idata: PSOItemData;
begin
  idata := GetItemData(Node);
  //Be sure to release the interface ...
  if idata <> nil then
  begin
    idata^.JSONData := nil;

  end;
  inherited DoFreeNode(Node);
end;

function TSOGrid.GetOptions: TStringTreeOptions;
begin
  Result := TStringTreeOptions(inherited TreeOptions);
end;

function TSOGrid.FindColumnByPropertyName(propertyname: string): TSOGridColumn;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Header.Columns.Count - 1 do
  begin
    if TSOGridColumn(Header.Columns[i]).PropertyName = propertyname then
    begin
      Result := TSOGridColumn(Header.Columns[i]);
      Break;
    end;
  end;
end;

procedure TSOGrid.CreateColumnsFromData(FitWidth: Boolean);
var
  values,prop,properties,Row,propname:ISuperObject;
  col : TSOGridColumn;
  i:Integer;
  NewColStartIdx:Integer;
begin
  NewColStartIdx := NoColumn;
  BeginUpdate;
  try
    properties := TSuperObject.Create(stArray);
    values := TSuperObject.Create(stArray);

    for row in data do
    begin
      for propname in row.AsObject.GetNames do
      begin
        col := FindColumnByPropertyName(propname.AsString);
        if col = Nil then
        begin
          prop := row[propname.AsString];
          col :=Header.Columns.Add as TSOGridColumn;
          NewColStartIdx:=col.Index;
          col.Text:=propname.AsString;
          col.PropertyName:=propname.AsString;
          col.Width:= 100;
        end;
        {else
          if col.Width< 3*Length(prop.AsString) then
            col.Width := Length(prop.AsString);}
      end;
      values := TSuperObject.Create(stArray);
      for i:=0 to Header.Columns.Count-1 do
      begin
        if coVisible in Header.Columns[i].Options then
          values.AsArray.Add('"'+Row.S[TSOGridColumn(Header.Columns[i]).PropertyName]+'"');
      end;
    end;
  finally
    if FitWidth and (NewColStartIdx<>NoColumn)  then
        Header.AutoFitColumns(False,smaUseColumnOption,NewColStartIdx);
    EndUpdate;
  end;
end;

procedure TSOGrid.SetSettings(AValue: ISuperObject);
var
  i: integer;
  gridcol: TSOGridColumn;
  prop, column, columns: ISuperObject;

begin
  if AValue <> nil then
  begin
    if AValue.AsObject.Find('columns', columns) then
    begin
      for column in Columns do
      begin
        gridcol := FindColumnByPropertyName(column.S['propertyname']);
        if gridcol <> nil then
        begin
          if column.AsObject.Find('position', prop) then
            gridcol.Position := prop.AsInteger;
          if column.AsObject.Find('width', prop) then
            gridcol.Width := prop.AsInteger;
          if column.AsObject.Find('visible', prop) then
            if prop.AsBoolean then
              gridcol.Options := gridcol.Options + [coVisible]
            else
              gridcol.Options := gridcol.Options - [coVisible];
        end;
      end;
    end;
    if AValue.AsObject.Find('sortcolumn', prop) then
      Header.SortColumn := prop.AsInteger;

    if AValue.AsObject.Find('sortdirection', prop) then
      Header.SortDirection := TSortDirection(prop.AsInteger);

    if AValue.AsObject.Find('headerheight', prop) then
      Header.Height := prop.AsInteger;

    if AValue.AsObject.Find('defaultnodeheight', prop) then
      DefaultNodeHeight := prop.AsInteger;

  end;
end;

procedure TSOGrid.WMKeyDown(var Message: TLMKeyDown);

var
  Shift: TShiftState;
  KeyState: TKeyboardState;
  Buffer: array[0..1] of Char;
  amsg:TLMessage;

begin
  // manage immediate editor
  with Message do
  begin
    Shift := KeyDataToShiftState(KeyData);
    GetKeyboardState(KeyState);
    // Avoid conversion to control characters. We have captured the control key state already in Shift.
    KeyState[VK_CONTROL] := 0;
    if (ToASCII(Message.CharCode, (Message.KeyData shr 16) and 7, KeyState, @Buffer, 0) > 0)
        and (Shift * [ssCtrl, ssAlt] = []) and (CharCode >= 32) then
    begin
      //case Buffer[0] of
      EditColumn := FocusedColumn;
      DoEdit;
      amsg.msg:=WM_CHAR;
      amsg.wParam:=ord(Buffer[0]);
      amsg.lParam:=0;
      EditLink.ProcessMessage( amsg);
    end
    else
      inherited;
  end;
end;

procedure TSOGrid.LoadData;
var
  row: ISuperObject;
  Node: PVirtualNode;
  ItemData: PSOItemData;
begin
  if (Data = nil) or (Data.AsArray = nil) then
    inherited Clear
  else
  begin
    //todo handle object
    BeginUpdate;
    try
      inherited Clear;
      RootNodeCount := Data.AsArray.Length;
    finally
      EndUpdate;
    end;
  end;
end;

function TSOGrid.GetCellData(N: PVirtualNode; FieldName: string;
  Default: ISuperObject=Nil): ISuperObject;
var
  pdata: PSOItemData;
  idata: ISuperObject;
begin
  if N<>Nil then
  begin
      pdata := GetItemData(N);
      if pdata <> nil then
      begin
        idata := pdata^.JSONData[FieldName];
        if idata <> nil then
          Result := idata
        else
          Result := Default;
      end
      else
        Result := Default;
  end
  else
    Result := Default;
end;

procedure TSOGrid.SetData(const Value: ISuperObject);
begin
  //redirect to datasource if defined
  if Assigned(FDatasource) then
    FDatasource.Data := Value
  else
  begin
    if FData = Value then
      exit;
    LoadData;
    FData := Value;
  end;
end;

procedure TSOGrid.SetDatasource(AValue: TSODataSource);
begin
  if FDataSource = AValue then
    Exit;
  if Assigned(FDatasource) then
    FDataSource.UnregisterView(Self);
  FDataSource := AValue;
  if Assigned(FDatasource) then
  begin
    FDataSource.RegisterView(Self);
    // be sure to release interface
    FData := Nil;
    LoadData;
  end;
end;

function TSOGrid.GetSettings: ISuperObject;
var
  i: integer;
  col: ISuperObject;
begin
  Result := TSuperObject.Create;
  Result.I['sortcolumn'] := Header.SortColumn;
  Result.I['sortdirection'] := integer(Header.SortDirection);
  Result.I['headerheight'] := integer(Header.Height);
  Result.I['defaultnodeheight'] := integer(DefaultNodeHeight);
  Result['columns'] := TSuperObject.Create(stArray);
  for i := 0 to Header.Columns.Count - 1 do
  begin
    col := TSuperObject.Create;
    Result['columns'].AsArray.Add(col);
    col.S['propertyname'] := TSOGridColumn(Header.Columns[i]).PropertyName;
    col.S['text'] := Header.Columns[i].Text;
    col.I['position'] := Header.Columns[i].Position;
    col.I['width'] := Header.Columns[i].Width;
    col.B['visible'] := (coVisible in Header.Columns[i].Options);
  end;
end;

procedure TSOGrid.SetColumnToFind(AValue: integer);
begin
  if FColumnToFind = AValue then
    Exit;
  FColumnToFind := AValue;
end;

procedure TSOGrid.SetOptions(const Value: TStringTreeOptions);
begin
  TreeOptions.Assign(Value);
end;

procedure TSOGrid.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  ItemData: PSOItemData;
  RowData, CellData: ISuperObject;
begin
  RowData := nil;
  CellData := nil;
  if Node <> nil then
  begin
    ItemData := GetItemData(Node);
    if ItemData <> nil then
    begin
      RowData := ItemData^.JSONData;
      if RowData <> nil then
      begin
        if (Column >= 0) and Header.Columns.IsValidColumn(Column) then
          CellData := RowData[TSOGridColumn(Header.Columns.Items[Column]).PropertyName]
        else
          CellData := RowData[DefaultText];

        if CellData <> nil then
          CellText := UTF8Encode(CellData.AsString)
        else
          CellText := DefaultText;
      end;
    end
    else
      CellText := 'uninitialized';
  end
  else
    CellText := '';
  if Assigned(FOnGetText) then
    FOnGetText(Self, Node, RowData, CellData, Column, TextType, CellText);
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
var
  mi: TMenuItem;
begin
  inherited Create(AOwner);
  DefaultText := '';
  FItemDataOffset := AllocateInternalDataArea(SizeOf(TSOItemData));

  WantTabs:=True;
  TabStop:=True;

  with TreeOptions do
  begin
    PaintOptions := PaintOptions - [toShowRoot] +
      [toAlwaysHideSelection, toShowHorzGridLines, toShowVertGridLines, toHideFocusRect];
    SelectionOptions := SelectionOptions + [toExtendedFocus, toSimpleDrawSelection{,toRightClickSelect}];
    MiscOptions := MiscOptions + [toEditable, toGridExtensions, toFullRowDrag] -
      [toWheelPanning];

    AutoOptions := AutoOptions + [toAutoSort];
  end;

  Header.Options := [hoColumnResize, hoDblClickResize, hoDrag,
    hoShowSortGlyphs, hoVisible];
  Header.Style := hsFlatButtons;

  // Initialisation de la boite de dialogue de recherche
  FindDlg := TFindDialog.Create(nil);
  FindDlg.OnFind := FindDlgFind;
  FindDlg.Options := FindDlg.Options + [frHideMatchCase,frHideEntireScope,frEntireScope];

  Header.PopupMenu :=  TSOHeaderPopupMenu.Create(Self);
  Header.PopupMenu.PopupComponent := Self;

end;


procedure TSOGrid.FillMenu(LocalMenu: TPopupMenu);

  function AddItem(ACaption: string; AShortcut: TShortCut; AEvent: TNotifyEvent): HMENU;
  var
    AMI: TMenuItem;
  begin
    AMI := TMenuItem.Create(LocalMenu);
    with AMI do
    begin
      Caption := ACaption;
      ShortCut := AShortcut;
      OnClick := AEvent;
    end;
    LocalMenu.Items.Add(AMI);
    Result := AMI.Handle;
  end;

begin
  if not FMenuFilled then
    try

      if (LocalMenu.Items.Count > 0) then
        AddItem('-', 0, nil);

      {if (HMUndo = 0) then
        HMUndo := AddItem(GSConst_UndoLastUpdate, ShortCut(Ord('Z'), [ssCtrl]),
          @DoUndoLastUpdate);
      if (HMRevert = 0) then
        HMRevert := AddItem(GSConst_RevertRecord, 0, @DoRevertRecord);
      AddItem('-', 0, nil);}
      HMFind := AddItem(GSConst_Find, ShortCut(Ord('F'), [ssCtrl]), DoFindText);
      HMFindNext := AddItem(GSConst_FindNext, VK_F3, DoFindNext);
      {HMFindReplace := AddItem(GSConst_FindReplace, ShortCut(Ord('H'), [ssCtrl]),
        @DoFindReplace);}
      AddItem('-', 0, nil);
      if (toEditable in TreeOptions.MiscOptions) then
        HMCut := AddItem(GSConst_Cut, ShortCut(Ord('X'), [ssCtrl]), DoCutToClipBoard);
      HMCopy := AddItem(GSConst_Copy, ShortCut(Ord('C'), [ssCtrl]), DoCopyToClipBoard);
      if (toEditable in TreeOptions.MiscOptions) then
        HMPast := AddItem(GSConst_Paste, ShortCut(Ord('V'), [ssCtrl]), DoPaste);
      AddItem('-', 0, nil);
      if (toEditable in TreeOptions.MiscOptions) then
        HMDelete := AddItem(GSConst_Delete, ShortCut(VK_DELETE, [ssCtrl]), DoDeleteRows);
      if toMultiSelect in TreeOptions.SelectionOptions then
        HMSelAll := AddItem(GSConst_SelectAll, ShortCut(Ord('A'), [ssCtrl]), DoSelectAllRows);
      AddItem('-', 0, nil);
      if (toMultiSelect in TreeOptions.SelectionOptions) then
        HMExcel := AddItem(GSConst_ExportSelectedExcel, 0, DoExportExcel)
      else
        HMExcel := AddItem(GSConst_ExportAllExcel, 0, DoExportExcel);
      {if (HMPrint = 0) then
        HMPrint := AddItem(GSConst_Print, ShortCut(Ord('P'), [ssCtrl]), @DoPrint);
      AddItem('-', 0, nil);
      HMExpAll := AddItem(GSConst_ExpandAll, Shortcut(Ord('E'), [ssCtrl, ssShift]),
        @DoExpandAll);
      HMCollAll := AddItem(GSConst_CollapseAll, Shortcut(Ord('R'), [ssCtrl, ssShift]),
        @DoCollapseAll);}
      AddItem('-', 0, nil);
      HMCustomize := AddItem(GSConst_CustomizeColumns, 0, DoCustomizeColumns);
    finally
      FMenuFilled := True;
    end;
end;

procedure TSOGrid.DoEnter;
begin
  if (PopupMenu = nil) then
    PopupMenu := TPopupMenu.Create(Self);
  FillMenu(PopupMenu);
  inherited DoEnter;
end;


destructor TSOGrid.Destroy;
begin
  FData := Nil;
  if Assigned(FDatasource) then
    FDatasource.UnregisterView(Self);
  inherited Destroy;
end;

function TSOGrid.GetNodeSOData(Node: PVirtualNode): ISuperObject;
var
  ItemData: PSOItemData;
begin
  if Node <> nil then
  begin
    ItemData := GetItemData(Node);
    if ItemData <> nil then
      Result := ItemData^.JSONData
    else
      Result := nil;
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
  if (ItemData <> nil) and (Data<>Nil) and (Node^.Index < Data.AsArray.Length) then
  begin
    //This increment the refcount of the interface
    ItemData^.JSONData := Data.AsArray[Node^.Index];
    Node^.CheckType := ctCheckBox;
    //Node^.States:=Node^.States + [vsMultiline];
  end;
  inherited DoInitNode(ParentNode, Node, InitStates);
end;

function TSOGrid.GetCellStrValue(N: PVirtualNode; FieldName: string;
  Default: string = ''): string;
var
  idata: ISuperObject;
begin
  idata := GetCellData(N, FieldName);
  if iData = nil then
    Result := Default
  else if iData.DataType = stArray then
    Result := join(',', idata)
  else
    Result := UTF8Encode(idata.AsString);
end;

function TSOGrid.SelectedRows: ISuperObject;
var
  N: PVirtualNode;
  res: ISuperObject;
begin
  N := GetFirstSelected;
  Result := TSuperObject.Create(stArray);
  while (N <> nil) do
  begin
    Result.AsArray.Add(GetNodeSOData(N));
    N := GetNextSelected(N);
  end;
end;


function TSOGrid.DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex): IVTEditLink;

begin
  //Result := inherited DoCreateEditor(Node, Column);
  // Enable generic label editing support if the application does not have own editors.
  //if Result = nil then
  Result := TSOStringEditLink.Create;
end;


function TSOGrid.DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): integer;
var
  n1, n2, d1, d2: ISuperObject;
  propname: string;
begin
  Result := 0;
  if Assigned(OnCompareNodes) then
    OnCompareNodes(Self, Node1, Node2, Column, Result)
  else
  begin
    n1 := GetNodeSOData(Node1);
    n2 := GetNodeSOData(Node2);

    if (Column >= 0) and (n1 <> nil) and (n2 <> nil) then
    begin
      propname := TSOGridColumn(Header.Columns[column]).PropertyName;
      d1 := n1[propname];
      d2 := n2[propname];
      if (d1 <> nil) and (d2 <> nil) then
        Result := CompareText(d1.AsString, d2.AsString)
      else
        Result := 0;
    end
    else
      Result := 0;
  end;
end;

procedure TSOGrid.DoHeaderClick(HitInfo: TVTHeaderHitInfo);
var
  Direction: TSortDirection;
begin
  if Assigned(OnHeaderClick) then
    OnHeaderClick(Header, HitInfo)
  else
  if (HitInfo.Shift=[]) and (HitInfo.Button = mbLeft) then
  begin
    if Header.SortDirection = sdAscending then
      Direction := sdDescending
    else
      Direction := sdAscending;

    Header.SortColumn := HitInfo.Column;
    Header.SortDirection := Direction;
  end;
end;

function TSOGrid.NodesForData(sodata: ISuperObject): TNodeArray;
var
  p: PVirtualNode;
begin
  SetLength(Result, 0);
  p := TopNode;
  while (p <> nil) do
  begin
    if GetNodeSOData(p) = sodata then
    begin
      SetLength(Result, length(Result) + 1);
      Result[length(Result) - 1] := p;
    end;
    p := GetNext(p);

  end;
end;

procedure TSOGrid.InvalidateFordata(sodata: ISuperObject);
var
  p: PVirtualNode;
begin
  p := TopNode;
  while (p <> nil) do
  begin
    if GetNodeSOData(p) = sodata then
      InvalidateNode(p);
    p := GetNext(p);
  end;
end;

function TSOGrid.FocusedColumnObject: TSOGridColumn;
begin
  if (FocusedColumn>=0) and Header.Columns.IsValidColumn(FocusedColumn) then
    result := TSOGridColumn(Header.Columns[FocusedColumn])
  else
    result := Nil;
end;

procedure TSOGrid.SaveSettingsToIni(inifilename: string);
var
  b64: string;
  inifile: TIniFile;
begin
  b64 := EncodeStringBase64(Settings.AsJSon);
  IniFile := TIniFile.Create(inifilename);
  try
    inifile.WriteString(Owner.Name, Name, b64);
  finally
    FreeAndNil(iniFile);
  end;
end;

procedure TSOGrid.LoadSettingsFromIni(inifilename: string);
var
  b64: string;
  inifile: TIniFile;
begin
  IniFile := TIniFile.Create(inifilename);
  try
    b64 := inifile.readString(Owner.Name, Name, '');
    if b64 <> '' then
    begin
      Settings := SO(DecodeStringBase64(b64));
    end;
  finally
    FreeAndNil(iniFile);
  end;
end;

procedure TSOGrid.DoCopyToClipBoard(Sender: TObject);
var
  st: string;
begin
  Clipboard.Open;
  try
    Clipboard.Clear;
    st := ContentToUTF8(tstSelected, ';');
    Clipboard.AddFormat(CF_Text, st[1], Length(st));

    st := SelectedRows.AsJSon(True);
    Clipboard.AddFormat(ClipbrdJson, st[1], Length(st));

  finally
    Clipboard.Close;
  end;

end;

procedure TSOGrid.DoCutToClipBoard(Sender: TObject);
begin

end;

procedure TSOGrid.DoDeleteRows(Sender: TObject);
var
  row, sel: ISuperObject;
  i: integer;

begin
  if Dialogs.MessageDlg('Confirmer', 'Confirmez-vous la suppression de ' +
    IntToStr(SelectedCount) + ' enregistrement(s) ?', mtConfirmation, mbYesNoCancel, 0) =
    mrYes then
  begin
    for sel in SelectedRows do
    begin
      for i := 0 to Data.AsArray.Length - 1 do
        if Data.AsArray[i] = sel then
        begin
          Data.AsArray.Delete(i);
          break;
        end;
    end;
    DeleteSelectedNodes;
  end;
end;

procedure TSOGrid.DoPaste(Sender: TObject);
var
  row: ISuperObject;
begin
  if Data = Nil then
    Data := TSuperObject.Create(stArray);
  for row in ClipboardSOData do
    Data.AsArray.Add(row);
  LoadData;
end;

procedure TSOGrid.DoSelectAllRows(Sender: TObject);
begin
  SelectAll(False);
end;

procedure TSOGrid.DoPrint(Sender: TObject);
begin

end;

procedure TSOGrid.DoCustomizeColumns(Sender: TObject);
begin
  Header.PopupMenu.PopUp;
end;

procedure TSOGrid.DoExpandAll(Sender: TObject);
begin
  FullExpand;
end;

procedure TSOGrid.DoCollapseAll(Sender: TObject);
begin
  FullCollapse;
end;

function TSOGrid.DoKeyAction(var CharCode: Word; var Shift: TShiftState
  ): Boolean;
var
  msg:TLMessage;
begin
  {if (Shift * [ssCtrl, ssAlt] = []) and (CharCode >= 32) then
  begin
    ToASCII(CharCode, , KeyState, @Buffer, 0);
    if (Shift = []) and Assigned(FocusedNode) and CanEdit(FocusedNode, FocusedColumn) then
    begin
      EditColumn := FocusedColumn;
      DoEdit;
      msg.msg:=WM_KEYDOWN;
      msg.wParam:=CharCode;
      msg.lParam:=0;
      msg.Result:=0;
      EditLink.ProcessMessage(msg);
      //DoStateChange([tsEditPending]);
      Result := False;
    end
    else
      Result := True;
  end
  else}
    Result:=inherited DoKeyAction(CharCode, Shift);
end;

procedure TSOGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FDatasource) then
    FDatasource := nil;
  Inherited Notification(AComponent,Operation);
end;

procedure TSOGrid.Clear;
begin
  inherited Clear;
  FData := nil;
end;

procedure TSOGrid.NotifyChange(EventType:TSODataEvent;Row:ISuperObject;PropertyName:String;OldData,Newdata:ISuperObject);
begin
  //deFieldChange, deDataSetChange,deUpdateRecord, deUpdateState,deFieldListChange
  if not (csDestroying in ComponentState) then
  begin
    if (EventType in [deUpdateRecord,deFieldChange]) and (Row<>Nil) then
      InvalidateFordata(Row)
    else if EventType in [deUpdateState,deDataSetChange] then
      LoadData
    else
      Invalidate;
  end;
end;

procedure TSOGrid.PrepareCell(var PaintInfo: TVTPaintInfo;
  WindowOrgX, MaxWidth: integer);
begin
  inherited PrepareCell(PaintInfo, WindowOrgX, MaxWidth);
end;

procedure TSOGrid.DoBeforeCellPaint(ACanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
begin
  //Pour affichage lignes multiselect en gris clair avec cellule focused en bleu
  if (CellPaintMode = cpmPaint) and (toMultiSelect in TreeOptions.SelectionOptions) and
    (vsSelected in Node^.States) then
    if (not Focused or (column <> FocusedColumn) or (Node <> FocusedNode)) then
    begin
      ACanvas.Brush.Color := clLtGray;
      ACanvas.FillRect(CellRect);
    end
    else
    if (column = FocusedColumn) then
    begin
      ACanvas.Brush.Color := Colors.SelectionRectangleBlendColor;
      ACanvas.FillRect(CellRect);
    end;
  inherited;
end;

procedure TSOGrid.DoTextDrawing(var PaintInfo: TVTPaintInfo;
  const AText: string; CellRect: TRect; DrawFormat: cardinal);
begin
  //Pour affichage lignes multiselect en gris clair avec cellule focused en bleu
  if (toMultiSelect in TreeOptions.SelectionOptions) and Focused and
    (vsSelected in PaintInfo.Node^.States) and (PaintInfo.Node = FocusedNode) and
    (PaintInfo.column = FocusedColumn) then
    PaintInfo.Canvas.Font.Color := clWhite;
  inherited;
end;

function TSOGrid.FindText(Txt: string): PVirtualNode;
begin
  TextToFind := Txt;
  FStartSearchNode := nil;
  TextFound := False;
  DoFindNext(Self);
  if TextFound then
    Result := FocusedNode;
end;

procedure TSOGrid.FindDlgFind(Sender: TObject);
begin
  if (FindDlg.FindText <> TextToFind) then
  begin
    if toFullRowSelect in TreeOptions.SelectionOptions then
      ColumnToFind := -1
    else
      ColumnToFind := FocusedColumn;

    TextToFind := FindDlg.FindText;
    //if frDown in FindDlg.Options then
    begin
      DoFindNext(Sender);
      if TextFound then
        FindDlg.CloseDialog;
    end;
    //else
    //  if DoFindLast then
    //    FindDlg.CloseDialog;
  end
  else
    //if frDown in FindDlg.Options then
    DoFindNext(Sender);
  //else
  //DoFindPrior(Sender);
end;

procedure TSOGrid.DoFindText(Sender: TObject);
begin
  FStartSearchNode := nil;
  TextFound := False;
  TextToFind:='';
  DoFindNext(Sender);
end;

procedure TSOGrid.DoFindNext(Sender: TObject);
var
  col: integer;
  p: PVirtualNode;

  function match(node: PVirtualNode; Txt: string): integer;
  var
    i: integer;
    cellTxt: string;
  begin
    txt := LowerCase(txt);
    Result := -1;
    if FColumnToFind >= 0 then
    begin
      DoGetText(node, FColumnToFind, ttNormal, cellTxt);
      if (not (frWholeWord in FindDlg.Options) and (pos(txt, LowerCase(cellTxt)) > 0)) or
        (txt = LowerCase(cellTxt)) then
      begin
        Result := FColumnToFind;
        TextFound := True;
        exit;
      end;
    end
    else
      for i := 0 to Header.Columns.Count - 1 do
      begin
        DoGetText(node, i, ttNormal, cellTxt);
        if not (frWholeWord in FindDlg.Options) and (pos(txt, LowerCase(cellTxt)) > 0) or
          (txt = LowerCase(cellTxt)) then
        begin
          TextFound := True;
          Result := i;
          exit;
        end;
      end;
  end;

begin
  //On part de la ligne en cours
	if (TextToFind = '') then
		FindDlg.Execute
	else
	try
    p := FocusedNode;
    TextFound := False;

    if p <> nil then
    begin
      //depart de recherche. teste la ligne en cours
      if (FStartSearchNode = nil) then
      begin
        FStartSearchNode := P;
        col := match(p, FTextToFind);
        if col >= 0 then
        begin
          FocusedColumn := col;
          Exit;
        end;
      end;

      //on teste a partir du suivant
      if (FindDlg <> Nil) and not (frDown in FindDlg.Options) then
        P := GetPrevious(P)
      else
        P := GetNext(P);

      while (p <> nil) and (p <> FStartSearchNode) do
      begin
        col := match(p, FTextToFind);
        if col >= 0 then
        begin
          SelectNodes(p,p,False);
          FocusedNode := p;
          FocusedColumn := col;
          SetFocus;
          Exit;
        end;

        //on teste a partir du suivant
        if (FindDlg <> Nil) and not (frDown in FindDlg.Options) then
          P := GetPrevious(P)
        else
          P := GetNext(P);

        // on reboucle sur le debut
        if p = nil then
          if (FindDlg <> Nil) and not (frDown in FindDlg.Options) then
            P := GetLast(Nil)
          else
            P := GetFirst(False);
      end;
    end;
    FStartSearchNode := nil;
    ShowMessage(GSConst_NoRecordFind);
  finally
  end;
end;

procedure TSOGrid.DoFindReplace(Sender: TObject);
begin
  if ReplaceDialog = nil then
    ReplaceDialog := TReplaceDialog.Create(Self);
  try
    ReplaceDialog.Options := [frDown, frDisableUpDown, frReplace, frReplaceAll,frEntireScope];
    //ReplaceDialog.OnReplace := ReplaceDialog1Replace;
    if EditLink<>Nil  then
      ReplaceDialog.FindText :=  TStringEditLink(EditLink).Edit.Text
    else
      ReplaceDialog.FindText :=  GetCellStrValue(FocusedNode,FocusedPropertyName);
    ReplaceDialog.Execute;
  finally
  end;

end;

function TSOGrid.FocusedPropertyName:String;
begin
  Result := TSOGridColumn(Header.Columns[FocusedColumn]).PropertyName;
end;

function TSOGrid.GetData: ISuperObject;
begin
  if Assigned(FDatasource) then
    Result := FDatasource.Data
  else
    Result := FData;
end;

procedure TSOGrid.DoUndoLastUpdate(Sender: TObject);
begin

end;

procedure TSOGrid.DoRevertRecord(Sender: TObject);
begin

end;

Function GetTempFileName(Const ext : String) : String;

Var
  I : Integer;
  Start : String;

begin
  Start:=GetTempDir;
  I:=0;
  Repeat
    Result:=Format('%s%.5d%s',[Start,I,ext]);
    Inc(I);
  Until not FileExists(Result);
end;

function TSOGrid.ContentAsCSV(Source: TVSTTextSourceType; const Separator: String):Utf8String;
var
  values,prop,Row,Rows,value:ISuperObject;
  i:Integer;
begin
  if Source in [tstAll,tstInitialized,tstVisible] then
    Rows := Data
  else
    Rows := SelectedRows;

  result :='';

  values := TSuperObject.Create(stArray);

  for i:=0 to Header.Columns.Count-1 do
  begin
    if coVisible in Header.Columns[i].Options then
      values.AsArray.Add('"'+UTF8Decode(TSOGridColumn(Header.Columns[i]).Text)+'"');
  end;
  Result := Result+Join(Separator,values)+LineEnding;
  for row in rows do
  begin
    values := TSuperObject.Create(stArray);
    for i:=0 to Header.Columns.Count-1 do
    begin
      if coVisible in Header.Columns[i].Options then
      begin
        value := Row[TSOGridColumn(Header.Columns[i]).PropertyName];
        if value<>Nil then
          values.AsArray.Add(value.AsString)
        else
          values.AsArray.Add('""');
      end;
    end;
    Result := Result+Join(Separator,values)+LineEnding;
  end;
end;

procedure TSOGrid.DoExportExcel(Sender: TObject);
var
  tempfn:Utf8String;
  txt:Utf8String;
  txtbuf:PChar;
  l:LongInt;
  st:File;
begin
  tempfn:=GetTempFileName('.csv');
  AssignFile(st,tempfn);
  Rewrite(st,1);
  try
    if (toMultiSelect in TreeOptions.SelectionOptions) then
      txt := ContentAsCSV(tstSelected,#9)+#0
    else
      txt := ContentAsCSV(tstAll,#9)+#0;
    txtbuf := pchar(txt);
    l := strlen(txtbuf);
    BlockWrite(st,txtbuf^,l);
  finally
    CloseFile(st);
    OpenDocument(tempfn);
  end;
end;


procedure TSOGrid.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
  const AText: string);
var
  ItemData: PSOItemData;
  RowData, OldCelldata, NewCellData: ISuperObject;
  PropertyName:String;
begin
  RowData := nil;
  OldCellData := nil;
  NewCellData := nil;

  if Node <> nil then
  begin
    ItemData := GetItemData(Node);
    if ItemData <> nil then
    begin
      RowData := ItemData^.JSONData;
      if RowData <> nil then
      begin
        if Column >= 0 then
        begin
          PropertyName:=TSOGridColumn(Header.Columns.Items[Column]).PropertyName;
          OldCelldata := RowData[PropertyName];
          NewCellData := SO(UTF8Decode(AText));
          RowData[PropertyName] := NewCellData;
        end
        else
        begin
          PropertyName:=DefaultText;
          OldCelldata := RowData[DefaultText];
          NewCellData := SO(UTF8Decode(AText));
          RowData[DefaultText] := NewCellData;
        end;
      end;
    end;
  end;
  inherited DoNewText(Node, Column, AText);
  if Assigned(FDatasource) then
    FDatasource.NotifyChange(deFieldChange,RowData,PropertyName,OldCelldata,NewCellData);
end;

// hack to allow right click menu on header popup menu  and different popup menu on rows
//   set message.msg to 0 if handled to stop message processing.
type
  TVTHeaderHack = class(TVTHeader);

//Bugfix :
procedure TSOGrid.WndProc(var Message: TLMessage);
var
  Handled: Boolean;

begin
  Handled := False;

  // Try the header whether it needs to take this message.
  if Assigned(Header) and (Header.States <> []) then
    Handled := TVTHeaderHack(Header).HandleMessage(Message);
  if not Handled then
  begin
    // For auto drag mode, let tree handle itself, instead of TControl.
    if not (csDesigning in ComponentState) and
       ((Message.Msg = LM_LBUTTONDOWN) or (Message.Msg = LM_LBUTTONDBLCLK)) then
    begin
      //lclheader
      //when FHeader.FStates = [] it comes until here unlike Delphi (uses NC messages)
      //skip this code when is clicked inside the header
      if (DragMode = dmAutomatic) and (DragKind = dkDrag) and
        not Header.InHeader(SmallPointToPoint(TLMMouse(Message).Pos)) then
      begin
        if IsControlMouseMsg(TLMMouse(Message)) then
          Handled := True;
        if not Handled then
        begin
          ControlState := ControlState + [csLButtonDown];
          Dispatch(Message);  // overrides TControl's BeginDrag
          Handled := True;
        end;
      end;
    end;

    if not Handled and Assigned(Header) then
      Handled := TVTHeaderHack(Header).HandleMessage(Message);

    if not Handled then
    begin
      //lcl: probably not necessary
      //if (Message.Msg in [WM_NCLBUTTONDOWN, WM_NCRBUTTONDOWN, WM_NCMBUTTONDOWN]) and not Focused and CanFocus then
      //  SetFocus;
      inherited;
    end
    //// BUGFIX Tranquil IT Systems.
    else
       Message.Msg := 0;
    //// end BUGFIX
  end;
end;


//----------------- TSOEdit --------------------------------------------------------------------------------------------
// Implementation of a generic node cell editor.

constructor TSOEdit.Create(Link: TSOStringEditLink);

begin
  inherited Create(nil);
  ShowHint := False;
  ParentShowHint := False;
  // This assignment increases the reference count for the interface.
  FRefLink := Link;
  // This reference is used to access the link.
  FLink := Link;
end;


procedure TSOEdit.CMAutoAdjust(var Message: TLMessage);

begin
  AutoAdjustSize;
end;


procedure TSOEdit.CMExit(var Message: TLMessage);

begin
  if Assigned(FLink) and not FLink.FStopping then
    with FLink, FTree do
    begin
      if (toAutoAcceptEditChange in TreeOptions.StringOptions) then
        DoEndEdit
      else
        DoCancelEdit;
    end;
end;


procedure TSOEdit.CMRelease(var Message: TLMessage);

begin
  Free;
end;


procedure TSOEdit.CNCommand(var Message: TLMCommand);

begin
  {if Assigned(FLink) and Assigned(FLink.FTree) and (Message.NotifyCode = EN_UPDATE) and
    not (toGridExtensions in FLink.FTree.FOptions.FMiscOptions) and
    not (vsMultiline in FLink.FNode.States) then
    // Instead directly calling AutoAdjustSize it is necessary on Win9x/Me to decouple this notification message
    // and eventual resizing. Hence we use a message to accomplish that.
    if IsWinNT then
      AutoAdjustSize
    else
      PostMessage(Handle, CM_AUTOADJUST, 0, 0);}
end;


procedure TSOEdit.WMChar(var Message: TLMChar);

begin
  if not (Message.CharCode in [VK_ESCAPE, VK_TAB]) then
    inherited;
end;


procedure TSOEdit.WMDestroy(var Message: TLMDestroy);

begin
  // If editing stopped by other means than accept or cancel then we have to do default processing for
  // pending changes.
  if Assigned(FLink) and not FLink.FStopping then
  begin
    with FLink, FTree do
    begin
      if (toAutoAcceptEditChange in TreeOptions.StringOptions) and Modified then
        Text[FNode, FColumn] := FEdit.Text;
    end;
    FLink := nil;
    FRefLink := nil;
  end;

  inherited;
end;


procedure TSOEdit.WMGetDlgCode(var Message: TLMNoParams);

begin
  inherited;

  Message.Result := Message.Result or DLGC_WANTALLKEYS or DLGC_WANTTAB or DLGC_WANTARROWS;
end;


procedure TSOEdit.WMKeyDown(var Message: TLMKeyDown);

// Handles some control keys.

var
  Shift: TShiftState;
  EndEdit: Boolean;
  Tree: TSOGrid;

begin
  case Message.CharCode of
    VK_ESCAPE:
      begin
        Tree := FLink.FTree;
        FLink.FTree.DoCancelEdit;
        Tree.SetFocus;
      end;
    VK_RETURN:
      begin
        EndEdit := not (vsMultiline in FLink.FNode^.States);
        if not EndEdit then
        begin
          // If a multiline node is being edited the finish editing only if Ctrl+Enter was pressed,
          // otherwise allow to insert line breaks into the text.
          Shift := KeyDataToShiftState(Message.KeyData);
          EndEdit := ssCtrl in Shift;
        end;
        if EndEdit then
        begin
          Tree := FLink.FTree;
          FLink.FTree.InvalidateNode(FLink.FNode);
          FLink.FTree.DoEndEdit;
          Tree.SetFocus;
        end;
      end;
    VK_UP,VK_DOWN:
      begin
        if not (vsMultiline in FLink.FNode^.States) then
        begin
            Tree := (FLink as TSOStringEditLink).FTree;
            Tree.InvalidateNode((FLink as TSOStringEditLink).FNode);
            Tree.DoEndEdit;
            Tree.SetFocus;
            SendMessage(Tree.Handle,Message.Msg,Message.CharCode,Message.KeyData);
        end
        else
          inherited;
      end;
    VK_TAB:
      begin
          Tree := (FLink as TSOStringEditLink).FTree;
          Tree.InvalidateNode((FLink as TSOStringEditLink).FNode);
          Tree.DoEndEdit;
          Tree.SetFocus;
          SendMessage(Tree.Handle,Message.Msg,Message.CharCode,Message.KeyData);
      end;
  else
    inherited;
  end;
end;


procedure TSOEdit.AutoAdjustSize;

// Changes the size of the edit to accomodate as much as possible of its text within its container window.
// NewChar describes the next character which will be added to the edit's text.

var
  DC: HDC;
  Size: TSize;
  LastFont: THandle;

begin
  if not (vsMultiline in FLink.FNode^.States) then
  begin
    DC := GetDC(Handle);
    LastFont := SelectObject(DC, Font.Reference.Handle);
    try
      // Read needed space for the current text.
      GetTextExtentPoint32(DC, PChar(Text), Length(Text), Size);
      Inc(Size.cx, 2 * FLink.FTree.TextMargin);

      // Repaint associated node if the edit becomes smaller.
      if Size.cx < Width then
        FLink.FTree.InvalidateNode(FLink.FNode);

      if FLink.FAlignment = taRightJustify then
        FLink.SetBounds(Rect(Left + Width - Size.cx, Top, Left + Width, Top + Height))
      else
        FLink.SetBounds(Rect(Left, Top, Left + Size.cx, Top + Height));
    finally
      SelectObject(DC, LastFont);
      ReleaseDC(Handle, DC);
    end;
  end;
end;


procedure TSOEdit.CreateParams(var Params: TCreateParams);

begin
  inherited;

  // Only with multiline style we can use the text formatting rectangle.
  // This does not harm formatting as single line control, if we don't use word wrapping.
  with Params do
  begin
    //todo: delphi uses Multiline for all
    //Style := Style or ES_MULTILINE;
    if vsMultiline in FLink.FNode^.States then
    begin
      Style := Style and not (ES_AUTOHSCROLL or WS_HSCROLL) or WS_VSCROLL or ES_AUTOVSCROLL;
      Style := Style or ES_MULTILINE;
    end;
    {if tsUseThemes in FLink.FTree.States then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end
    else}
    begin
      Style := Style or WS_BORDER;
      ExStyle := ExStyle and not WS_EX_CLIENTEDGE;
    end;
  end;
end;


procedure TSOEdit.Release;

begin
  if HandleAllocated then
    PostMessage(Handle, CM_RELEASE, 0, 0);
end;

//----------------- TSOStringEditLink ------------------------------------------------------------------------------------

constructor TSOStringEditLink.Create;

begin
  inherited;
  FEdit := TSOEdit.Create(Self);
  with FEdit do
  begin
    Visible := False;
    BorderStyle := bsSingle;
    AutoSize := False;
  end;
end;


destructor TSOStringEditLink.Destroy;

begin
  FEdit.Release;
  inherited;
end;


function TSOStringEditLink.BeginEdit: Boolean; stdcall;

// Notifies the edit link that editing can start now. descendants may cancel node edit
// by returning False.

begin
  Result := not FStopping;
  if Result then
  begin
    FEdit.Show;
    FEdit.SelectAll;
    FEdit.SetFocus;
  end;
end;


procedure TSOStringEditLink.SetEdit(const Value: TSOEdit);

begin
  if Assigned(FEdit) then
    FEdit.Free;
  FEdit := Value;
end;


function TSOStringEditLink.CancelEdit: Boolean; stdcall;

begin
  Result := not FStopping;
  if Result then
  begin
    FStopping := True;
    FEdit.Hide;
    FTree.CancelEditNode;
    FEdit.FLink := nil;
    FEdit.FRefLink := nil;
  end;
end;


function TSOStringEditLink.EndEdit: Boolean; stdcall;

begin
  Result := not FStopping;
  if Result then
  try
    FStopping := True;
    if FEdit.Modified then
      FTree.Text[FNode, FColumn] := FEdit.Text;
    FEdit.Hide;
    FEdit.FLink := nil;
    FEdit.FRefLink := nil;
  except
    FStopping := False;
    raise;
  end;
end;


function TSOStringEditLink.GetBounds: TRect; stdcall;

begin
  Result := FEdit.BoundsRect;
end;


function TSOStringEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;

// Retrieves the true text bounds from the owner tree.

var
  Text: String;

begin
  Result := Tree is TCustomVirtualStringTree;
  if Result then
  begin
    FTree := Tree as TSOGrid;
    FNode := Node;
    FColumn := Column;
    // Initial size, font and text of the node.
    FTree.GetTextInfo(Node, Column, FEdit.Font, FTextBounds, Text);
    FEdit.Font.Color := clWindowText;
    FEdit.Parent := Tree;
    FEdit.HandleNeeded;
    FEdit.Text := Text;

    if Column <= NoColumn then
    begin
      FEdit.BidiMode := FTree.BidiMode;
      FAlignment := FTree.Alignment;
    end
    else
    begin
      FEdit.BidiMode := FTree.Header.Columns[Column].BidiMode;
      FAlignment := FTree.Header.Columns[Column].Alignment;
    end;

    if FEdit.BidiMode <> bdLeftToRight then
      ChangeBidiModeAlignment(FAlignment);
  end;
end;


procedure TSOStringEditLink.ProcessMessage(var Message: TLMessage); stdcall;
begin
  Message.Result := SendMessage(FEdit.Handle,Message.msg,Message.wParam,Message.lParam);
  //FEdit.WindowProc(Message);
end;


procedure TSOStringEditLink.SetBounds(R: TRect); stdcall;

// Sets the outer bounds of the edit control and the actual edit area in the control.

var
  Offset: Integer;

begin
  if not FStopping then
  begin
    with R do
    begin
      // Set the edit's bounds but make sure there's a minimum width and the right border does not
      // extend beyond the parent's left/right border.
      if Left < 0 then
        Left := 0;
      if Right - Left < 30 then
      begin
        if FAlignment = taRightJustify then
          Left := Right - 30
        else
          Right := Left + 30;
      end;
      if Right > FTree.ClientWidth then
        Right := FTree.ClientWidth;
      FEdit.BoundsRect := R;

      // The selected text shall exclude the text margins and be centered vertically.
      // We have to take out the two pixel border of the edit control as well as a one pixel "edit border" the
      // control leaves around the (selected) text.
      R := FEdit.ClientRect;
      Offset := 2;
      {if tsUseThemes in FTree.FStates then
        Inc(Offset);}
      InflateRect(R, -FTree.TextMargin + Offset, Offset);
      if not (vsMultiline in FNode^.States) then
        OffsetRect(R, 0, FTextBounds.Top - FEdit.Top);

      SendMessage(FEdit.Handle, EM_SETRECTNP, 0, PtrUInt(@R));
    end;
  end;
end;

end.
