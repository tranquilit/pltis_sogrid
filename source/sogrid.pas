{**********************************************************************
 Package pltis_sogrid.pkg
 This unit is based on package virtualtreesextra work from CodeTyphon Studio  (http://www.pilotlogic.com/)
***********************************************************************}

unit sogrid;

{$mode objfpc}{$H+}


interface

uses
  {$IFDEF windows}
  Windows,
  {$ENDIF}
  Classes, Controls, ComCtrls, VirtualTrees, math, SysUtils,
  SuperObject, Menus, Graphics, Clipbrd, LCLType, Dialogs,LMessages,StdCtrls,
  Types, DefaultTranslator,
  mormot.core.base,
  mormot.core.variants,
  mormot.core.unicode,
  mormot.core.rtti,
  sogridcommon;

type

  TSOGrid = class;

  TSONodesEvent = procedure(Sender: TSOGrid; Rows: ISuperObject) of object;

  TSOCompareNodesEvent = procedure(Sender: TSOGrid; Node1, Node2: ISuperObject; const Columns: Array of String;
    var Result: Integer) of object;

  TSOCanPasteEvent = function(Sender: TSOGrid;Row:ISuperObject):boolean of object;

  /// filter options for header popup menu
  TTisGridFilterOptions = class(TPersistent)
  private
    fGrid: TSOGrid;
    fFilters: TDocVariantData;
    fCaseInsensitive: Boolean;
    fClearAterLoadingData: Boolean;
    fDisplayedCount: Integer;
    fEnabled: Boolean;
  protected const
    DefaultDisplayedCount = 10;
    DefaultEnabled = False;
    DefaultCaseInsensitive = False;
    DefaultClearAterLoadingData = False;
    MARK_ARROW = ' ↓';
  protected
    /// clear MARK_ARROW mark of all header columns
    procedure ClearHeaderArrows;
  public
    constructor Create(aGrid: TSOGrid); reintroduce;
    procedure AssignTo(aDest: TPersistent); override;
    /// check if a filter exists
    function FilterExists(const aPropertyName: RawUtf8; const aValue: string): Boolean;
    /// apply all filters in all columns
    procedure ApplyFilters;
    /// clear all filters
    procedure ClearFilters;
    property Filters: TDocVariantData read fFilters;
  published
    property CaseInsensitive: Boolean read fCaseInsensitive write fCaseInsensitive default DefaultCaseInsensitive;
    /// used after call Grid.LoadData
    // - if it is TRUE, it will call ClearFilters, otherwise it will call ApplyFilters
    property ClearAterLoadingData: Boolean read fClearAterLoadingData write fClearAterLoadingData default DefaultClearAterLoadingData;
    /// how many menu items will be used to show filters
    property DisplayedCount: Integer read fDisplayedCount write fDisplayedCount default DefaultDisplayedCount;
    /// if FALSE, none filter menu item will be created
    property Enabled: Boolean read fEnabled write fEnabled default DefaultEnabled;
  end;

  { TSOGridColumn }
  TSOGridColumn = class(TVirtualTreeColumn)
  private
    fAllowFilter: Boolean;
    FPropertyName: string;
    procedure SetPropertyName(const Value: string);
    function GetTitle: TCaption;
    procedure SetTitle(AValue: TCaption);
  protected const
    DefaultAllowFilter = True;
  public
    constructor Create(aCollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    /// allow use filter for this column, if Grid.FilterOptions.Enable is TRUE
    property AllowFilter: Boolean read fAllowFilter write fAllowFilter default DefaultAllowFilter;
    property Text: TCaption read GetTitle write SetTitle;
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

  { TSOHeaderPopupMenu }

  TSOHeaderPopupMenu = class(TPopupMenu)
  private
    FOptions: TSOHeaderPopupOptions;
    FOnAddHeaderPopupItem: TAddHeaderPopupItemEvent;
    FOnColumnChange: TColumnChangeEvent;
  protected
    procedure DoAddHeaderPopupItem(const Column: TColumnIndex; out Cmd: TAddPopupItemType); virtual;
    procedure DoColumnChange(Column: TColumnIndex; Visible: Boolean); virtual;
    procedure OnMenuItemClick(Sender: TObject);
    procedure OnMenuShowAllClick(Sender: TObject);
    procedure OnMenuHideAllClick(Sender: TObject);
    procedure OnMenuRestoreClick(Sender: TObject);
    procedure OnMenuFilterClick(aSender: TObject);
    procedure OnMenuFilterClearClick(aSender: TObject);
  public
    procedure FillPopupMenu;
  published
    property Options: TSOHeaderPopupOptions read FOptions write FOptions default [];
    property OnAddHeaderPopupItem: TAddHeaderPopupItemEvent read FOnAddHeaderPopupItem write FOnAddHeaderPopupItem;
    property OnColumnChange: TColumnChangeEvent read FOnColumnChange write FOnColumnChange;
  end;

  { TSOStringEditLink }

  // Edit support classes.
  TSOStringEditLink = class;

  /// this class implements the base for an in-place edit control
  // - use it if you want to implement your own controls

  { TTisGridControl }

  TTisGridControl = class(TObject)
  private
  protected
    fInternal: TWinControl;
    fIsReadOnly: Boolean;
    procedure SetIsReadOnly(AValue: Boolean); Virtual;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    /// access to the internal (generic) WinControl instance
    function Internal: TWinControl;
    /// set OnKeyDown event allowing grid to be in control
    procedure SetOnKeyDown(aEvent: TKeyEvent); virtual;
    /// set OnExit event allowing grid to be in control
    procedure SetOnExit(aEvent: TNotifyEvent); virtual;
    /// it returns the value edited by user
    function GetValue: Variant; virtual;
    /// it set the value from grid to the control
    procedure SetValue(const aValue: Variant); virtual;

    property IsReadOnly: Boolean read fIsReadOnly write SetIsReadOnly;
  end;

  /// control used for all String data type

  { TTisGridEditControl }

  TTisGridEditControl = class(TTisGridControl)
  protected
    procedure SetIsReadOnly(AValue: Boolean); override;
  public
    constructor Create; override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
    function Edit: TEdit;
  end;

  TSOStringEditLink = class(TInterfacedObject, IVTEditLink)
  private
    fControl: TTisGridControl;
    fGrid: TSOGrid;
    fNode: PVirtualNode;
    fColumn: Integer;
    FTextBounds: TRect;              // Smallest rectangle around the text.
  protected
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure EditExit(Sender: TObject); virtual;
    function NewControl: TTisGridControl; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    function PrepareEdit(aTree: TBaseVirtualTree; aNode: PVirtualNode; aColumn: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var aMessage: TLMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
  end;

  TTisGridExportFormatOption = (
    /// inherited types
    efoRtf,  // by ContentToRTF
    efoHtml, // by ContentToHTML
    efoText, // by ContentToText
    /// our custom types
    efoCsv,  // by ContentToCsv
    efoJson // by ContentToJson
  );

  TTisGridExportFormatOptions = set of TTisGridExportFormatOption;

  /// adapter for TTisGridExportFormatOption
  TTisGridExportFormatOptionAdapter = object
    /// convert enum to caption
    function EnumToCaption(const aValue: TTisGridExportFormatOption): string;
    /// convert caption to enum
    // - if aValue not found, it will return the first element
    function CaptionToEnum(const aValue: string): TTisGridExportFormatOption;
    /// convert all enums to strings
    // - you can customize elements using aCustom
    procedure EnumsToStrings(aDest: TStrings; const aCustom: TTisGridExportFormatOptions = [
      low(TTisGridExportFormatOption)..high(TTisGridExportFormatOption)]);
    /// convert file extension to enum
    // - if aValue not found, it will return the first element
    function ExtensionToEnum(const aValue: TFileName): TTisGridExportFormatOption;
    /// convert enum to save dialog filter
    function EnumToFilter(const aValue: TTisGridExportFormatOption): string;
  end;

  TTisGridTextSourceTypes = set of TVSTTextSourceType;

  /// adapter for TVSTTextSourceType
  TTisGridTextSourceTypeAdapter = object
    /// convert enum to caption
    function EnumToCaption(const aValue: TVSTTextSourceType): string;
    /// convert caption to enum
    // - if aValue not found, it will return the first element
    function CaptionToEnum(const aValue: string): TVSTTextSourceType;
    /// convert all enums to strings
    // - you can customize elements using aCustom
    procedure EnumsToStrings(aDest: TStrings; const aCustom: TTisGridTextSourceTypes = [tstAll, tstSelected]);
  end;

  TSOGridGetText = procedure(Sender: TBaseVirtualTree; Node: PVirtualNode;
    RowData, CellData: ISuperObject; Column: TColumnIndex; TextType: TVSTTextType;
    var CellText: string) of object;

  /// event that allow to validate the new value from user input
  // - aCurValue is the current value for the aColumn
  // - use it for check/change the aNewValue argument, before assign it, and/or abort the process
  TOnGridEditValidated = procedure(aSender: TSOGrid; aColumn: TSOGridColumn;
    const aCurValue: Variant; var aNewValue: Variant; var aAbort: Boolean) of object;

  /// event that allows change aNode.States after it was changed
  // - use it to force showing (or not) some node
  TOnGridNodeFiltering = procedure(aSender: TSOGrid; aNode: PVirtualNode) of object;

  { TSOGrid }
  TSOGrid = class(TCustomVirtualStringTree,ISODataView)
  private
    FKeyFieldsList: Array of String;
    FOnBeforePaste: TSOCanPasteEvent;
    FOnCutToClipBoard: TNotifyEvent;
    FOnNodesDelete: TSONodesEvent;
    FOnSOCompareNodes: TSOCompareNodesEvent;
    FParentProperty: String;
    FSelectedAndTotalLabel: TLabel;
    FShowAdvancedColumnsCustomize: Boolean;
    FAllowDataExport: Boolean;
    FTextFound: boolean;
    FindDlg: TFindDialog;
    FZebraPaint: Boolean;
    fZebraLightness: Byte;
    ReplaceDialog: TReplaceDialog;

    FColumnToFind: integer;
    FStartSearchNode: PVirtualNode;
    FTextToFind: string;

    FData: ISuperObject;
    FItemDataOffset: integer;
    FOnGetText: TSOGridGetText;

    FPendingAppendObject:ISuperObject;

    FDefaultPopupMenu: TPopupMenu;
    fExportFormatOptions: TTisGridExportFormatOptions;
    fFilterOptions: TTisGridFilterOptions;
    fDefaultSettings: ISuperObject; // all default settings after load component
    fOnEditValidated: TOnGridEditValidated;
    fOnNodeFiltering: TOnGridNodeFiltering;
    function FocusedPropertyName: String;
    function GetData: ISuperObject;
    function GetFocusedColumnObject: TSOGridColumn;
    function GetFocusedRow: ISuperObject;
    function GetGridSettings: String;
    function GetKeyFieldsNames: String;
    function GetSettings: ISuperObject;

    procedure SetColumnToFind(AValue: integer);
    procedure SetData(const Value: ISuperObject);
    procedure SetFocusedColumnObject(AValue: TSOGridColumn);
    procedure SetFocusedRow(AValue: ISuperObject);
    procedure SetGridSettings(AValue: String);
    procedure SetKeyFieldsNames(AValue: String);
    procedure SetOnCutToClipBoard(AValue: TNotifyEvent);
    procedure SetOptions(const Value: TStringTreeOptions);
    function GetOptions: TStringTreeOptions;
    procedure SetParentProperty(AValue: String);
    procedure SetSelectedRows(AValue: ISuperObject);
    procedure SetSelectedAndTotalLabel(AValue: TLabel);
    procedure SetSettings(AValue: ISuperObject);
    procedure SetShowAdvancedColumnsCustomize(AValue: Boolean);
    procedure SetAllowDataExport(AValue: Boolean);

    procedure WMKeyDown(var Message: TLMKeyDown); message LM_KEYDOWN;


  protected
    const DefaultExportFormatOptions = [efoCsv, efoJson];
    procedure Loaded; override;

    property RootNodeCount stored False;
    property NodeDataSize;

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

    /// fill the popup menu with items in runtime when grid gets focus, for deal with the data and grid itself
    // - each item will use the POPUP_ITEM_TAG public constant as its Tag value
    // - it is called in DoEnter
    procedure FillPopupMenu; virtual;
    /// clean the popup when lost focus, removing items inserted before
    // - each item that has the POPUP_ITEM_TAG public constant as its Tag value, will be removed
    // - it is called in DoExit
    procedure CleanPopupMenu; virtual;

    procedure PrepareCell(var PaintInfo: TVTPaintInfo;
      WindowOrgX, MaxWidth: integer); override;

    //gestion affichage multiselection
    procedure DoBeforeCellPaint(aCanvas: TCanvas; aNode: PVirtualNode;
      aColumn: TColumnIndex; aCellPaintMode: TVTCellPaintMode;
  aCellRect: TRect; var aContentRect: TRect); override;
    procedure DoTextDrawing(var aPaintInfo: TVTPaintInfo; const aText: string;
      aCellRect: TRect; aDrawFormat: cardinal); override;

    procedure DoBeforeItemErase(aCanvas: TCanvas; aNode: PVirtualNode;
      const aItemRect: TRect; var aColor: TColor; var aEraseAction: TItemEraseAction); override;

    function FindText(Txt: string): PVirtualNode;
    procedure FindDlgFind(Sender: TObject);

    procedure DoFindText(Sender: TObject);
    procedure DoFindNext(Sender: TObject);
    procedure DoFindReplace(Sender: TObject);

    procedure DoUndoLastUpdate(Sender: TObject); virtual;
    procedure DoRevertRecord(Sender: TObject); virtual;
    procedure DoExport(Sender: TObject); virtual;
    procedure DoCopyToClipBoard(Sender: TObject); virtual;
    procedure DoCopyCellToClipBoard(Sender: TObject); virtual;
    procedure DoCopySpecialToClipboard({%H-}aSender: TObject); virtual;
    procedure DoCutToClipBoard(Sender: TObject); virtual;
    procedure DoDeleteRows(Sender: TObject); virtual;
    procedure DoPaste(Sender: TObject); virtual;
    procedure DoSelectAllRows(Sender: TObject); virtual;
    procedure DoPrint(Sender: TObject); virtual;
    procedure DoCustomizeColumns(Sender: TObject); virtual;
    procedure DoAdvancedCustomizeColumns(Sender: TObject); virtual;

    procedure DoExpandAll(Sender: TObject); virtual;
    procedure DoCollapseAll(Sender: TObject); virtual;


    property ColumnToFind: integer read FColumnToFind write SetColumnToFind;
    property TextToFind: string read FTextToFind write FTextToFind;
    property TextFound: boolean read FTextFound write FTextFound;

    function DoKeyAction(var CharCode: Word; var Shift: TShiftState): Boolean; override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GetSelectedRows: ISuperObject;


    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
      const AXProportion, AYProportion: Double); override;

    procedure DoChange(Node: PVirtualNode); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    /// called before open a context menu
    // - it will call Clean/FillPopupMenu, as some Captions translation should be done before show up
    procedure DoContextPopup(aMousePos: TPoint; var aHandled: Boolean); override;
    procedure DoHeaderMouseDown(aButton: TMouseButton; aShift: TShiftState; aX, aY: Integer); override;
    procedure DoEditValidated(const aColumn: TSOGridColumn; const aCurValue: Variant;
      var aNewValue: Variant; var aAbort: Boolean); virtual;
    procedure DoNodeFiltering(aNode: PVirtualNode); virtual;
  public
    const POPUP_ITEM_TAG = 250;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure FixDesignFontsPPI(const ADesignTimePPI: Integer); override;
    procedure ScaleFontsPPI(const AToPPI: Integer; const AProportion: Double); override;

    function GetNodeSOData(Node: PVirtualNode): ISuperObject;
    procedure LoadData;
    property Data: ISuperObject read GetData write SetData;
    property ParentProperty: String read FParentProperty write SetParentProperty;
    property SelectedRows: ISuperObject read GetSelectedRows write SetSelectedRows;
    property FocusedRow:ISuperObject read GetFocusedRow write SetFocusedRow;
    function CheckedRows: ISuperObject;

    procedure SetFocusedRowNoClearSelection(AValue: ISuperObject);

    function GetCellData(N: PVirtualNode; FieldName: string;
      Default: ISuperObject = nil): ISuperObject;
    function GetCellStrValue(N: PVirtualNode; FieldName: string;
      Default: string = ''): string;


    // returns list of nodes matching exactly this record pointer
    function NodesForData(sodata: ISuperObject): TNodeArray;
    // returns list of nodes matching the key fields (from grid's KeyFieldsNames property) of sodata
    function NodesForKey(sodata: ISuperObject): TNodeArray;
    function NodesForKeyStr(Keyname,Keyvalue: String): TNodeArray;

    // Append a list of rows to the Grid
    procedure AddRows(SOArray: ISuperObject;AllowDuplicates:Boolean=False);
    // Append rows, calling OnBeforePaste for each (to filter row or remove some properties...)
    procedure PasteRows(Rows: ISuperObject);

    // Delete a list of rows from the Grid
    procedure DeleteRows(SOArray: ISuperObject);

    // ask to delete the semected rows
    procedure DeleteSelectedRows;

    // Handle the default sort behavious
    procedure DoHeaderClickSort(HitInfo: TVTHeaderHitInfo);

    function DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex
      ): IVTEditLink; override;

    // redraw the rows matching this record
    procedure InvalidateFordata(sodata: ISuperObject);
    procedure Clear; override;

    // sort columns headers collection based on manual positioning
    procedure ReorderColumns;

    procedure Customize;

    // SODatasource events handling
    procedure NotifyChange(EventType:TSODataEvent;Row:ISuperObject;OldValues,NewValues:ISuperObject);
    property FocusedColumnObject:TSOGridColumn read GetFocusedColumnObject write SetFocusedColumnObject;

    // sauvegarde et restauration des customisations utilisateurs
    property Settings: ISuperObject read GetSettings write SetSettings;
    procedure SaveSettingsToIni(inifilename: string);
    procedure LoadSettingsFromIni(inifilename: string);

    function FindColumnByPropertyName(propertyname: string): TSOGridColumn;
    function FindColumnByIndex(const aIndex: TColumnIndex): TSOGridColumn;

    //Ajouter les colonnes en s'inspirant du contenu Data
    procedure CreateColumnsFromData(FitWidth,AppendMissingAsHidden: Boolean);

    /// export Data to CSV format
    function ContentToCsv(aSource: TVSTTextSourceType; const aSeparator: string = ',';
      aColumnsVisibleOnly: Boolean = True; aColumnsTranslated: Boolean = True): RawUtf8;
    /// export Data to JSON format
    function ContentToJson(aSource: TVSTTextSourceType; aColumnsVisibleOnly: Boolean = True): RawUtf8;
    /// export data
    // - it will export using the format that matchs to aFileName extension
    // - if extension do not exist in ExportFormatOptions, it will use OnExportCustomContent event to get the content
    // - use aSelection as tstAll to export all nodes - default
    // - use aSelection as tstSelected to export only selected nodes
    procedure ExportData(const aFileName: TFileName; const aSelection: TVSTTextSourceType = tstAll);
    // Creates a temporary CSV file and open it in the default app
    procedure ExportExcel(Prefix:String='';Selection: TVSTTextSourceType=tstAll; Separator:Char=',');

    // Force refresh the "Selected / Total : %d/%d" label
    procedure UpdateSelectedAndTotalLabel;

    /// it returns the filter for the Save Dialog, when user wants to export data
    // - it will add file filters based on ExportFormatOptions property values
    // - you can override this method to customize default filters
    function GetExportDialogFilter: string; virtual;
    /// it restore original settings from original design
    procedure RestoreSettings;
  published
    property OnGetText: TSOGridGetText read FOnGetText write FOnGetText;

    property OnCutToClipBoard: TNotifyEvent read FOnCutToClipBoard write SetOnCutToClipBoard;
    property OnBeforePaste: TSOCanPasteEvent read FOnBeforePaste write FOnBeforePaste;
    property OnNodesDelete: TSONodesEvent read FOnNodesDelete write FOnNodesDelete;

    property ShowAdvancedColumnsCustomize: Boolean read FShowAdvancedColumnsCustomize write SetShowAdvancedColumnsCustomize;
    property AllowDataExport: Boolean read FAllowDataExport write SetAllowDataExport;
    property KeyFieldsList: TStringDynArray read FKeyFieldsList;
    property KeyFieldsNames: String read GetKeyFieldsNames write SetKeyFieldsNames;

    property OnSOCompareNodes: TSOCompareNodesEvent read FOnSOCompareNodes write FOnSOCompareNodes;

    property GridSettings: String read GetGridSettings write SetGridSettings stored False;
    property ExportFormatOptions: TTisGridExportFormatOptions
      read fExportFormatOptions write fExportFormatOptions default DefaultExportFormatOptions;
    property FilterOptions: TTisGridFilterOptions read fFilterOptions write fFilterOptions;
    property OnEditValidated: TOnGridEditValidated
      read fOnEditValidated write fOnEditValidated;
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
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBarOptions;
    property SelectedAndTotalLabel: TLabel read FSelectedAndTotalLabel write SetSelectedAndTotalLabel;
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

    property ZebraPaint:Boolean read FZebraPaint write FZebraPaint stored True default False ;
  end;

  TWidgetHelper = class helper for TWinControl
  public
    procedure SetFocusSafe;
  end;

  resourcestring
    GSConst_NoRecordFind = 'No more record found for "%s"';
    GSConst_PrintOn = 'Printed on';
    GSConst_Page = 'Page';
    GSConst_Confirmation = 'Confirm';
    GSConst_UndoLastUpdate = 'Undo last change';
    GSConst_RevertRecord = 'Revert to initial record';
    GSConst_Find = 'Search...';
    GSConst_FindNext = 'Find next';
    GSConst_FindReplace = 'Find and replace...';
    GSConst_Copy = 'Copy';
    GSConst_CopyCell = 'Copy cell';
    GSConst_CopySpecial = 'Copy special...';
    GSConst_Cut = 'Cut';
    GSConst_Paste = 'Paste';
    GSConst_Insert = 'Insert';
    GSConst_Delete = 'Delete';
    GSConst_DeleteRows = 'Delete selected rows';
    GSConst_ConfDeleteRow = 'Confirm the deletion of the %d selected rows ?';
    GSConst_SelectAll = 'Select all rows';
    GSConst_ExportSelected = 'Export selected rows to file...';
    GSConst_ExportAll = 'Export all rows to file...';
    GSConst_Print = 'Print...';
    GSConst_ExpandAll = 'Expand all';
    GSConst_CollapseAll = 'Collapse all';
    GSConst_CustomizeColumns = 'Customize columns...';
    GSConst_AdvancedCustomizeColumns = 'Advanced customize of table...';
    GSConst_ShowHideColumns = 'Show/hide columns';
    GSConst_ShowAllColumns = 'Show all columns';
    GSConst_HideAllColumns = 'Hide all columns';
    GSConst_RestoreDefaultColumns = 'Restore default columns';
    GSConst_GridFilterClear = 'Clear filter';
    GSConst_GridFilterClearAll = 'Clear all filters';

procedure Translate(const aDirectory, aLang: string);

implementation

uses soutils, soclipbrd, base64, IniFiles, LCLIntf, messages, forms,
  LCLTranslator, GraphUtil, variants, tisstrings, sogrideditor, ucopyspecial;

type
  TSOItemData = record
    JSONData: ISuperObject;
    JSONChildren: ISuperObject;
  end;
  PSOItemData = ^TSOItemData;

procedure Translate(const aDirectory, aLang: string);
var
  vDir: TFileName;
begin
  vDir := IncludeTrailingPathDelimiter(aDirectory);
  TranslateUnitResourceStringsEx(
    aLang, vDir, 'sogrid.po', 'sogrid');
end;

{ TTisGridFilterOptions }

procedure TTisGridFilterOptions.ClearHeaderArrows;
var
  v1: Integer;
  vColumn: TVirtualTreeColumn;
begin
  for v1 := 0 to fGrid.Header.Columns.Count-1 do
  begin
    vColumn := fGrid.Header.Columns[v1];
    vColumn.Text := StringReplace(vColumn.Text, MARK_ARROW, '', [rfReplaceAll]);
  end;
end;

constructor TTisGridFilterOptions.Create(aGrid: TSOGrid);
begin
  inherited Create;
  fGrid := aGrid;
  fFilters.Clear;
  fFilters.InitArray([], JSON_FAST_FLOAT);
  fCaseInsensitive := DefaultCaseInsensitive;
  fDisplayedCount := DefaultDisplayedCount;
  fEnabled := DefaultEnabled;
end;

procedure TTisGridFilterOptions.AssignTo(aDest: TPersistent);
begin
  if aDest is TTisGridFilterOptions then
  begin
    with TTisGridFilterOptions(aDest) do
    begin
      fFilters.Clear;
      fFilters.InitCopy(Variant(self.fFilters), JSON_[mDefault]);
      CaseInsensitive := self.CaseInsensitive;
      DisplayedCount := self.DisplayedCount;
      Enabled := self.Enabled;
    end;
  end
  else
    inherited AssignTo(aDest);
end;

function TTisGridFilterOptions.FilterExists(const aPropertyName: RawUtf8;
  const aValue: string): Boolean;
var
  vObj: PDocVariantData;
  vTest: TDocVariantData;
begin
  result := False;
  vTest.Clear;
  vTest.InitFast(dvObject);
  vTest.S[aPropertyName] := aValue;
  for vObj in fFilters.Objects do
  begin
    if vObj^.Equals(vTest) then
    begin
      result := True;
      break;
    end;
  end;
end;

procedure TTisGridFilterOptions.ApplyFilters;
var
  vData: ISuperObject;
  vNode: PVirtualNode;
  vField: TDocVariantFields;
  v1, v2: Integer;
  vColumn: TSOGridColumn;
begin
  ClearHeaderArrows;
  vNode := fGrid.GetFirst(True);
  while vNode <> nil do
  begin
    vData := fGrid.GetNodeSOData(vNode);
    if Assigned(vData) then
    begin
      if fFilters.Count > 0 then
      begin
        for v1 := 0 to fGrid.Header.Columns.Count-1 do
        begin
          vColumn := fGrid.Header.Columns[v1] as TSOGridColumn;
          // turn it invisible by default
          Exclude(vNode^.States, vsVisible);
          for v2 := fFilters.Count-1 downto 0 do
          begin
            for vField in DocVariantData(fFilters.Value[v2])^.Fields do
              if (not fGrid.FilterOptions.CaseInsensitive and SameText(vData.S[vField.Name^], vField.Value^))
                or (fGrid.FilterOptions.CaseInsensitive and SameStr(vData.S[vField.Name^], vField.Value^)) then
              begin
                Include(vNode^.States, vsVisible);
                fGrid.DoNodeFiltering(vNode);
                // add an MARK_ARROW in header column text, if there are filters for this column
                if (vsVisible in vNode^.States)
                  and (vField.Name^ = vColumn.PropertyName)
                  and (Pos(MARK_ARROW, vColumn.Text) = 0) then
                    vColumn.Text := vColumn.Text + MARK_ARROW;
                break;
              end;
          end;
        end;
      end
      else
      begin
        // if there is no filters, turn it visible by default
        Include(vNode^.States, vsVisible);
        fGrid.DoNodeFiltering(vNode);
      end;
    end;
    vNode := fGrid.GetNext(vNode, True);
  end;
  fGrid.Invalidate;
  if (fGrid.FocusedNode = nil) or not (vsVisible in fGrid.FocusedNode^.States) then
  begin
    fGrid.ClearSelection;
    fGrid.FocusedNode := fGrid.GetFirstVisible;
    fGrid.Selected[fGrid.FocusedNode] := True;
  end;
  fGrid.ScrollIntoView(fGrid.FocusedNode, False);
end;

procedure TTisGridFilterOptions.ClearFilters;
begin
  fFilters.Clear;
  ApplyFilters;
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

function TSOGridColumn.GetTitle: TCaption;
begin
  Result := GetText();
end;

procedure TSOGridColumn.SetTitle(AValue: TCaption);
begin
  SetText(AValue);
end;

constructor TSOGridColumn.Create(aCollection: TCollection);
begin
  inherited Create(aCollection);
  Options := Options + [coWrapCaption];
  fAllowFilter := DefaultAllowFilter;
end;

{
constructor TSOGridColumn.Create(aCollection: TCollection);
begin
  inherited Create(Collection);
  Options:=Options+[coWrapCaption];
end;
}
procedure TSOGridColumn.Assign(Source: TPersistent);
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

procedure TSOHeaderPopupMenu.OnMenuShowAllClick(Sender: TObject);
var
  i: Integer;
begin
  if Assigned(PopupComponent) and (PopupComponent is TBaseVirtualTree) then
  begin
    with TVirtualTreeCast(PopupComponent).Header.Columns do
    begin
      for i := 0 to Count-1 do
      if not (coVisible in Items[i].Options) then
      begin
        Items[i].Options := Items[i].Options + [coVisible];
        DoColumnChange(i, True);
      end;
    end;
  end;
end;

procedure TSOHeaderPopupMenu.OnMenuHideAllClick(Sender: TObject);
var
  i: Integer;
begin
  if Assigned(PopupComponent) and (PopupComponent is TBaseVirtualTree) then
  begin
    with TVirtualTreeCast(PopupComponent).Header.Columns do
    begin
      for i := 0 to Count-1 do
      if coVisible in Items[i].Options then
      begin
        Items[i].Options := Items[i].Options - [coVisible];
        DoColumnChange(i, False);
      end;
    end;
  end;
end;

procedure TSOHeaderPopupMenu.OnMenuRestoreClick(Sender: TObject);
begin
  TSOGrid(PopupComponent).RestoreSettings;
end;

procedure TSOHeaderPopupMenu.OnMenuFilterClick(aSender: TObject);
var
  vGrid: TSOGrid;
  vItem: TMenuItem;
  vColumn: TSOGridColumn;
  vObj: Variant;
begin
  if Assigned(PopupComponent) and (PopupComponent is TBaseVirtualTree) then
  begin
    if PopupComponent is TSOGrid then
    begin
      vItem := aSender as TMenuItem;
      vItem.Checked := not vItem.Checked;
      vGrid := PopupComponent as TSOGrid;
      vColumn := vGrid.FindColumnByIndex(vItem.Tag);
      vObj := _ObjFast([vColumn.PropertyName, StringToUtf8(vItem.Caption)]);
      if vItem.Checked then
        vGrid.FilterOptions.Filters.AddItem(vObj)
      else
        vGrid.FilterOptions.Filters.DeleteByValue(vObj);
      vGrid.FilterOptions.ApplyFilters;
    end;
  end;
end;

procedure TSOHeaderPopupMenu.OnMenuFilterClearClick(aSender: TObject);
var
  vGrid: TSOGrid;
  vItem: TMenuItem;
  vColumn: TSOGridColumn;
  v1: Integer;
  vFieldName: PRawUtf8;
begin
  if Assigned(PopupComponent) and (PopupComponent is TBaseVirtualTree) then
  begin
    if PopupComponent is TSOGrid then
    begin
      vGrid := PopupComponent as TSOGrid;
      vItem := aSender as TMenuItem;
      vColumn := vGrid.FindColumnByIndex(vItem.Tag);
      if Assigned(vColumn) then
      begin
        // clear all filters for the same propertyname
        for v1 := vGrid.FilterOptions.Filters.Count-1 downto 0 do
        begin
          for vFieldName in DocVariantData(vGrid.FilterOptions.Filters.Value[v1])^.FieldNames do
            if vFieldName^ = vColumn.PropertyName then
            begin
              vGrid.FilterOptions.Filters.Delete(v1);
              break;
            end;
        end;
        vGrid.FilterOptions.ApplyFilters;
      end
      else
        // if not found vColumn, it should clear all filters in the grid
        vGrid.FilterOptions.ClearFilters;
    end;
  end;
end;

procedure TSOHeaderPopupMenu.FillPopupMenu;

  procedure AddFilterItems(aGrid: TSOGrid; aColIdx: TColumnIndex);
  var
    vNewMenuItem: TMenuItem;
    vCount: Integer;
    vNode: PVirtualNode;
    vData: ISuperObject;
    vItem: TMenuItem;
    vValue: string;
    vFound: Boolean;
    vColumn: TSOGridColumn;
  begin
    vCount := 0;
    vColumn := aGrid.FindColumnByIndex(aColIdx);
    vNode := aGrid.GetFirst(True);
    while vNode <> nil do
    begin
      vData := aGrid.GetNodeSOData(vNode);
      if Assigned(vData) then
      begin
        vValue := vData.S[vColumn.PropertyName];
        vFound := False;
        // search duplicated value
        for vItem in Items do
        begin
          if (not aGrid.FilterOptions.CaseInsensitive and SameText(vItem.Caption, vValue))
            or (aGrid.FilterOptions.CaseInsensitive and SameStr(vItem.Caption, vValue)) then
          begin
            vFound := True;
            break;
          end;
        end;
        // do not duplicate items
        if not vFound then
        begin
          vNewMenuItem := TMenuItem.Create(Self);
          vNewMenuItem.Tag := aColIdx; // it will be use on OnMenuFilterClick
          vNewMenuItem.Caption := vValue;
          vNewMenuItem.OnClick := @OnMenuFilterClick;
          vNewMenuItem.Checked := aGrid.FilterOptions.FilterExists(vColumn.PropertyName, vValue);
          Items.Add(vNewMenuItem);
          Inc(vCount);
          if vCount >= aGrid.FilterOptions.DisplayedCount then
            exit;
        end;
      end;
      vNode := aGrid.GetNext(vNode, True);
    end;
  end;

var
  I: Integer;
  ColPos: TColumnPosition;
  ColIdx: TColumnIndex;
  NewMenuItem, vShowHideMenuItem: TSOMenuItem;
  Cmd: TAddPopupItemType;
  VisibleCounter: Cardinal;
  VisibleItem: TSOMenuItem;
  vMousePos: TPoint;
  vGrid: TSOGrid;
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
    with TVirtualTreeCast(PopupComponent).Header do
    begin
      // enable/disable filter
      if PopupComponent is TSOGrid then
      begin
        vGrid := PopupComponent as TSOGrid;
        RecordZero(@vMousePos, TypeInfo(TPoint));
        GetCursorPos(vMousePos);
        ColIdx := Columns.ColumnFromPosition(vGrid.ScreenToClient(vMousePos));
        if (ColIdx > NoColumn)
          and vGrid.FilterOptions.Enabled
          and vGrid.FindColumnByIndex(ColIdx).AllowFilter then
        begin
          // add a item for delete filters for a column
          NewMenuItem := TSOMenuItem.Create(Self);
          NewMenuItem.Tag := ColIdx;
          NewMenuItem.Caption := GSConst_GridFilterClear;
          NewMenuItem.OnClick := @OnMenuFilterClearClick;
          Items.Add(NewMenuItem);
          NewMenuItem := TSOMenuItem.Create(Self);
          NewMenuItem.Caption := '-';
          Items.Add(NewMenuItem);
          AddFilterItems(vGrid, ColIdx);
          NewMenuItem := TSOMenuItem.Create(Self);
          NewMenuItem.Caption := '-';
          Items.Add(NewMenuItem);
          // add a item for delete all filters
          NewMenuItem := TSOMenuItem.Create(Self);
          NewMenuItem.Tag := NoColumn;
          NewMenuItem.Caption := GSConst_GridFilterClearAll;
          NewMenuItem.OnClick := @OnMenuFilterClearClick;
          Items.Add(NewMenuItem);
        end;
      end;
      // add subitem "show/hide columns"
      vShowHideMenuItem := TSOMenuItem.Create(Self);
      vShowHideMenuItem.Caption := GSConst_ShowHideColumns;
      Items.Add(vShowHideMenuItem);
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
        if ColIdx = NoColumn then
          break;
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
            NewMenuItem.OnClick := @OnMenuItemClick;
            if Cmd = apDisabled then
              NewMenuItem.Enabled := False
            else
              if coVisible in Options then
                VisibleItem := NewMenuItem;
            vShowHideMenuItem.Add(NewMenuItem);
          end;
        end;
      end;

      NewMenuItem := TSOMenuItem.Create(Self);
      NewMenuItem.Tag := -1;
      NewMenuItem.Caption := GSConst_ShowAllColumns;
      NewMenuItem.OnClick := @OnMenuShowAllClick;
      Items.Add(NewMenuItem);

      NewMenuItem := TSOMenuItem.Create(Self);
      NewMenuItem.Tag := -2;
      NewMenuItem.Caption := GSConst_HideAllColumns;
      NewMenuItem.OnClick := @OnMenuHideAllClick;
      Items.Add(NewMenuItem);

      // restore default columns

      NewMenuItem := TSOMenuItem.Create(Self);
      NewMenuItem.Tag := -3;
      NewMenuItem.Caption := GSConst_RestoreDefaultColumns;
      NewMenuItem.OnClick := @OnMenuRestoreClick;
      Items.Add(NewMenuItem);
      // Conditionally disable menu item of last enabled column.
      if (VisibleCounter = 1) and (VisibleItem <> nil) and not (poAllowHideAll in FOptions) then
        VisibleItem.Enabled := False;
    end;
  end;
end;

{ TTisGridControl }

procedure TTisGridControl.SetIsReadOnly(AValue: Boolean);
begin
  if fIsReadOnly = AValue then Exit;
  fIsReadOnly := AValue;
end;

constructor TTisGridControl.Create;
begin
  inherited Create;
end;

destructor TTisGridControl.Destroy;
begin
  Application.ReleaseComponent(fInternal);
  inherited Destroy;
end;

function TTisGridControl.Internal: TWinControl;
begin
  result := fInternal;
end;

procedure TTisGridControl.SetOnKeyDown(aEvent: TKeyEvent);
begin
  fInternal.OnKeyDown := aEvent;
end;

procedure TTisGridControl.SetOnExit(aEvent: TNotifyEvent);
begin
  fInternal.OnExit := aEvent;
end;

function TTisGridControl.GetValue: Variant;
begin
  if fInternal.Caption = '' then
    result := NULL
  else
    result := fInternal.Caption;
end;

procedure TTisGridControl.SetValue(const aValue: Variant);
begin
  fInternal.Caption := VarToStr(aValue);
end;

{ TTisGridEditControl }

procedure TTisGridEditControl.SetIsReadOnly(AValue: Boolean);
begin
  inherited SetIsReadOnly(AValue);
  Edit.ReadOnly := fIsReadOnly;
end;

constructor TTisGridEditControl.Create;
begin
  inherited Create;
  fInternal := TEdit.Create(nil);
  Edit.Clear;
end;

function TTisGridEditControl.GetValue: Variant;
begin
  result := Edit.Text;
end;

procedure TTisGridEditControl.SetValue(const aValue: Variant);
begin
  Edit.Text := VarToStr(aValue);
end;

function TTisGridEditControl.Edit: TEdit;
begin
  result := fInternal as TEdit;
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

procedure TSOGrid.SetParentProperty(AValue: String);
begin
  if FParentProperty=AValue then Exit;
  FParentProperty:=AValue;
  LoadData;
end;

// select all the nodes matching the AValue Array list of ISuperObject
// if
procedure TSOGrid.SetSelectedRows(AValue: ISuperObject);
var
  ANodes:TNodeArray;
  ANode,NewFocusedNode: PVirtualNode;
  OldFocused,ARec: ISuperObject;
begin
  If AValue = Nil then
  begin
    ClearSelection;
    FocusedNode:=Nil;
  end
  else
  begin
    OldFocused := FocusedRow;

    ClearSelection;
    ANode := Nil;

    NewFocusedNode:=Nil;

    BeginUpdate;
    try
      for ARec in AValue do
      begin
        if Length(KeyFieldsList)=1 then
          ANodes := NodesForKeyStr(KeyFieldsList[0],ARec.S[KeyFieldsList[0]])
        else if Length(KeyFieldsList)>0 then
          ANodes := NodesForKey(ARec)
        else
          ANodes := NodesForData(ARec);

        for ANode in ANodes do begin
          if ARec = OldFocused then
            NewFocusedNode := ANode;
          Selected[ANode] := True;
        end;
      end;

    finally
      EndUpdate;
    end;

    // Focused the last selected node.
    if NewFocusedNode <> Nil then
      FocusedNode := NewFocusedNode
    else if (ANode <> Nil) then
      FocusedNode := ANode;
  end;
end;

procedure TSOGrid.SetSelectedAndTotalLabel(AValue: TLabel);
begin
  FSelectedAndTotalLabel := AValue;
  UpdateSelectedAndTotalLabel;
end;

procedure TSOGrid.UpdateSelectedAndTotalLabel;
var
  nbrTotal, nbrSelected: integer;
begin
  if not Assigned(FSelectedAndTotalLabel) then
    Exit;

  if (self.Data <> nil) and Assigned(self.Data) and Assigned(self.Data.AsArray) then
    nbrTotal := self.Data.AsArray.Length
  else
    nbrTotal := 0;

  nbrSelected := self.SelectedCount;

  if nbrSelected > 0 then
    FSelectedAndTotalLabel.Caption := Format('Selected / Total : %d / %d', [nbrSelected, nbrTotal])
  else
    FSelectedAndTotalLabel.Caption := Format('Total : %d elements', [nbrTotal]);
end;

function TSOGrid.GetExportDialogFilter: string;
var
  efo: TTisGridExportFormatOptionAdapter;
  i: TTisGridExportFormatOption;
begin
  result := '';
  for i := high(TTisGridExportFormatOption) downto low(TTisGridExportFormatOption) do
  begin
    if i in fExportFormatOptions then
    begin
      if result <> '' then
        result += '|';
      result += efo.EnumToFilter(i);
    end;
  end;
end;

procedure TSOGrid.RestoreSettings;
begin
  Settings := fDefaultSettings;
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

function TSOGrid.FindColumnByIndex(const aIndex: TColumnIndex): TSOGridColumn;
begin
  if aIndex = NoColumn then
    result := nil
  else
    result := TSOGridColumn(Header.Columns[aIndex]);
end;

procedure TSOGrid.CreateColumnsFromData(FitWidth,AppendMissingAsHidden: Boolean);
var
  values,prop,Row,propname:ISuperObject;
  col : TSOGridColumn;
  i:Integer;
  NewColStartIdx:Integer;
  NewCol : Boolean;
begin
  NewColStartIdx := NoColumn;
  BeginUpdate;
  try
    values := TSuperObject.Create(stArray);
    NewCol := False;
    for row in data do
    begin
      for propname in row.AsObject.GetNames do
      begin
        col := FindColumnByPropertyName(UTF8Encode(propname.AsString));
        if col = Nil then
        begin
          begin
            NewCol := True;
            col :=Header.Columns.Add as TSOGridColumn;
            NewColStartIdx:=col.Index;
            col.Text:=UTF8Encode(propname.AsString);
            col.PropertyName:=UTF8Encode(propname.AsString);
            col.Width:= 100;
            if AppendMissingAsHidden then
              col.Options:=col.Options - [coVisible];
            prop := row[propname.AsString];
            if (prop <> Nil) and (prop.DataType in [stDouble,stCurrency,stInt]) then
              col.Alignment:=taRightJustify;
          end;
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
    if FitWidth and (NewColStartIdx<>NoColumn) then
        Header.AutoFitColumns(False,smaUseColumnOption,NewColStartIdx);
    if NewCol and (Header.Columns.Count = 1) then
      Header.Columns[0].Width := 100;
    EndUpdate;
  end;
end;

procedure TSOGrid.SetSettings(AValue: ISuperObject);
var
  gridcol: TSOGridColumn;
  prop, column, columns: ISuperObject;
  propname : String;

begin
  if (AValue <> nil) and (AValue.AsObject <> Nil)  then
  begin
    if AValue.AsObject.Find('columns', columns) then
    begin
      for column in Columns do
      begin
        propname := UTF8Encode(column.S['propertyname']);
        gridcol := FindColumnByPropertyName(propname);
        if gridcol = nil then
        begin
          gridcol := Header.Columns.Add as TSOGridColumn;
          gridcol.Text:=UTF8Encode(column.S['text']);
          gridcol.PropertyName:=propname;
          gridcol.Width:= 100;
        end;

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
      Header.SortDirection := VirtualTrees.TSortDirection(prop.AsInteger);

    if AValue.AsObject.Find('headerheight', prop) then
      Header.Height := prop.AsInteger;

    if AValue.AsObject.Find('defaultnodeheight', prop) then
      DefaultNodeHeight := prop.AsInteger;

  end;
end;

procedure TSOGrid.SetShowAdvancedColumnsCustomize(AValue: Boolean);
begin
  if FShowAdvancedColumnsCustomize=AValue then Exit;
  FShowAdvancedColumnsCustomize:=AValue;
end;

procedure TSOGrid.SetAllowDataExport(AValue: Boolean);
begin
  if FAllowDataExport = AValue then Exit;
  FAllowDataExport := AValue;
end;

{$IFNDEF windows}
procedure GetKeyboardState( ks : TKeyBoardState );
var
  i : integer;
begin
  for i := 0 to 255 do
      ks[i] := GetKeyState(i);
end;

function ToASCII( vk :  integer; san_code :  integer; const key_state : TKeyboardState; output_buffer : PChar; flags : integer ) : integer;
begin

    if( (vk >= VK_NUMPAD0) and (vk <= VK_NUMPAD9) ) then
    begin
         output_buffer^ := char(vk - 48) ;
         result := 1;
         exit;
    end;

     if (vk >= VK_0) and (vk <= VK_9 ) then
     begin
       output_buffer^ := char(vk);
       result := 1;
       exit;
     end;

     if( (vk >= VK_A) and (vk <= VK_Z) ) then
     begin
       output_buffer^ := char(vk + 32);
       result := 1;
       exit;
     end;

     result := 0;
end;
{$ENDIF}

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
      if (
        (ToASCII(Message.CharCode, (Message.KeyData shr 16) and 7, KeyState, @Buffer, 0) > 0) or
        (Message.CharCode = VK_F2)
        )
        and (Shift * [ssCtrl, ssAlt] = []) and (CharCode >= 32) then
      begin
        //case Buffer[0] of
        EditColumn := FocusedColumn;
        DoEdit;
        //send first key which triggered the editor to newly created editor
        If CanEdit(FocusedNode,EditColumn) and (Message.CharCode<>VK_F2) then
        begin
          amsg.msg:=WM_CHAR;
          amsg.wParam:=ord(Buffer[0]);
          amsg.lParam:=0;
          EditLink.ProcessMessage( amsg);
        end;
      end
      else
        inherited WMKeyDown(Message);
    end
    //else
    //  inherited WMKeyDown(Message);
end;

procedure TSOGrid.Loaded;
begin
  inherited Loaded;
  fDefaultSettings := GetSettings;
end;


procedure TSOGrid.LoadData;
var
  AFocused,ASelected: ISuperObject;
  ANodes:TNodeArray;
  ANode: PVirtualNode;
  TopRec: ISuperObject;
  PrevReadOnly: Boolean;
begin
  if (Data = nil) or (Data.AsArray = nil) then
  begin
    PrevReadOnly := toReadOnly in TreeOptions.MiscOptions;
    TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toReadOnly];
    inherited Clear;
    if PrevReadOnly then
      TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toReadOnly];
  end
  else
  begin
    //Stores previous focused and selected rows
    BeginUpdate;
    try
      if Length(FKeyFieldsList) > 0 then
        ASelected := ExtractFields(SelectedRows,FKeyFieldsList)
      else
        ASelected := Nil;
      AFocused := FocusedRow;
      TopRec := GetNodeSOData(TopNode);
      SetLength(ANodes,0);
      PrevReadOnly := toReadOnly in TreeOptions.MiscOptions;
      TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toReadOnly];
      try
        inherited Clear;
        if ParentProperty='' then
          RootNodeCount := Data.AsArray.Length
        else
        begin
          // find root nodes (Parent value is Nil or not found in current data array)
          // RootData := GetSORootNodes(Data,ParentRow);
          // RootNodeCount := RootData.AsArray.Length;
          // For each root node, set SOChildren recursively
        end;
      finally
        if PrevReadOnly then
          TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toReadOnly];
      end;
    finally
      try
        // restore selected nodes
        if (ASelected <>Nil) and (ASelected.AsArray<>Nil) and (ASelected.AsArray.Length>0) then begin
          SelectedRows := ASelected;
        end;

        // Restore focused node
        if AFocused <> Nil then
          SetFocusedRowNoClearSelection(AFocused);

        // Restore top visible node
        if (TopRec <> Nil) and not (tsScrolling in TreeStates) then
        begin
          if KeyFieldsNames<>'' then
            ANodes := NodesForKey(TopRec)
          else
            ANodes := NodesForData(TopRec);
        end;

      finally
        EndUpdate;
        for ANode in ANodes do begin
          TopNode := ANode;
          break;
        end;
        // restore visible focused column
        ScrollIntoView(FocusedColumn, False);
      end;
      // should update the popup menu as some items depend on whether or not there is data
      CleanPopupMenu;
      FillPopupMenu;
      // clear all filters after loading
      if fFilterOptions.ClearAterLoadingData then
        fFilterOptions.ClearFilters
      else
        fFilterOptions.ApplyFilters;
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
  if FData = Value then
    exit;
  FData := Value;
  LoadData;

  UpdateSelectedAndTotalLabel;
end;

procedure TSOGrid.SetFocusedColumnObject(AValue: TSOGridColumn);
begin
  if (AValue<>Nil) and Header.Columns.IsValidColumn(AValue.Index) then
    FocusedColumn:=AValue.Index;
end;

procedure TSOGrid.SetFocusedRow(AValue: ISuperObject);
begin
  ClearSelection;
  SetFocusedRowNoClearSelection(AValue);
end;

procedure TSOGrid.SetGridSettings(AValue: String);
begin
  if AValue <> '' then
    Settings := SO(DecodeStringBase64(AValue));
end;

procedure TSOGrid.SetFocusedRowNoClearSelection(AValue: ISuperObject);
var
  ANodes:TNodeArray;
begin
  If AValue = Nil then
    FocusedNode:=Nil
  else
  begin
    ANodes := NodesForKey(AValue);
    if length(ANodes)>0 then
    begin
      FocusedNode:=ANodes[0];
      Selected[ANodes[0]] := True;
      ScrollIntoView(FocusedNode,False);
    end;
  end;
end;

procedure TSOGrid.SetKeyFieldsNames(AValue: String);
begin
  FKeyFieldsList := StrSplit(AValue,';',True);
end;

procedure TSOGrid.SetOnCutToClipBoard(AValue: TNotifyEvent);
begin
  FOnCutToClipBoard:=AValue;
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
  if Assigned(FOnGetText) and (Column >= 0) and Header.Columns.IsValidColumn(Column) then
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
begin
  inherited Create(AOwner);
  fZebraLightness := 250;
  DefaultText := '';
  SetLength(FKeyFieldsList,0);

  FItemDataOffset := AllocateInternalDataArea(SizeOf(TSOItemData));
  fExportFormatOptions := DefaultExportFormatOptions;
  fFilterOptions := TTisGridFilterOptions.Create(self);
  WantTabs:=True;
  TabStop:=True;


  with TreeOptions do
  begin
    PaintOptions := PaintOptions - [toShowRoot] +
      [toAlwaysHideSelection, toShowHorzGridLines, toShowVertGridLines, toHideFocusRect];
    SelectionOptions := SelectionOptions + [toExtendedFocus, toSimpleDrawSelection, toRightClickSelect, toDisableDrawSelection];
    MiscOptions := MiscOptions + [toGridExtensions, toFullRowDrag] -
      [toWheelPanning,toEditOnClick,toEditOnDblClick,toToggleOnDblClick];

    AutoOptions := AutoOptions + [toAutoSort,toAutoChangeScale];
  end;

  Header.Options := [hoColumnResize, hoDblClickResize, hoDrag,
    hoShowSortGlyphs, hoVisible,hoHeaderClickAutoSort];
  Header.Style := hsFlatButtons;
  Header.DefaultHeight:=18;
  Header.MinHeight:=18;
  Header.Height:=18;
  Header.MaxHeight:=100;

  DefaultNodeHeight:=18;

  // Initialisation de la boite de dialogue de recherche
  FindDlg := TFindDialog.Create(Self);
  FindDlg.OnFind := @FindDlgFind;
  FindDlg.Options := FindDlg.Options + [frHideMatchCase,frHideEntireScope,frEntireScope,frHideUpDown];

  Header.PopupMenu :=  TSOHeaderPopupMenu.Create(Self);
  Header.PopupMenu.PopupComponent := Self;

end;

procedure TSOGrid.FillPopupMenu;

  procedure _AddItem(ACaption: string; AShortcut: TShortCut; AEvent: TNotifyEvent);
  var
    vMenuItem: TMenuItem;
  begin
    vMenuItem := PopupMenu.Items.Find(ACaption);
    if vMenuItem = Nil then
    begin
      vMenuItem := TMenuItem.Create(PopupMenu);
      with vMenuItem do
      begin
        Caption := ACaption;
        ShortCut := AShortcut;
        OnClick := AEvent;
        Tag := POPUP_ITEM_TAG;
      end;
      PopupMenu.Items.Add(vMenuItem);
    end;
  end;

begin
  if not Assigned(PopupMenu) then
    PopupMenu := TPopupMenu.Create(self);
  // fire the original user event, if it exists, for customize its items
  if Assigned(PopupMenu.OnPopup) then
    PopupMenu.OnPopup(PopupMenu);
  if PopupMenu.Items.Count > 0 then
    _AddItem('-', 0, nil);
  _AddItem(GSConst_Find, ShortCut(Ord('F'), [ssCtrl]), @DoFindText);
  _AddItem(GSConst_FindNext, VK_F3, @DoFindNext);
  {_AddItem(GSConst_FindReplace, ShortCut(Ord('H'), [ssCtrl]),
    @DoFindReplace);}
  _AddItem('-', 0, nil);
  if (not (toReadOnly in TreeOptions.MiscOptions)) and Assigned(FOnCutToClipBoard) then
    _AddItem(GSConst_Cut, ShortCut(Ord('X'), [ssCtrl]), @DoCutToClipBoard);
  _AddItem(GSConst_Copy, ShortCut(Ord('C'), [ssCtrl]), @DoCopyToClipBoard);
  _AddItem(GSConst_CopyCell, ShortCut(Ord('C'), [ssCtrl,ssShift]), @DoCopyCellToClipBoard);
  if AllowDataExport then
    _AddItem(GSConst_CopySpecial, ShortCut(Ord('S'), [ssCtrl,ssShift]), @DoCopySpecialToClipboard);
  if not (toReadOnly in TreeOptions.MiscOptions) and ((toEditable in TreeOptions.MiscOptions) or Assigned(FOnBeforePaste))  then
    _AddItem(GSConst_Paste, ShortCut(Ord('V'), [ssCtrl]), @DoPaste);
  _AddItem('-', 0, nil);
  if not (toReadOnly in TreeOptions.MiscOptions) or Assigned(FOnNodesDelete) then
    _AddItem(GSConst_DeleteRows, ShortCut(VK_DELETE, [ssCtrl]), @DoDeleteRows);
  if toMultiSelect in TreeOptions.SelectionOptions then
    _AddItem(GSConst_SelectAll, ShortCut(Ord('A'), [ssCtrl]), @DoSelectAllRows);
  _AddItem('-', 0, nil);
  if AllowDataExport then
  begin
    if (toMultiSelect in TreeOptions.SelectionOptions) then
      _AddItem(GSConst_ExportSelected, 0, @DoExport)
    else
      _AddItem(GSConst_ExportAll, 0, @DoExport);
  end;
  {if (HMPrint = 0) then
    HMPrint := _AddItem(GSConst_Print, ShortCut(Ord('P'), [ssCtrl]), @DoPrint);
  _AddItem('-', 0, nil);
  HMExpAll := _AddItem(GSConst_ExpandAll, Shortcut(Ord('E'), [ssCtrl, ssShift]),
    @DoExpandAll);
  HMCollAll := _AddItem(GSConst_CollapseAll, Shortcut(Ord('R'), [ssCtrl, ssShift]),
    @DoCollapseAll);}
  _AddItem('-', 0, nil);
  _AddItem(GSConst_CustomizeColumns, 0, @DoCustomizeColumns);
  if (csDesigning in ComponentState) or ShowAdvancedColumnsCustomize then
    _AddItem(GSConst_AdvancedCustomizeColumns, 0, @DoAdvancedCustomizeColumns);
end;

procedure TSOGrid.CleanPopupMenu;
var
  i: Integer;
begin
  if Assigned(PopupMenu) then
    for i := PopupMenu.Items.Count-1 downto 0 do
      if PopupMenu.Items[i].Tag = POPUP_ITEM_TAG then
        PopupMenu.Items.Delete(i);
end;

destructor TSOGrid.Destroy;
begin
  FData := Nil;
  if Assigned(FindDlg) then
    FreeAndNil(FindDlg);
  fFilterOptions.Free;
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

function TSOGrid.GetSelectedRows: ISuperObject;
var
  N: PVirtualNode;
begin
  N := GetFirstSelected;
  Result := TSuperObject.Create(stArray);
  while (N <> nil) do
  begin
    Result.AsArray.Add(GetNodeSOData(N));
    N := GetNextSelected(N,True);
  end;
end;

procedure TSOGrid.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: Double);
var
  i: Integer;
begin
  if (AMode in [lapAutoAdjustForDPI]) then
  begin
    Header.MinHeight := max(18,round(Header.MinHeight * AXProportion)+1);
    Header.MaxHeight := max(18,round(Header.MaxHeight * AXProportion)+1);
    Header.DefaultHeight := max(18,round(Header.DefaultHeight * AXProportion)+1);
    Header.Height := max(18,round(Header.Height * AXProportion)+1);

    for i := 0 to header.Columns.Count-1 do
    begin
      header.Columns[i].MaxWidth:=round(header.Columns[i].MaxWidth * AXProportion);
      header.Columns[i].Width:=round(header.Columns[i].Width * AXProportion);
      header.Columns[i].MinWidth:=round(header.Columns[i].MinWidth * AXProportion);
    end;
  end;
end;

procedure TSOGrid.DoChange(Node: PVirtualNode);
begin
  inherited DoChange(Node);
  if Assigned(FSelectedAndTotalLabel) then
    SetSelectedAndTotalLabel(FSelectedAndTotalLabel);
end;

procedure TSOGrid.DoEnter;
begin
  inherited DoEnter;
  FillPopupMenu;
end;

procedure TSOGrid.DoExit;
begin
  CleanPopupMenu;
  inherited DoExit;
end;

procedure TSOGrid.DoContextPopup(aMousePos: TPoint; var aHandled: Boolean);
begin
  // on MacOS and Linux DoContextPopup is called even when user clicks on Header
  // - this will prevent not call PopupMenu from grid, instead using Header.PopupMenu
  if Header.InHeader(aMousePos) then
  begin
    aHandled := True;
    exit;
  end;
  CleanPopupMenu;
  FillPopupMenu;
  inherited DoContextPopup(aMousePos, aHandled);
end;

procedure TSOGrid.DoHeaderMouseDown(aButton: TMouseButton; aShift: TShiftState;
  aX, aY: Integer);
begin
  if not Assigned(Header.PopupMenu) then
    Header.PopupMenu := TSOHeaderPopupMenu.Create(self);
  if Header.PopupMenu is TSOHeaderPopupMenu then
    with Header.PopupMenu as TSOHeaderPopupMenu do
    begin
      Header.PopupMenu.PopupComponent := self;
      FillPopupMenu;
    end;
  inherited DoHeaderMouseUp(aButton, aShift, aX, aY);
end;

procedure TSOGrid.DoEditValidated(const aColumn: TSOGridColumn;
  const aCurValue: Variant; var aNewValue: Variant; var aAbort: Boolean);
begin
  if Assigned(fOnEditValidated) then
    fOnEditValidated(self, aColumn, aCurValue, aNewValue, aAbort);
end;

procedure TSOGrid.DoNodeFiltering(aNode: PVirtualNode);
begin
  if Assigned(fOnNodeFiltering) then
    fOnNodeFiltering(self, aNode);
end;

procedure TSOGrid.FixDesignFontsPPI(const ADesignTimePPI: Integer);
begin
  inherited FixDesignFontsPPI(ADesignTimePPI);
  DoFixDesignFontPPI(Header.Font, ADesignTimePPI);
end;

procedure TSOGrid.ScaleFontsPPI(const AToPPI: Integer; const AProportion: Double
  );
begin
  inherited ScaleFontsPPI(AToPPI, AProportion);
  DoScaleFontPPI(Header.Font, AToPPI, AProportion);
end;

function TSOGrid.CheckedRows: ISuperObject;
var
  N: PVirtualNode;
begin
  N := GetFirstChecked;
  Result := TSuperObject.Create(stArray);
  while (N <> nil) do
  begin
    Result.AsArray.Add(GetNodeSOData(N));
    N := GetNextChecked(N,csCheckedNormal,True);
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
  compresult : TSuperCompareResult;
begin
  Result := inherited DoCompare(Node1, Node2,Column);

  n1 := Nil;
  n2 := Nil;

  // pending appended node appears at the end
  if FPendingAppendObject<>Nil then
  begin
    n1 := GetNodeSOData(Node1);
    if (n1<>Nil) and (n1 = FPendingAppendObject) then
      result := 1
    else
    begin
      n2 := GetNodeSOData(Node2);
      if (n2<>Nil) and (n2 = FPendingAppendObject) then
        result := -1;
    end;
  end;

  if (result = 0) and (Column >= 0) then
  begin
    propname := TSOGridColumn(Header.Columns[column]).PropertyName;
    if n1 = Nil then
      n1 := GetNodeSOData(Node1);
    if n2 = Nil then
      n2 := GetNodeSOData(Node2);

    if Assigned(OnSOCompareNodes) then
    begin
      if propname <> '' then
        OnSOCompareNodes(Self, n1, n2,[propname], Result)
      else
        OnSOCompareNodes(Self, n1, n2, FKeyFieldsList, Result)
    end
    else
    begin
      if (propname<>'') and (n1 <> nil) and (n2 <> nil) then
      begin
        d1 := n1[propname];
        d2 := n2[propname];
        if d1=nil then d1:=SO('""');
        if d2=nil then d2:=SO('""');
        if (d1 <> nil) and (d2 <> nil) then
        begin
          CompResult := d1.Compare(d2);
          case compresult of
            cpLess : Result := -1;
            cpEqu  : Result := 0;
            cpGreat : Result := 1;
            cpError :  Result := strcompare(UTF8Encode(n1.S[propname]),UTF8Encode(n2.S[propname]));
          end;
        end
        else
          Result := -1;
      end
      else
        Result := 0;
    end;
  end;
end;

procedure TSOGrid.DoHeaderClickSort(HitInfo: TVTHeaderHitInfo);
begin
  if (HitInfo.Shift=[]) and (HitInfo.Button = mbLeft) then
  begin
    if Header.SortColumn = HitInfo.Column then
    begin
      if Header.SortDirection = sdAscending then
        Header.SortDirection := sdDescending
      else if Header.SortDirection = sdDescending then
      begin
        Header.SortColumn := -1;
        Header.SortDirection := sdAscending;
      end;
    end
    else
    begin
      Header.SortColumn := HitInfo.Column;
      Header.SortDirection := sdAscending;
    end;
  end;
end;

// Return list of nodes which match the sodata key (multiple fields)
// If FKeyFieldsList is not empty, only fields from FKeyFieldsList are taken n account from sodata
function TSOGrid.NodesForKey(sodata: ISuperObject): TNodeArray;
var
  ASO: ISuperObject;
  p: PVirtualNode;
  key: ISuperObject;
begin
  SetLength(Result, 0);
  if sodata= Nil then
    Exit;
  if sodata.AsObject = Nil then
    exit;
  key := SOExtractFields(sodata,FKeyFieldsList);
  p := GetFirst(True);
  while (p <> nil) do
  begin
    ASO := GetNodeSOData(p);
    if (ASO <> nil) and ((ASO = key) or (SOCompareByKeys(ASO,key,FKeyFieldsList)=cpEqu)) then
    begin
      SetLength(Result, length(Result) + 1);
      Result[length(Result) - 1] := p;
    end;
    p := GetNext(p,True);
  end;
end;

function TSOGrid.NodesForKeyStr(Keyname,Keyvalue: String): TNodeArray;
var
  ASO: ISuperObject;
  p: PVirtualNode;
begin
  SetLength(Result, 0);
  p := GetFirst(True);
  while (p <> nil) do
  begin
    ASO := GetNodeSOData(p);
    if (ASO <> nil) and (ASO.S[Keyname] = KeyValue ) then
    begin
      SetLength(Result, length(Result) + 1);
      Result[length(Result) - 1] := p;
    end;
    p := GetNext(p,True);
  end;
end;


procedure TSOGrid.AddRows(SOArray: ISuperObject;AllowDuplicates:Boolean=False);
var
  ToAdd    : ISuperObject;
begin
  if not Assigned(SOArray) then
    Exit;

  BeginUpdate;
  if not Assigned(Data) then
    Data := TSuperObject.Create(stArray);
  try
    for ToAdd in SOArray do
      // don't add if already in grid...
      if AllowDuplicates or (
          (Length(KeyFieldsList)=0) and (Length(NodesForData(ToAdd))=0)) or (Length(NodesForKey(ToAdd))=0) then
        Data.AsArray.Add(ToAdd);
  finally
    EndUpdate;
    // Load all nodes
    LoadData;
  end;
end;

procedure TSOGrid.DeleteRows(SOArray: ISuperObject);
var
  ToDelete  : ISuperObject;
  i: integer;
  ANode: PVirtualNode;
  ANodesArray: TNodeArray;

begin
  if not Assigned(SOArray) then
    Exit;

  if Data <> Nil then
    for ToDelete in SOArray do begin
      //Remove from SO backend
      for i := data.AsArray.Length-1 downto 0  do
        if data.AsArray[i] = ToDelete then
          Data.AsArray.Delete(i);

      //Remove from node array
      ANodesArray := NodesForData(ToDelete);
      for ANode in ANodesArray do
        DeleteNode(ANode,ANode=ANodesArray[Length(ANodesArray)-1]);
    end;
end;

procedure TSOGrid.DeleteSelectedRows;
begin
  DoDeleteRows(Self);
end;

function TSOGrid.NodesForData(sodata: ISuperObject): TNodeArray;
var
  ASO: ISuperObject;
  p: PVirtualNode;
begin
  SetLength(Result, 0);
  if sodata= Nil then
    Exit;
  if sodata.AsObject = Nil then
    exit;
  p := GetFirst(True);
  while (p <> nil) do
  begin
    ASO := GetNodeSOData(p);
    if (ASO <> nil) and (ASO = sodata) then
    begin
      SetLength(Result, length(Result) + 1);
      Result[length(Result) - 1] := p;
      //OutputDebugString('kkk');
    end;
    p := GetNext(p,True);

  end;
end;

procedure TSOGrid.InvalidateFordata(sodata: ISuperObject);
var
  p: PVirtualNode;
begin
  if not Assigned(sodata) then
    Exit;
  p := GetFirst(True);
  while (p <> nil) do
  begin
    if GetNodeSOData(p) = sodata then
      InvalidateNode(p);
    p := GetNext(p,True);
  end;
end;

function TSOGrid.GetFocusedColumnObject: TSOGridColumn;
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
  b64 := EncodeStringBase64(UTF8Encode(Settings.AsJSon));
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

    st := UTF8Encode(SelectedRows.AsJSon(True));
    Clipboard.AddFormat(ClipbrdJson, st[1], Length(st));

  finally
    Clipboard.Close;
  end;
end;

procedure TSOGrid.DoCopyCellToClipBoard(Sender: TObject);
var
  focusedField,st: ansistring;
  row,cells : ISuperObject;
begin
  if (FocusedColumnObject<>Nil) and (FocusedRow<>Nil) then
  begin
    Clipboard.Open;
    try
      Clipboard.Clear;
      focusedField:=FocusedColumnObject.PropertyName;
      cells := TSuperObject.Create(stArray);
      for row in SelectedRows do
        cells.AsArray.Add(row.S[focusedField]);
      st := Join(#13#10,cells);
      Clipboard.AddFormat(CF_Text, st[1], Length(st)+1);

      st := UTF8Encode(cells.AsJSon(True));
      Clipboard.AddFormat(ClipbrdJson, st[1], Length(st));

    finally
      Clipboard.Close;
    end;
  end;
end;

procedure TSOGrid.DoCopySpecialToClipboard(aSender: TObject);
var
  buf: RawByteString;
  efo: TTisGridExportFormatOptionAdapter;
  tst: TTisGridTextSourceTypeAdapter;
  params: record
    Selection: TVSTTextSourceType;
    Format: TTisGridExportFormatOption;
    Columns: record
      VisibleOnly: Boolean;
      Translated: Boolean;
    end;
  end;
  form: TVisCopySpecial;
begin
  form := TVisCopySpecial.Create(self.Owner);
  try
    Clipboard.Open;
    efo.EnumsToStrings(form.FormatCombo.Items, [efoCsv, efoJson]);
    form.FormatCombo.ItemIndex := 0;
    tst.EnumsToStrings(form.SelectionCombo.Items);
    form.SelectionCombo.ItemIndex := 0;
    if form.ShowModal <> mrOK then
      exit;
    Clipboard.Clear;
    params.Selection := tst.CaptionToEnum(form.SelectionCombo.Text);
    params.Format := efo.CaptionToEnum(form.FormatCombo.Text);
    params.Columns.VisibleOnly := form.ColumnsVisibleOnlyCheckBox.Checked;
    params.Columns.Translated := form.TranslatedColumnsCheckBox.Checked;
    case params.Format of
      efoCsv:
      begin
        buf := ContentToCsv(params.Selection, ',', params.Columns.VisibleOnly, params.Columns.Translated);
        Clipboard.AddFormat(CF_Text, buf[1], Length(buf));
      end;
      efoJson:
      begin
        buf := ContentToJson(params.Selection, params.Columns.VisibleOnly);
        Clipboard.AddFormat(CF_Text, buf[1], Length(buf));
        Clipboard.AddFormat(ClipbrdJson, buf[1], Length(buf));
      end;
    else
      raise Exception.Create('Format not enabled to copy from it.');
    end;
  finally
    Clipboard.Close;
    form.Free;
  end;
end;

procedure TSOGrid.DoCutToClipBoard(Sender: TObject);
begin
  if Assigned(FOnCutToClipBoard) then
    FOnCutToClipBoard(Sender);
end;

procedure TSOGrid.DoDeleteRows(Sender: TObject);
var
  sel, todelete: ISuperObject;
  i: integer;
  newFocusedNode:PVirtualNode;
  ANodes:TNodeArray;

begin
  if Assigned(FOnNodesDelete) then
  begin
    todelete := SelectedRows;
    FOnNodesDelete(self,todelete);
  end
  else
    if Dialogs.MessageDlg(GSConst_Confirmation, Format(GSConst_ConfDeleteRow,[SelectedCount]),
        mtConfirmation, mbYesNoCancel, 0) = mrYes then
    begin
      todelete := SelectedRows;
      newFocusedNode := Nil;
      if todelete.AsArray.Length>0 then
      begin
        ANodes := NodesForData(todelete.AsArray[0]);
        if length(ANodes)>0 then
          newFocusedNode := ANodes[0];
        if newFocusedNode <> Nil then
          newFocusedNode:=GetPrevious(newFocusedNode);
        for sel in todelete do
        begin
          //standalone grid
          for i := 0 to Data.AsArray.Length - 1 do
            if Data.AsArray[i] = sel then
            begin
              Data.AsArray.Delete(i);
              break;
            end;
        end;
        DeleteSelectedNodes;
      end;
      if newFocusedNode<>Nil then
      begin
        FocusedNode:= newFocusedNode;
        Selected[FocusedNode] := True;
      end;
    end;
end;

procedure TSOGrid.PasteRows(Rows: ISuperObject);
var
  row: ISuperObject;
  canpaste: Boolean;
begin
  if Data = Nil then
    Data := TSuperObject.Create(stArray);
  for row in Rows do
  begin
    if Assigned(FOnBeforePaste) then
      canpaste := FOnBeforePaste(Self,row)
    else
      canpaste := True;
    if canpaste then
      Data.AsArray.Add(row);
  end;
  LoadData;
end;

procedure TSOGrid.DoPaste(Sender: TObject);
begin
  PasteRows(ClipboardSOData);
end;

procedure TSOGrid.DoSelectAllRows(Sender: TObject);
begin
  SelectAll(False);
end;

procedure TSOGrid.DoPrint(Sender: TObject);
begin
  raise Exception.Create('Not implemented');
end;

procedure TSOGrid.DoCustomizeColumns(Sender: TObject);
begin
  Header.PopupMenu.PopUp;
end;

procedure TSOGrid.DoAdvancedCustomizeColumns(Sender: TObject);
begin
  Customize;
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
  newdata:ISuperObject;
begin
  Result:=inherited DoKeyAction(CharCode, Shift);
end;

procedure TSOGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  Inherited Notification(AComponent,Operation);
end;

procedure TSOGrid.Clear;
var
  PrevReadOnly: Boolean;
begin
  PrevReadOnly := toReadOnly in TreeOptions.MiscOptions;
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toReadOnly];
  inherited Clear;
  if PrevReadOnly then
    TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toReadOnly];
  FData := nil;
end;

function SortColumnsPosition(c1,c2:TCollectionItem):integer;
begin
  if (c1=Nil) or (c2=Nil) then
  begin
    if c1 = Nil then
      Result := -1;
    if c2 = Nil then
      Result := 1;
    if (c1=Nil) and (c2=Nil) then
      Result := 0;
  end
  else
  if TSOGridColumn(c1).tag<TSOGridColumn(c2).tag then
    result := -1
  else
  if TSOGridColumn(c1).tag>TSOGridColumn(c2).tag then
    result := 1
  else
    Result := 0;
end;

procedure TSOGrid.ReorderColumns;
var
  i:TColumnIndex;
  FocColumn:TSOGridColumn;
begin
  try
    FocColumn:=FocusedColumnObject;
    for i:=0 to Header.Columns.Count-1 do
      Header.Columns[i].Tag:=Header.Columns[i].Position;
    Header.Columns.Sort(@SortColumnsPosition);
    for i:=0 to Header.Columns.count-1 do
      Header.Columns[i].Position :=  TSOGridColumn(Header.Columns[i]).Index ;
  finally
    FocusedColumnObject := FocColumn;
  end;
end;

procedure TSOGrid.Customize;
var
  i:Integer;
  col : TSOGridColumn;
  target : TSOGrid;
begin
  BeginUpdate;
  //BeginOperation;
  try
    With TSOGridEditor.Create(Application) do
    try
        target := self;
        asogrid.Header.Height:=target.Height;
        for i:=0 to target.Header.Columns.count-1 do
        begin
           col := ASOGrid.Header.Columns.Add as TSOGridColumn;
           col.Assign(target.Header.Columns[i]);
           col.Options:=target.Header.Columns[i].Options;
        end;
        asogrid.Settings := target.Settings;
        asogrid.FilterOptions.Assign(target.FilterOptions);
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
          target.FilterOptions.Assign(asogrid.FilterOptions);
        end;
    finally
      Free;
    end;

  finally
    EndUpdate;
  end;
end;

procedure TSOGrid.NotifyChange(EventType:TSODataEvent;Row:ISuperObject;OldValues,NewValues:ISuperObject);
begin
  //deFieldChange, deDataSetChange,deUpdateRecord, deUpdateState,deFieldListChange
  if not (csDestroying in ComponentState) then
  begin
    if (EventType in [deUpdateRecord]) and (Row<>Nil) then
      InvalidateFordata(Row)
    else if EventType in [deUpdateState,deDataSetChange,deAddrecord,deDeleteRecord] then
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

procedure TSOGrid.DoBeforeCellPaint(aCanvas: TCanvas; aNode: PVirtualNode;
  aColumn: TColumnIndex; aCellPaintMode: TVTCellPaintMode; aCellRect: TRect;
  var aContentRect: TRect);
begin
  //Pour affichage lignes multiselect en gris clair avec cellule focused en bleu
  if (aCellPaintMode = cpmPaint) and (toMultiSelect in TreeOptions.SelectionOptions) and
    (vsSelected in aNode^.States) then
  begin
    if not Focused or (aColumn <> FocusedColumn) or (aNode <> FocusedNode) then
    begin
      aCanvas.Brush.Color := clLtGray;
      aCanvas.FillRect(aCellRect);
    end
    else
    if (aColumn = FocusedColumn) and (aNode = FocusedNode) and Focused then
    begin
      aCanvas.Brush.Color := Colors.SelectionRectangleBlendColor;
      aCanvas.FillRect(aCellRect);
    end;
  end
  else
  if (aCellPaintMode = cpmPaint) and not (toMultiSelect in TreeOptions.SelectionOptions) and
     (aNode = FocusedNode) then
  begin
    if (aColumn <> FocusedColumn) then
    begin
      aCanvas.Brush.Color := clLtGray;
      aCanvas.FillRect(aCellRect);
    end
    else
    begin
      aCanvas.Brush.Color := Colors.SelectionRectangleBlendColor;
      aCanvas.FillRect(aCellRect);
    end;
  end;
  inherited DoBeforeCellPaint(aCanvas, aNode, aColumn, aCellPaintMode, aCellRect, aContentRect);
end;

procedure TSOGrid.DoTextDrawing(var aPaintInfo: TVTPaintInfo;
  const aText: string; aCellRect: TRect; aDrawFormat: cardinal);
const
  cDark = 255;
var
  vHue, vSaturation, vLightness: Byte;
begin
  // to display multiselect rows in light gray with focused cell in blue
  if (Focused or not (toHideSelection in TreeOptions.PaintOptions) or (toPopupMode in TreeOptions.PaintOptions))  and
        (vsSelected in aPaintInfo.Node^.States) and
        (aPaintInfo.Node = FocusedNode) and
        (aPaintInfo.Column = FocusedColumn) then
    aPaintInfo.Canvas.Font.Color := Colors.SelectionTextColor
  else
  begin
    ColorToHLS(aPaintInfo.Canvas.Brush.Color, vHue, vLightness, vSaturation);
    if vLightness>128 then
      aPaintInfo.Canvas.Font.Color := clBlack
    else
      aPaintInfo.Canvas.Font.Color := clWhite;

    //aPaintInfo.Canvas.Font.Color := HLStoColor(vHue, cDark - vLightness, 255);
  end;
  inherited DoTextDrawing(aPaintInfo, aText, aCellRect, aDrawFormat);
end;

procedure TSOGrid.DoBeforeItemErase(aCanvas: TCanvas; aNode: PVirtualNode;
  const aItemRect: TRect; var aColor: TColor; var aEraseAction: TItemEraseAction);
var
  vHue, vSaturation, vLightness: Byte;
begin
  if fZebraPaint and (aNode <> nil) and Odd(aNode^.Index) then
  begin
    ColorToHLS(aColor, vHue, vLightness, vSaturation);
    if vLightness < fZebraLightness then
      aColor := HLStoColor(vHue, vLightness - fZebraLightness, vSaturation)
    else
      aColor := HLStoColor(vHue, vLightness + fZebraLightness, vSaturation);
    aEraseAction := eaColor;
  end;
  inherited DoBeforeItemErase(aCanvas, aNode, aItemRect, aColor, aEraseAction);
end;

function TSOGrid.FindText(Txt: string): PVirtualNode;
begin
  Result := Nil;
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
          SetFocusSafe;
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
    ShowMessageFmt(GSConst_NoRecordFind,[TextToFind]);
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
  // Side effect if we use Data = Nil as an indicator of lazy loading.
  //if not Assigned(FData) then
  //  FData := TSuperObject.Create(stArray);
  Result := FData;
end;

function TSOGrid.GetFocusedRow: ISuperObject;
var
  N: PVirtualNode;
begin
  N := FocusedNode;
  if N<>Nil then
    Result := GetNodeSOData(N)
  else
    Result := Nil;
end;

function TSOGrid.GetGridSettings: String;
begin
  Result := EncodeStringBase64(UTF8Encode(Settings.AsJSon));
end;

function TSOGrid.GetKeyFieldsNames: String;
begin
  result := StrJoin(';',FKeyFieldsList);
end;

procedure TSOGrid.DoUndoLastUpdate(Sender: TObject);
begin
end;

procedure TSOGrid.DoRevertRecord(Sender: TObject);
begin
end;

Function GetTempFileName(Const Prefix,ext : String) : String;
Var
  I : Integer;
  Start : String;
  Disc  : String;

begin
  Start:=GetTempDir;
  I:=0;
  Disc := '';
  Repeat
    Result:=Format('%s%s%s%s',[Start,Prefix,Disc,ext]);
    Disc := Format('%.5d',[i]);
    Inc(I);
  Until not FileExists(Result);
end;

function TSOGrid.ContentToCsv(aSource: TVSTTextSourceType;
  const aSeparator: string; aColumnsVisibleOnly: Boolean;
  aColumnsTranslated: Boolean): RawUtf8;
var
  values,Row,Rows,value:ISuperObject;
  i:Integer;
begin
  if aSource in [tstAll,tstInitialized,tstVisible] then
    Rows := Data
  else
    Rows := SelectedRows;

  result :='';

  values := TSuperObject.Create(stArray);

  for i:=0 to Header.Columns.Count-1 do
  begin
    if (coVisible in Header.Columns[i].Options) or (not aColumnsVisibleOnly) then
    begin
      if aColumnsTranslated then
        values.AsArray.Add('"'+UTF8Decode(TSOGridColumn(Header.Columns[i]).Text)+'"')
      else
        values.AsArray.Add('"'+UTF8Decode(TSOGridColumn(Header.Columns[i]).PropertyName)+'"');
    end;
  end;
  Result := Result+Join(aSeparator,values)+LineEnding;
  for row in rows do
  begin
    values := TSuperObject.Create(stArray);
    for i:=0 to Header.Columns.Count-1 do
    begin
      if (coVisible in Header.Columns[i].Options) or (not aColumnsVisibleOnly) then
      begin
        value := Row[TSOGridColumn(Header.Columns[i]).PropertyName];
        if (value<>Nil) and not ObjectIsNull((value)) then
        begin
          if values.DataType in [stInt,stDouble] then
            values.AsArray.Add(value.AsString)
          else
            values.AsArray.Add(UTF8Decode(AnsiQuotedStr(UTF8Encode(value.AsString),'"')))
        end
        else
          values.AsArray.Add('""');
      end;
    end;
    Result := Result+Join(aSeparator,values)+LineEnding;
  end;
end;

function TSOGrid.ContentToJson(aSource: TVSTTextSourceType;
  aColumnsVisibleOnly: Boolean): RawUtf8;
var
  json: RawUtf8;
  rows: PDocVariantData;
  cols, res: TDocVariantData;
  col: TSOGridColumn;
  c: Integer;
begin
  if aSource in [tstAll, tstInitialized, tstVisible] then
    json := SynUnicodeToUtf8(Data.AsJSon)
  else
    json := SynUnicodeToUtf8(SelectedRows.AsJSon);
  rows := _Safe(_JsonFastFloat(json));
  cols.InitArray([], JSON_FAST_FLOAT);
  for c := 0 to Header.Columns.Count-1 do
  begin
    col := TSOGridColumn(Header.Columns[c]);
    if ((coVisible in col.Options) or not aColumnsVisibleOnly) then
      cols.AddItemText(col.PropertyName);
  end;
  res.Clear;
  rows^.Reduce(cols.ToRawUtf8DynArray, False, res);
  result := res.ToJson;
end;

procedure TSOGrid.ExportData(const aFileName: TFileName;
  const aSelection: TVSTTextSourceType);

  procedure _SaveToFile(const aBuffer: RawUtf8);
  var
    buf: PUtf8Char;
    l: LongInt;
    st: File;
  begin
    AssignFile(st, aFileName);
    Rewrite(st,1);
    try
      buf := PUtf8Char(aBuffer + #0);
      l := StrLen(buf);
      BlockWrite(st, buf^, l);
    finally
      CloseFile(st);
    end;
  end;

var
  buf: RawUtf8;
begin
  buf := '';
  case SysUtils.LowerCase(ExtractFileExt(aFileName)) of
    '.csv':
      buf := ContentToCsv(aSelection, ',');
    '.json':
      buf := ContentToJson(aSelection);
    '.html', '.htm':
      buf := StringToUtf8(ContentToHTML(aSelection));
    '.rtf':
      buf := StringToUtf8(ContentToRTF(aSelection));
    '.txt':
      buf := StringToUtf8(ContentToText(aSelection, ','));
  else
    raise Exception.CreateFmt('File extension "%s" is not valid to export.', [ExtractFileExt(aFileName)]);
  end;
  _SaveToFile(buf);
end;

procedure TSOGrid.ExportExcel(Prefix:String='';Selection:TVSTTextSourceType=tstAll;Separator:Char=',');
var
  tempfn:Utf8String;
  txt:Utf8String;
  txtbuf:PChar;
  l:LongInt;
  st:File;
begin
  tempfn:=GetTempFileName(Prefix,'.csv');
  AssignFile(st,tempfn);
  Rewrite(st,1);
  try
    txt := ContentToCsv(Selection,Separator)+#0;
    txtbuf := pchar(txt);
    l := strlen(txtbuf);
    BlockWrite(st,txtbuf^,l);
  finally
    CloseFile(st);
    OpenDocument(tempfn);
  end;
end;

procedure TSOGrid.DoExport(Sender: TObject);

  function _GetSelectionType: TVSTTextSourceType;
  begin
    if (toMultiSelect in TreeOptions.SelectionOptions) then
      result := tstSelected
    else
      result := tstAll;
  end;

var
  dlg: TSaveDialog;
begin
  dlg := TSaveDialog.Create(nil);
  try
    dlg.Title := Application.Title;
    dlg.Filter := GetExportDialogFilter;
    dlg.FileName := 'data';
    dlg.Options := dlg.Options + [ofOverwritePrompt];
    if dlg.Execute then
      ExportData(dlg.FileName, _GetSelectionType);
  finally
    dlg.Free;
  end;
end;


procedure TSOGrid.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
  const AText: string);
var
  ItemData: PSOItemData;
  RowData, OldCellData, NewCellData: ISuperObject;
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
          //standalone grid
          OldCelldata := RowData[PropertyName];
          NewCellData := TSuperObject.Create(UTF8Decode(AText));
          RowData[PropertyName] := NewCellData;
        end
        else
        begin
          PropertyName:=DefaultText;
          OldCelldata := RowData[DefaultText];
          NewCellData := TSuperObject.Create(UTF8Decode(AText));
          RowData[DefaultText] := NewCellData;
        end;
      end;
    end;
  end;
  //reset to allow append
  FPendingAppendObject:=Nil;
  inherited DoNewText(Node, Column, AText);
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
      //lcl: probably  necessary
      {$IFNDEF UNIX}
      if (Message.Msg in [WM_NCLBUTTONDOWN, WM_NCRBUTTONDOWN, WM_NCMBUTTONDOWN]) and not Focused and CanFocus then
        SetFocus;
      {$ENDIF}
      inherited;
    end
    //// BUGFIX Tranquil IT Systems.
    else
       Message.Msg := 0;
    //// end BUGFIX
  end;
end;

//----------------- TSOStringEditLink ------------------------------------------------------------------------------------

procedure TSOStringEditLink.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  CanAdvance: Boolean;
begin
  CanAdvance := True;
  case Key of
    VK_ESCAPE:
      if CanAdvance then
      begin
        fControl.Internal.OnExit := nil; // prevents an Access Violation
        fGrid.SetFocusSafe; // needed if grid.parent is a Frame
        fGrid.CancelEditNode;
        Key := 0;
      end;
    VK_RETURN:
      if CanAdvance then
      begin
        fGrid.EndEditNode;
        Key := 0;
      end;
    VK_UP,
    VK_DOWN:
      begin
        // Consider special cases before finishing edit mode.
        CanAdvance := Shift = [];
        if fControl.Internal is TCustomComboBox then
          CanAdvance := CanAdvance and not TCustomComboBox(fControl.Internal).DroppedDown;
        if CanAdvance then
        begin
          // Forward the keypress to the tree. It will asynchronously change the focused node.
          PostMessage(fGrid.Handle, LM_KEYDOWN, Key, 0);
          Key := 0;
        end;
      end;
  end;
end;

procedure TSOStringEditLink.EditExit(Sender: TObject);
begin
  if Assigned(fControl) then
  begin
    if (toAutoAcceptEditChange in fGrid.TreeOptions.StringOptions) then
      fGrid.EndEditNode
    else
      fGrid.CancelEditNode;
  end;
end;

function TSOStringEditLink.NewControl: TTisGridControl;
begin
  result := TTisGridEditControl.Create;
  result.SetOnKeyDown(@EditKeyDown);
  result.SetOnExit(@EditExit);
  result.Internal.Visible := False;
  result.Internal.Parent := fGrid;
end;

constructor TSOStringEditLink.Create;
begin
  inherited Create;
end;

destructor TSOStringEditLink.Destroy;
begin
  fControl.Free;
  inherited Destroy;
end;

function TSOStringEditLink.BeginEdit: Boolean; stdcall;
begin
  result := True;
  fControl.Internal.Show;
  fControl.Internal.SetFocus;
end;

function TSOStringEditLink.CancelEdit: Boolean; stdcall;
begin
  result := True;
  fControl.Internal.Hide;
end;

function TSOStringEditLink.EndEdit: Boolean; stdcall;
var
  vCol: TSOGridColumn;
  vDoc: ISuperObject;
  vCur, vNew: Variant;
  vAborted: Boolean;
begin
  result := True;
  vDoc := nil;
  vCur := nil;
  vCol := fGrid.FindColumnByIndex(fColumn);
  vDoc := fGrid.GetNodeSOData(fNode);
  if vDoc <> nil then
    vCur := vDoc.S[vCol.PropertyName];
  vNew := fControl.GetValue;
  vAborted := False;
  fGrid.DoEditValidated(vCol, vCur, vNew, vAborted);
  try
    if vAborted then
      exit;
    if VarIsNull(vNew) then
      vDoc.O[vCol.PropertyName] := NULL
    else
    begin
      fGrid.Text[fNode, fColumn] := VarToStr(vNew);
      vDoc.S[vCol.PropertyName] := VarToStr(vNew);
    end;
  finally
    FreeAndNil(fControl); // for do not perform any event from it
    fGrid.InvalidateNode(fNode);
    fGrid.SetFocusSafe;
  end;
end;

function TSOStringEditLink.GetBounds: TRect; stdcall;
begin
  result := fControl.Internal.BoundsRect;
end;

function TSOStringEditLink.PrepareEdit(aTree: TBaseVirtualTree; aNode: PVirtualNode;
  aColumn: TColumnIndex): Boolean; stdcall;
var
  text: string;
begin
  result := True;
  fGrid := aTree as TSOGrid;
  fNode := aNode;
  fColumn := aColumn;
  FreeAndNil(fControl);
  fControl := NewControl;
  fGrid.GetTextInfo(fNode, fColumn, fControl.Internal.Font, FTextBounds, text);
  fControl.SetValue(text);
  fControl.IsReadOnly := not aTree.CanEdit(aNode,aColumn);
end;

procedure TSOStringEditLink.ProcessMessage(var aMessage: TLMessage); stdcall;
begin
  PostMessage(fControl.Internal.Handle, aMessage.Msg, aMessage.wParam, aMessage.lParam);
end;

procedure TSOStringEditLink.SetBounds(R: TRect); stdcall;
var
  dummy: Integer;
begin
  // Since we don't want to activate grid extensions in the tree (this would
  // influence how the selection is drawn)
  // we have to set the edit's width explicitly to the width of the column.
  fGrid.Header.Columns.GetColumnBounds(fColumn, dummy, R.Right);
  fControl.Internal.BoundsRect := R;
end;

{ TTisGridExportFormatOptionAdapter }

const
  GRID_EXPORT_FORMAT_OPTIONS: array[TTisGridExportFormatOption] of record
    Caption: string;
    Extension: string;
    Filter: string;
  end = (
    (Caption: 'RTF'; Extension: '.rtf'; Filter: 'RTF (*.rtf)|*.rtf'),
    (Caption: 'HTML'; Extension: '.html'; Filter: 'HTML (*.html)|*.html'),
    (Caption: 'Text'; Extension: '.text'; Filter: 'Text (*.txt)|*.txt'),
    (Caption: 'CSV'; Extension: '.csv'; Filter: 'CSV (*.csv)|*.csv'),
    (Caption: 'JSON'; Extension: '.json'; Filter: 'JSON (*.json)|*.json')
  );

function TTisGridExportFormatOptionAdapter.EnumToCaption(
  const aValue: TTisGridExportFormatOption): string;
begin
  result := GRID_EXPORT_FORMAT_OPTIONS[aValue].Caption;
end;

function TTisGridExportFormatOptionAdapter.CaptionToEnum(const aValue: string): TTisGridExportFormatOption;
var
  i: TTisGridExportFormatOption;
begin
  result := low(TTisGridExportFormatOption);
  for i := low(GRID_EXPORT_FORMAT_OPTIONS) to high(GRID_EXPORT_FORMAT_OPTIONS) do
    if GRID_EXPORT_FORMAT_OPTIONS[i].Caption = aValue then
    begin
      result := i;
      exit;
    end;
end;

procedure TTisGridExportFormatOptionAdapter.EnumsToStrings(aDest: TStrings;
  const aCustom: TTisGridExportFormatOptions);
var
  i: TTisGridExportFormatOption;
begin
  for i := low(TTisGridExportFormatOption) to high(TTisGridExportFormatOption) do
    if i in aCustom then
      aDest.Append(EnumToCaption(i));
end;

function TTisGridExportFormatOptionAdapter.ExtensionToEnum(
  const aValue: TFileName): TTisGridExportFormatOption;
var
  i: TTisGridExportFormatOption;
begin
  result := low(TTisGridExportFormatOption);
  for i := low(GRID_EXPORT_FORMAT_OPTIONS) to high(GRID_EXPORT_FORMAT_OPTIONS) do
    if GRID_EXPORT_FORMAT_OPTIONS[i].Extension = aValue then
    begin
      result := i;
      exit;
    end;
end;

function TTisGridExportFormatOptionAdapter.EnumToFilter(
  const aValue: TTisGridExportFormatOption): string;
begin
  result := GRID_EXPORT_FORMAT_OPTIONS[aValue].Filter;
end;

{ TTisGridTextSourceTypeAdapter }

const
  GRID_TEXT_SOURCE_TYPES: array[TVSTTextSourceType] of record
    Caption: string;
  end = (
    (Caption: 'All'),
    (Caption: 'Initialized'),
    (Caption: 'Selected'),
    (Caption: 'CutCopySet'),
    (Caption: 'Visible'),
    (Caption: 'Checked')
  );

function TTisGridTextSourceTypeAdapter.EnumToCaption(
  const aValue: TVSTTextSourceType): string;
begin
  result := GRID_TEXT_SOURCE_TYPES[aValue].Caption;
end;

function TTisGridTextSourceTypeAdapter.CaptionToEnum(const aValue: string): TVSTTextSourceType;
var
  i: TVSTTextSourceType;
begin
  result := low(TVSTTextSourceType);
  for i := low(GRID_TEXT_SOURCE_TYPES) to high(GRID_TEXT_SOURCE_TYPES) do
    if GRID_TEXT_SOURCE_TYPES[i].Caption = aValue then
    begin
      result := i;
      exit;
    end;
end;

procedure TTisGridTextSourceTypeAdapter.EnumsToStrings(aDest: TStrings;
  const aCustom: TTisGridTextSourceTypes);
var
  i: TVSTTextSourceType;
begin
  for i := low(TVSTTextSourceType) to high(TVSTTextSourceType) do
    if i in aCustom then
      aDest.Append(EnumToCaption(i));
end;

{ TWidgetHelper }

procedure TWidgetHelper.SetFocusSafe;
var
  p: TWinControl;
begin
  try
    if Visible and Enabled then
    begin
      p := Parent;
      if p.InheritsFrom(TFrame) then
        p.SetFocusSafe;
      while Assigned(p) and p.Enabled do
      begin
        if p.InheritsFrom(TTabSheet) then
          TPageControl(p.Parent).ActivePage := TTabSheet(p);
        p := p.Parent;
      end;
      SetFocus;
    end;
  except
  end;
end;


end.
