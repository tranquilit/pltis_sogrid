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

  Classes, SysUtils, VirtualTrees, Controls,
  SuperObject, Menus, Graphics, Clipbrd, LCLType, Dialogs,LMessages,StdCtrls,Types,IdHTTP,DefaultTranslator;

type

  ESOUnknownKey = class(Exception);
  ESOUndefinedDatasource = class(Exception);
  ESOUndefinedConnection = class(Exception);
  ESONoDataReturned = class(Exception);

  TSOGrid = class;

  TSODataEvent = (deDataSetChange,
    deAddrecord,deDeleteRecord,deUpdateRecord, deUpdateState,deFieldListChange);

  TSODataChangeEvent = procedure (EventType:TSODataEvent;Row:ISuperObject;OldValues,NewValues:ISuperObject) of object;
  TSuperObjectRowEvent = procedure (ARow:ISuperObject) of object;

  TSOUpdateStatus = (usUnmodified, usModified, usInserted, usDeleted);
  TSOUpdateStatusSet = SET OF TSOUpdateStatus;

  TSOUpdateMode = (upWhereAll, upWhereChanged, upWhereKeyOnly);
  TSOResolverResponse = (rrSkip, rrAbort, rrMerge, rrApply, rrIgnore);

  TSOGetKeyEvent = procedure (ARow:ISuperObject;var key:Variant) of object;

  TSOCompareNodesEvent = procedure(Sender: TSOGrid; Node1, Node2: ISuperObject; const Columns: Array of String;
    var Result: Integer) of object;

  ISODataView = interface
    ['{2DF865FF-684D-453E-A9F0-7D7307DD0BDD}']
    procedure NotifyChange(EventType:TSODataEvent;Row:ISuperObject;OldValues,NewValues:ISuperObject);
  end;

  ISORowChanges = interface;

  { TSOConnection }
  TSOConnection = class(TComponent)
  private
    FIdHttpClient: TIdHTTP;
    FPassword: String;
    FServerURL: String;
    FUsername: String;
    procedure SetPassword(AValue: String);
    procedure SetServerURL(AValue: String);
    procedure SetUsername(AValue: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CallServerMethod(httpMethod: String;
      Args: array of String; KWArgs:ISuperObject=Nil;PostParams: ISuperObject=Nil): String;

    // return a list of provider (=table) names available on server
    // name: ,  description:
    function GetProviders:ISuperObject; virtual;

    //return metadata for given provider as a json array of dict
    //fields:
    //  keys are : fieldname, label, type, length, required, reference, inkey, inupdate, inwhere
    //params:
    //  keys are : name, label, type
    function Metadata(provider:String):ISuperObject; virtual;

    // read data from server, given providername and params return json packet
    function LoadData(provider:String;Params:ISuperObject; root:String = 'content'):ISuperObject; virtual;

    // read data from server, given providername and params return json packet
    function Refresh(provider:String;Params:ISuperObject):ISuperObject; virtual;

    // send Delta to server for updates : delta is an array of dict :
    //  [  {'seq':1,'update_type':'update','old':{ inkey,inwhere and old fields values},'new':{} },...  ]
    //return remaining delta
    function ApplyUpdates(provider:String;Params:ISuperObject;Delta:ISORowChanges):ISORowChanges; virtual;

  published
    property ServerURL:String read FServerURL write SetServerURL;
    property Username:String read FUsername write SetUsername;
    property Password:String read FPassword write SetPassword;
    property IdHttpClient:TIdHTTP read FIdHttpClient;

  end;

  { ISORowChange }

  ISORowChange = interface
    ['{36CD5EC4-43EC-4F83-98CF-2E0EC68D14BC}']
    function GetKey: Variant;
    function GetNewValues: ISuperObject;
    function GetOldValues: ISuperObject;
    function GetRow: ISuperObject;
    function GetUpdateError: String;
    function GetUpdateType: TSOUpdateStatus;
    procedure SetKey(AValue: Variant);
    procedure SetNewValues(AValue: ISuperObject);
    procedure SetOldValues(AValue: ISuperObject);
    procedure SetRow(AValue: ISuperObject);
    procedure SetUpdateError(AValue: String);
    procedure SetUpdateType(AValue: TSOUpdateStatus);

    property UpdateType : TSOUpdateStatus read GetUpdateType write SetUpdateType;
    property Key:Variant read GetKey write SetKey;
    property Row:ISuperObject read GetRow write SetRow;
    property OldValues:ISuperObject read GetOldValues write SetOldValues;
    property NewValues:ISuperObject read GetNewValues write SetNewValues;
    property UpdateError:String read GetUpdateError write SetUpdateError;
  end;


  { TSORowChange }
  // a single change of dataset
  // reference counted for easy memory management
  TSORowChange = class(TInterfacedObject,ISORowChange)
  private
    FNewValues: ISuperObject;
    FOldValues: ISuperObject;
    FRow: ISuperObject;
    FUpdateError: String;
    FUpdateType: TSOUpdateStatus;
    FKey:Variant;
    function GetKey: Variant;
    function GetNewValues: ISuperObject;
    function GetOldValues: ISuperObject;
    function GetRow: ISuperObject;
    function GetUpdateError: String;
    function GetUpdateType: TSOUpdateStatus;
    procedure SetKey(AValue: Variant);
    procedure SetNewValues(AValue: ISuperObject);
    procedure SetOldValues(AValue: ISuperObject);
    procedure SetRow(AValue: ISuperObject);
    procedure SetUpdateError(AValue: String);
    procedure SetUpdateType(AValue: TSOUpdateStatus);
  public
    property UpdateType : TSOUpdateStatus read GetUpdateType write SetUpdateType;
    property Row:ISuperObject read GetRow write SetRow;
    property OldValues:ISuperObject read GetOldValues write SetOldValues;
    property NewValues:ISuperObject read GetNewValues write SetNewValues;
    property UpdateError:String read GetUpdateError write SetUpdateError;
    constructor Create;
    destructor Destroy; override;
  end;


  { TSORowChanges }
  TSODataSource = class;
  TSORowChangesEnumerator = class;

  { ISORowChanges }

  ISORowChanges = interface(IInterfaceList)
    ['{310B128D-2751-4860-BC15-2F1AA1A0B2B7}']
    function Get(i : Integer) : ISORowChange;
    function GetDataSource: TSODataSource;
    procedure Put(i : Integer;item : ISORowChange);
    function GetEnumerator: TSORowChangesEnumerator;

    //associated datasource for flatten operations
    property Datasource:TSODataSource read GetDataSource;
    //overriden to return proper interface
    property Items[Index : Integer] : ISORowChange read Get write Put;default;
    //Add a new change to the list
    function AddChange(UpdateType:TSOUpdateStatus;Row:ISuperObject=nil;OldValues:ISuperObject=Nil;NewValues:ISuperObject=Nil):integer;
    // Create a new list of changes grouping all changes of each row into one delta
    procedure RemoveRowChanges(row:ISuperObject);
    // return list of changes as a JSon object
    function Flatten:ISORowChanges;
    // Create a new list of changes grouping all changes of each row into one delta
    function Delta:ISuperObject;
    // return True if AChange is the first one for the specified row
    function IsFirstChange(AChange:ISORowChange):Boolean;

  end;

  TSORowChanges = class;

  { TSORowChangesEnumerator }

  TSORowChangesEnumerator = class(TInterfaceListEnumerator)
  public
    constructor Create(AList: TSORowChanges);
    function GetCurrent: ISORowChange;
    property Current: ISORowChange read GetCurrent;
  end;

  // list of rowchanges to log the history of insert/update/delete and build a delta for further database sync
  TSORowChanges = class(TInterfaceList,ISORowChanges)
  private
    Fdatasource:TSODataSource;
    function Get(i : Integer) : ISORowChange;
    function GetDataSource: TSODataSource;
    procedure Put(i : Integer;item : ISORowChange);

  public
    constructor Create(adatasource:TSODatasource);
    destructor Destroy; override;

    function GetEnumerator: TSORowChangesEnumerator;

    function AddChange(UpdateType:TSOUpdateStatus;Row:ISuperObject=nil;OldValues:ISuperObject=Nil;NewValues:ISuperObject=Nil):integer;
    //creates flat json array of changes, merging all changes of each row into one
    function Delta:ISuperObject;
    //return true if AChange is the first one for target row.
    function IsFirstChange(AChange:ISORowChange):Boolean;

    // Create a new list of changes grouping all changes of each row into one delta
    function Flatten:ISORowChanges;
    // Remove all changes from list which reference the row parameter
    procedure RemoveRowChanges(row:ISuperObject);

    property Datasource:TSODataSource read GetDataSource;
    property Items[Index : Integer] : ISORowChange read Get write Put;default;

  end;

  { TSODataSource }
  TSODataSource = class(TComponent)
  private
    FConnection: TSOConnection;
    FData: ISuperObject;
    FChangeLog: ISORowChanges;
    FDataViews: TList;
    FDisableCount:Integer;
    FOnDataChange: TSODataChangeEvent;
    FOnGetKey: TSOGetKeyEvent;
    FOnNewRecord: TSuperObjectRowEvent;
    FOnStateChange: TNotifyEvent;
    FOnUpdateRecord: TSODataChangeEvent;
    FParams: ISuperObject;
    FProviderName: String;
    FRoot: String;

    function GetActive: Boolean;
    function GetEnabled: Boolean;
    function GetParamsJSON: String;
    procedure RemoveRecordFromData(row: ISuperObject);
    procedure Reset;
    procedure SetActive(AValue: Boolean);
    procedure SetConnection(AValue: TSOConnection);
    procedure SetData(AData: ISuperObject);
    procedure SetDataChanges(AValue: ISORowChanges);
    procedure SetEnabled(Value: Boolean);
    procedure SetParams(AValue: ISuperObject);
    procedure SetParamsJSON(AValue: String);
    procedure SetProviderName(AValue: String);
    function UndoSingle(change: ISORowChange;Notify:Boolean=True): ISuperObject;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure open;
    procedure close;

    // List of visual components to be notified of data changes
    property DataViews:TList read FDataViews write FDataViews;

    // Data store : current state of records
    property Data:ISuperObject read FData write SetData;

    // log of all changes for Applyupdates, UndoLastChange etc...:
    property ChangeLog:ISORowChanges read FChangeLog write SetDataChanges;

    // add a component to the list of notified objects
    procedure RegisterView(AComponent:TComponent);
    procedure UnregisterView(AComponent:TComponent);
    procedure NotifyChange(EventType:TSODataEvent;Row:ISuperObject=Nil;OldValues:ISuperObject=Nil;NewValues:ISuperObject=Nil);

    procedure LoadDataset; Virtual;
    procedure Emptydataset; Virtual;

    procedure EnableControls;
    procedure DisableControls;

    //Append a new row to the collection. update views after initialisation
    function  AppendRecord(new:ISuperObject=Nil):ISuperObject;Virtual;
    //Delete a row from the collection
    procedure DeleteRecord(row:ISuperObject); Virtual;

    //update a field of a record log changes and notify views
    procedure UpdateValue(row:ISuperObject;PropertyName:String;NewValue:ISuperObject);Virtual;
    //Update multiple values of a record, log changes and notify views
    procedure UpdateRecord(row:ISuperObject;NewValues:ISuperObject);Virtual;

    //load data as json in a file
    procedure LoadFromFile(Filename:String); Virtual;
    //save data as json in a file
    procedure SaveToFile(Filename:String); Virtual;

    // empty the log of changes : applyupdates is no more possible
    procedure MergeChangeLog; virtual;
    // count of single changes
    function ChangeCount:Integer; virtual;
    // undo the last single change
    function UndolastChange:ISuperObject; virtual;
    // revert row record to initial state, reverting all changes logged in datachanges
    function RevertRecord(row:ISuperObject):ISuperObject;Virtual;

    //send the delta to the datasource and remove from change log the applied updates
    function ApplyUpdates:Integer; virtual;

    //reload data from server keeping initial filtering/parameters
    procedure Refresh; virtual;

    //paste json data or csv data from the clipboard
    procedure PasteFromClipboard;

    //get a unique key for the row
    function GetKey(Row:ISuperObject):Variant; virtual;

    //returns the row associated with a key
    function FindKey(key:Variant):ISuperObject; virtual;


  published
    property Connection: TSOConnection read FConnection write SetConnection;
    //name of REST Provider on connection
    property ProviderName:String read FProviderName write SetProviderName;
    property Params:ISuperObject read FParams write SetParams;
    //Set the Params SuperObject and load data
    property ParamsJSON:String read GetParamsJSON write SetParamsJSON;
    property Root:String read FRoot write FRoot;

    //True if Data contains rows else False : data is empty
    property Active:Boolean read GetActive write SetActive;

    //if True, attached views are notified from changes through OnDataChange
    property Enabled: Boolean read GetEnabled write SetEnabled default True;

    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    //Called when data or state is changed and Enabled is True
    property OnDataChange: TSODataChangeEvent read FOnDataChange write FOnDataChange;
    //Called when a row has been modified, before changes is logged into ChangeLog and before views are notified
    property OnUpdateRecord: TSODataChangeEvent read FOnUpdateRecord write FOnUpdateRecord;
    //Called when new row is appended to Data
    property OnNewRecord: TSuperObjectRowEvent read FOnNewRecord write FOnNewRecord;
    //called to get the unique key of a row as a variant
    property OnGetKey:TSOGetKeyEvent read FOnGetKey write FOnGetKey;

    //called if Applyupdates can't apply all changes
    //property OnApplyUpdateError
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
    FKeyFieldsList: Array of String;
    FOnCutToClipBoard: TNotifyEvent;
    FOnSOCompareNodes: TSOCompareNodesEvent;
    FShowAdavancedColumnsCustomize: Boolean;
    FShowAdvancedColumnsCustomize: Boolean;
    FTextFound: boolean;
    FindDlg: TFindDialog;
    FZebraPaint: Boolean;
    ReplaceDialog: TReplaceDialog;

    FColumnToFind: integer;
    FStartSearchNode: PVirtualNode;
    FTextToFind: string;

    FData: ISuperObject;
    FItemDataOffset: integer;
    FOnGetText: TSOGridGetText;

    FDatasource: TSODataSource;

    FPendingAppendObject:ISuperObject;

    FDefaultPopupMenu: TPopupMenu;
    FMenuFilled: boolean;
    HMUndo, HMRevert: HMENU;
    HMFind, HMFindNext, HMReplace: HMENU;
    HMCut, HMCopy, HMCopyCell, HMPast, HMFindReplace: HMENU;
    HMInsert, HMDelete, HMSelAll: HMENU;
    HMExcel, HMPrint: HMENU;
    HMCollAll, HMExpAll: HMENU;
    HMCustomize: HMENU;
    HMAdvancedCustomize: HMENU;

    function FocusedPropertyName: String;
    function GetData: ISuperObject;
    function GetFocusedColumnObject: TSOGridColumn;
    function GetFocusedRow: ISuperObject;
    function GetKeyFieldsNames: String;
    function GetSettings: ISuperObject;

    procedure SetColumnToFind(AValue: integer);
    procedure SetData(const Value: ISuperObject);
    procedure SetDatasource(AValue: TSODataSource);
    procedure SetFocusedColumnObject(AValue: TSOGridColumn);
    procedure SetFocusedRow(AValue: ISuperObject);
    procedure SetKeyFieldsNames(AValue: String);
    procedure SetOnCutToClipBoard(AValue: TNotifyEvent);
    procedure SetOptions(const Value: TStringTreeOptions);
    function GetOptions: TStringTreeOptions;
    procedure SetSelectedRows(AValue: ISuperObject);
    procedure SetSettings(AValue: ISuperObject);
    procedure SetShowAdvancedColumnsCustomize(AValue: Boolean);

    procedure WMKeyDown(var Message: TLMKeyDown); message LM_KEYDOWN;


  protected

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

    procedure DoBeforeItemErase(ACanvas: TCanvas; Node: PVirtualNode;
      const ItemRect: TRect; var AColor: TColor;
      var EraseAction: TItemEraseAction); override;

    function FindText(Txt: string): PVirtualNode;
    procedure FindDlgFind(Sender: TObject);

    procedure DoFindText(Sender: TObject);
    procedure DoFindNext(Sender: TObject);
    procedure DoFindReplace(Sender: TObject);

    procedure DoUndoLastUpdate(Sender: TObject); virtual;
    procedure DoRevertRecord(Sender: TObject); virtual;
    procedure DoExportExcel(Sender: TObject); virtual;
    procedure DoCopyToClipBoard(Sender: TObject); virtual;
    procedure DoCopyCellToClipBoard(Sender: TObject); virtual;
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

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetNodeSOData(Node: PVirtualNode): ISuperObject;
    procedure LoadData;
    property Data: ISuperObject read GetData write SetData;
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

    //Ajouter les colonnes en s'inspirant du contenu Data
    procedure CreateColumnsFromData(FitWidth,AppendMissingAsHidden: Boolean);

    function ContentAsCSV(Source: TVSTTextSourceType; const Separator: String
      ): Utf8String;

    // Creates a temporary CSV file and open it in the default app
    procedure ExportExcel(Prefix:String='';Selection: TVSTTextSourceType=tstAll; Separator:Char=#9);

  published
    property OnGetText: TSOGridGetText read FOnGetText write FOnGetText;
    property Datasource: TSODataSource read FDataSource write SetDatasource;

    property OnCutToClipBoard: TNotifyEvent read FOnCutToClipBoard write SetOnCutToClipBoard;

    property ShowAdvancedColumnsCustomize: Boolean read FShowAdvancedColumnsCustomize write SetShowAdvancedColumnsCustomize;
    property KeyFieldsNames: String read GetKeyFieldsNames write SetKeyFieldsNames;

    property OnSOCompareNodes: TSOCompareNodesEvent read FOnSOCompareNodes write FOnSOCompareNodes;

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

  function EncodeURIComponent(const ASrc: AnsiString): AnsiString;

implementation

uses soutils, soclipbrd, base64, IniFiles,LCLIntf,messages,forms,
    variants,tisstrings,sogrideditor;

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
  GSConst_CopyCell = 'Copier la cellule';
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
  GSConst_AdvancedCustomizeColumns = 'Personnalisation avancée du tableau...';

type
  TSOItemData = record
    JSONData: ISuperObject;
  end;
  PSOItemData = ^TSOItemData;


  function EncodeURIComponent(const ASrc: Ansistring): AnsiString;
  const
    HexMap: AnsiString = '0123456789ABCDEF';

        function IsSafeChar(ch: Integer): Boolean;
        begin
          if (ch >= 48) and (ch <= 57) then Result := True    // 0-9
          else if (ch >= 65) and (ch <= 90) then Result := True  // A-Z
          else if (ch >= 97) and (ch <= 122) then Result := True  // a-z
          else if (ch = 33) then Result := True // !
          else if (ch >= 39) and (ch <= 42) then Result := True // '()*
          else if (ch >= 45) and (ch <= 46) then Result := True // -.
          else if (ch = 95) then Result := True // _
          else if (ch = 126) then Result := True // ~
          else Result := False;
        end;
  var
    I, J: Integer;
  begin
    Result := '';    {Do not Localize}

    I := 1; J := 1;
    SetLength(Result, Length(ASrc) * 3); // space to %xx encode every byte
    while I <= Length(ASrc) do
    begin
      if IsSafeChar(Ord(ASrc[I])) then
      begin
        Result[J] := ASrc[I];
        Inc(J);
      end
      else if ASrc[I] = ' ' then
      begin
        Result[J] := '+';
        Inc(J);
      end
      else
      begin
        Result[J] := '%';
        Result[J+1] := HexMap[(Ord(ASrc[I]) shr 4) + 1];
        Result[J+2] := HexMap[(Ord(ASrc[I]) and 15) + 1];
        Inc(J,3);
      end;
      Inc(I);
    end;

    SetLength(Result, J-1);
  end;

{ TSORowChangesEnumerator }

constructor TSORowChangesEnumerator.Create(AList: TSORowChanges);
begin
  inherited Create(AList);
end;

function TSORowChangesEnumerator.GetCurrent: ISORowChange;
begin
  result := inherited GetCurrent as ISORowChange;
end;

procedure TSOConnection.SetPassword(AValue: String);
begin
  if FPassword=AValue then Exit;
  FPassword:=AValue;
end;

procedure TSOConnection.SetServerURL(AValue: String);
begin
  if RightStr(AValue,1)='/' then
    Delete(AValue,length(AValue),1);
  if FServerURL=AValue then Exit;
  FServerURL:=AValue;
end;

procedure TSOConnection.SetUsername(AValue: String);
begin
  if FUsername=AValue then Exit;
  FUsername:=AValue;
end;

{ TSOConnection }
constructor TSOConnection.Create(AOwner: TComponent);
begin
  FIdHttpClient := TIdHTTP.Create(Self);
  FIdHttpClient.HandleRedirects:=True;
  inherited;
end;

destructor TSOConnection.Destroy;
begin
  FIdHttpClient.Free;
  inherited Destroy;
end;

//call https://server_url/action/args0/args1/args...?paramkey=paramvalue&paramkey=paramvalue
//  postcontent for POST or PUT http method.

function TSOConnection.CallServerMethod(httpMethod: String;
  Args: array of String; KWArgs:ISuperObject=Nil;PostParams: ISuperObject=Nil): String;
var
  postSt:TMemoryStream;
  url,paramsStr : String;
  i : integer;
  key:ISuperObject;

begin
  try
    url := ServerURL;

    if length(args)>0 then
    begin
      paramsStr := '';
      for i:=low(Args) to high(Args) do
      begin
        if paramsStr<>'' then
          paramsStr := paramsStr+'/';
        paramsStr := paramsStr+EncodeURIComponent(Args[i]);
      end;
      url := url+'/'+paramsStr;
    end;

    if (KWArgs<>Nil) then
    begin
      paramsStr := '';
      if KWArgs.DataType<>stObject then
        Raise Exception.Create('Connection CallServerMethod kwargs is not a JSon Object type');
      for key in KWArgs.AsObject.GetNames do
      begin
        if paramsStr<>'' then
          paramsStr := paramsStr+'&';
        paramsStr := key.AsString+'='+EncodeURIComponent(KWArgs.S[key.AsString]);
      end;
      url := url+'?'+paramsStr;
    end;

    IdHttpClient.Request.ContentType:='application/json';
    IdHttpClient.Response.ContentType:='application/json';

    if Username<>'' then
    begin
      IdHttpClient.Request.BasicAuthentication:=True;
      IdHttpClient.Request.Username:=Username;
      IdHttpClient.Request.Password:=Password;
    end;

    if httpMethod='GET' then
      result := IdHttpClient.Get(url)
    else
    if httpMethod='PUT' then
    begin
      PostSt :=TMemoryStream.Create;
      try
        if PostParams<>Nil then
          PostParams.SaveTo(postSt);
        postSt.Seek(0,0);
        result := IdHttpClient.put(url,postSt);
      finally
        postSt.Free;
      end;
    end
    else
    if httpMethod='POST' then
    begin
      PostSt :=TMemoryStream.Create;
      try
        if PostParams<>Nil then
          PostParams.SaveTo(postSt);
        postSt.Seek(0,0);
        result := IdHttpClient.Post(url,postSt);
      finally
        postSt.Free;
      end;
    end
    else
    if httpMethod='DELETE' then
      result := IdHttpClient.Delete(url);

  except
    on e:EIdHTTPProtocolException do
      if e.ErrorCode = 400  then
        result := e.ErrorMessage
      else
        raise
    else
      Raise
  end;
end;

function TSOConnection.GetProviders: ISuperObject;
var
  content,patterns,pattern,prov:ISuperObject;
  crochidx:integer;
begin
  Result := TSuperObject.Create(stArray);
  content := SO(CallServerMethod('GET',['api','patterns.json']));
  if content<>Nil then
  begin
    patterns := content['content'];
    for pattern in patterns do
    begin
      crochidx := pos('[',pattern.AsString);
      if crochidx>0 then
      begin
        prov := TSuperObject.Create(stArray);
        prov.AsArray.Add(copy(pattern.AsString,1,crochidx-1));
        prov.AsArray.Add(copy(pattern.AsString,crochidx+1,length(pattern.AsString)-crochidx-3));
        Result.AsArray.Add(prov);
      end;
    end;
  end;
end;

function TSOConnection.Metadata(provider: String): ISuperObject;
begin
  Result := TSuperObject.Create;
end;

function TSOConnection.LoadData(provider: String; Params: ISuperObject; root:String = 'content'): ISuperObject;
var
  args,res,par:String;
  pathargs:TDynStringArray;
  response:ISuperObject;
begin
  pathargs := StrSplit(provider,'/');
  //pathargs[length(pathargs)-1] := pathargs[length(pathargs)-1]+'.json';
  res := CallServerMethod('GET',pathargs,Params);
  response := SO(res);
  if response=Nil then
  begin
    if Params<>Nil then
      par := Params.AsJSon
    else
      par := '""';
    Raise ESONoDataReturned.Create('GET method on server '+ServerURL+'/'+provider+' with params '+Par+' returned no data')
  end
  else
  try
    case response.DataType of
      stObject : Result := response[Root];
      stArray : Result := response;
    else
      raise Exception.Create('returned data is neither a json Array nor an Object');
    end;
  except
      Result := Nil;
      if Params<>Nil then
        par := Params.AsJSon
      else
        par := '""';
      Raise ESONoDataReturned.Create('GET method on server '+ServerURL+'/'+provider+' with params '+Par+' returned bad data : '+copy(res,1,1000))
  end
end;

function TSOConnection.Refresh(provider: String; Params: ISuperObject
  ): ISuperObject;
begin

end;

function TSOConnection.ApplyUpdates(provider: String; Params: ISuperObject;
  Delta: ISORowChanges): ISORowChanges;
var
  change:ISORowChange;
  key:Variant;
  providerparams:String;
  JSonResult:String;
  SOResult:ISuperObject;
begin
  providerparams:='';
  result := TSORowChanges.Create(Delta.Datasource);
  for change in Delta do
  try
    if change.UpdateType=usInserted then
    begin
      JSonResult := CallServerMethod('POST',[provider],Nil,change.Row);
      //try to change tmpId to new definitive if returned
      SOResult := SO(JSonResult);
      if SOResult.AsObject.Exists('id') then
        change.Row.AsObject['id'] := SOResult.AsObject['id'];
    end
    else
    if change.UpdateType=usDeleted then
      JSonResult := CallServerMethod('DELETE',[provider],Nil)
    else
    if change.UpdateType = usModified then
    begin
      //JSonResult := CallServerMethod('PUT',[provider,VarToStr(change.key)],Nil,change.NewValues)
      JSonResult := CallServerMethod('PUT',[provider],Nil,change.NewValues)
    end;
  except
    on E:EIdHTTPProtocolException do
    begin
      change.UpdateError := E.ErrorMessage;
      ShowMessage(E.Message+' : '+E.ErrorMessage);
      result.Add(change);
    end
    else
      raise;
  end;
end;

{ TSORowChanges }

function TSORowChanges.AddChange(UpdateType: TSOUpdateStatus;
  Row: ISuperObject; OldValues: ISuperObject;
  NewValues: ISuperObject): integer;
var
  newChange:ISORowChange;
begin
  newChange := TSORowChange.Create;
  newChange.UpdateType:=UpdateType;
  newChange.Row := Row;
  newChange.OldValues := OldValues;
  newChange.NewValues := NewValues;
  result := Add(newChange);
end;

function SOCompareSeq(so1,so2:Pointer):integer;
var
   seq1,seq2:Integer;
begin
  seq1 := ISuperObject(so1).I['seq'];
  seq2 := ISuperObject(so2).I['seq'];
  if seq1<seq2 then
    Result := -1
  else if seq1=seq2 then
    Result :=0
  else
    Result := 1;
end;

// build a JSON array of all changes
function TSORowChanges.Delta: ISuperObject;
var
  i,updateIdx:Integer;
  rowchange : ISORowChange;
  rowdelta: ISuperObject;
  key:Variant;
  seq: String;

begin
  result := TSuperObject.Create(stArray);
  for rowchange in Self do
  begin
    rowdelta := TSuperObject.Create;
    case rowchange.UpdateType of
      usInserted : rowdelta.S['update_type'] := 'insert';
      usModified : rowdelta.S['update_type'] := 'update';
      usDeleted : rowdelta.S['update_type'] := 'delete';
    end;
    rowdelta['old'] := rowchange.OldValues;
    rowdelta['new'] := rowchange.NewValues;

    key := Null;
    if (rowchange.UpdateType=usModified) and (rowchange.OldValues<>Nil) then
      key := Fdatasource.GetKey(rowchange.OldValues);
    if VarIsNull(Key) then
      key := Fdatasource.GetKey(rowchange.Row);
    if not VarIsNull(key) then
      rowdelta.AsObject['key'] := SO(key);
    showmessage(key);
    Result.AsArray.Add(rowdelta);
  end;
end;

function TSORowChanges.Get(i: Integer): ISORowChange;
begin
  result := inherited Get(i) as ISORowChange;
end;

function TSORowChanges.GetDataSource: TSODataSource;
begin
  Result := Fdatasource;
end;

procedure TSORowChanges.Put(i: Integer; item: ISORowChange);
begin
  inherited Put(i,item);
end;

constructor TSORowChanges.Create(adatasource: TSODatasource);
begin
  Fdatasource := adatasource;
  inherited create;
end;

destructor TSORowChanges.Destroy;
begin
  inherited;
end;

function TSORowChanges.IsFirstChange(AChange: ISORowChange): Boolean;
var
  i:integer;
begin
  Result := False;
  for i:=0 to count-1 do
    if AChange.row.AsObject = Items[i].Row.AsObject then
    begin
      Result := (AChange = Items[i] as ISORowChange);
      Break;
    end;
end;

function TSORowChanges.Flatten: ISORowChanges;
var
  i,updateIdx:Integer;
  PropertyName:ISuperObject;
  change,rowchange : ISORowChange;
  key:Variant;
  seq: String;

  // find a concatenated change for the row, or creates a new one by cloning first change
  function GetDeltaForChange(FlatChanges:ISORowChanges;change:ISORowChange):ISORowChange;
  begin
    result := Nil;
    //find a previous update for this row
    for result in FlatChanges do
    begin
      if result.row.AsObject = change.Row.AsObject then
        Break
    end;

    if Result = Nil then
    begin
      //not found, clone one
      Result := TSORowChange.Create;
      Result.UpdateType:=change.UpdateType;
      Result.Row := change.Row;
      if change.OldValues<> Nil then
        Result.OldValues := change.OldValues.Clone;

      if Result.NewValues<>Nil then
        Result.NewValues := change.NewValues.Clone;

      FlatChanges.Add(result);
    end
  end;

  //merge changes of change to rowchange
  //  if new change is delete, then this supersede previous changes
  //  if new change is update over an insert, keep insert mode but add new values
  //  if new change is insert, then keep it as this.
  procedure mergeChange(change,rowchange:ISORowChange);
  begin
    if change.UpdateType=usDeleted then
    begin
      rowchange.UpdateType:=usDeleted;
      rowchange.OldValues := Nil;
      rowchange.NewValues := Nil;
    end
    else if change.UpdateType=usModified then
    begin
      if rowchange.OldValues = Nil then
        rowchange.OldValues := TSuperObject.Create;

      // update over update, update oldvalues keeping oldest
      if rowchange.UpdateType = usModified then
      begin
        // keep oldest values for each field
        for PropertyName in change.OldValues.AsObject.GetNames do
          if not rowchange.OldValues.AsObject.Exists(PropertyName.AsString) then
            rowchange.OldValues[PropertyName.AsString] := change.OldValues[PropertyName.AsString];
      end;

      if rowchange.NewValues = Nil then
        rowchange.NewValues := TSuperObject.Create;
      // override to update to newest value
      rowchange.NewValues.Merge(change.NewValues,True);
    end
    else if change.UpdateType=usInserted then
    begin
      // this should not happen... we can not indert a row after
      rowchange.OldValues := Nil;
      if rowchange.NewValues = Nil then
        rowchange.NewValues := TSuperObject.Create;
      rowchange.NewValues.Merge(change.Row,True);
      rowchange.UpdateType:=usInserted;
    end;
  end;

begin
  Result := TSORowChanges.Create(Fdatasource);
  for change in self do
  begin
    //insert (into result) of get (from result) a change for change.row
    rowchange := GetDeltaForChange(Result,change);
    mergeChange(change,rowchange);
    key := Null;
    if (rowchange.UpdateType=usModified) and (rowchange.OldValues<>Nil) then
      key := Fdatasource.GetKey(rowchange.OldValues);
    if VarIsNull(Key) then
      key := Fdatasource.GetKey(rowchange.Row);
    if not VarIsNull(key) then
      rowchange.key := key;
  end;
end;

procedure TSORowChanges.RemoveRowChanges(row: ISuperObject);
var
  i:integer;
  change:ISORowChange;
begin
  for i:=count-1 downto 0 do
  begin
    change := Items[i];
    if change.Row.AsObject = row.AsObject then
      Delete(i);
  end;
end;

function TSORowChanges.GetEnumerator: TSORowChangesEnumerator;
begin
  result:=TSORowChangesEnumerator.Create(Self)
end;

{ TSORowChange }

procedure TSORowChange.SetNewValues(AValue: ISuperObject);
begin
  if FNewValues=AValue then Exit;
  FNewValues:=AValue;
end;

function TSORowChange.GetKey: Variant;
begin
  Result := FKey;
end;

function TSORowChange.GetNewValues: ISuperObject;
begin
  Result := FNewValues;
end;

function TSORowChange.GetOldValues: ISuperObject;
begin
  Result := FOldValues;
end;

function TSORowChange.GetRow: ISuperObject;
begin
  Result := FRow;
end;

function TSORowChange.GetUpdateError: String;
begin
  Result := FUpdateError;
end;

function TSORowChange.GetUpdateType: TSOUpdateStatus;
begin
  Result := FUpdateType;
end;

procedure TSORowChange.SetKey(AValue: Variant);
begin
  FKey:=AValue;
end;

procedure TSORowChange.SetOldValues(AValue: ISuperObject);
begin
  if FOldValues=AValue then Exit;
  FOldValues:=AValue;
end;

procedure TSORowChange.SetRow(AValue: ISuperObject);
begin
  if FRow=AValue then Exit;
  FRow:=AValue;
end;

procedure TSORowChange.SetUpdateError(AValue: String);
begin
  if FUpdateError=AValue then Exit;
  FUpdateError:=AValue;
end;

procedure TSORowChange.SetUpdateType(AValue: TSOUpdateStatus);
begin
  if FUpdateType=AValue then Exit;
  FUpdateType:=AValue;
end;

constructor TSORowChange.Create;
begin
  FUpdateType:=usUnmodified;
end;

destructor TSORowChange.Destroy;
begin
  FOldValues:=Nil;
  FNewValues:=Nil;
  FRow:=Nil;
  inherited Destroy;
end;


{ TSODataSource }

procedure TSODataSource.SetData(AData: ISuperObject);
begin
  if Adata = FData then
    Exit;
  FData := AData;
  //cancel delta
  MergeChangeLog;
  NotifyChange(deDataSetChange,Nil,Nil,Nil);
end;

procedure TSODataSource.SetDataChanges(AValue: ISORowChanges);
begin
  if FChangeLog=AValue then Exit;
  FChangeLog:=AValue;
  NotifyChange(deDataSetChange);
end;

procedure TSODataSource.SetEnabled(Value: Boolean);
begin
  if value then
    EnableControls
  else
    DisableControls;
end;

procedure TSODataSource.SetParams(AValue: ISuperObject);
begin
  if FParams=AValue then Exit;
  FParams:=AValue;
  Active := False;
end;

procedure TSODataSource.SetParamsJSON(AValue: String);
begin
  If (lowercase(AValue)='nil') or (AValue='') then
    Params := Nil
  else
    Params := SO(AValue);
end;

procedure TSODataSource.SetProviderName(AValue: String);
begin
  if RightStr(AValue,1)='/' then
    Delete(AValue,length(AValue),1);
  if FProviderName=AValue then Exit;
  FProviderName:=AValue;
  Active := False;
end;

constructor TSODataSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataViews := TList.Create;
  FChangeLog := TSORowChanges.Create(Self);
  FDisableCount:=0;
  FRoot := 'content';
end;

destructor TSODataSource.Destroy;
begin
  FreeAndNil(FDataViews);
  //be sure to free the interface
  FChangeLog := Nil;
  inherited Destroy;
end;

procedure TSODataSource.open;
begin
  Active:=True;
end;

procedure TSODataSource.close;
begin
  Active:=False;
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
  Row: ISuperObject; OldValues: ISuperObject; NewValues: ISuperObject);
var
  i:integer;
begin
  if Enabled then
  begin
    for i:=0 to DataViews.Count-1 do
      (TInterfacedObject(DataViews[i]) as ISODataView).NotifyChange(EventType,Row,OldValues,NewValues);
    if Assigned(OnDataChange) then
      OnDataChange(EventType,Row,OldValues,NewValues);
  end;
end;

procedure TSODataSource.LoadDataset;
begin
  if (ProviderName<>'') and (Connection<>Nil) then
    Data := Connection.LoadData(ProviderName,Params,Root)
  else
    Data := TSuperObject.Create(stArray);
end;

procedure TSODataSource.Emptydataset;
begin
  Data := TSuperObject.Create(stArray);
end;

procedure TSODataSource.EnableControls;
var
  OldEnabled:Boolean;
begin
  OldEnabled:=Enabled;
  if FDisableCount>0 then
    dec(FDisableCount);
  if Enabled<>OldEnabled then
    NotifyChange(deUpdateState ,Nil,Nil,Nil);
end;

procedure TSODataSource.DisableControls;
var
  OldEnabled:Boolean;
begin
  OldEnabled:=Enabled;
  inc(FDisableCount);
  if Enabled<>OldEnabled then
    NotifyChange(deUpdateState ,Nil,Nil,Nil);
end;

function TSODataSource.AppendRecord(new:ISuperObject=Nil): ISuperObject;
begin
  if not Assigned(Data) then
    Emptydataset;
  if new<>Nil then
    Result := new
  else
    result := TSuperObject.Create;
  if Assigned(OnNewRecord) then
    OnNewRecord(result);
  data.AsArray.Add(result);
  if Assigned(OnUpdateRecord) then
    OnUpdateRecord(deAddrecord,result,Nil,Nil);
  ChangeLog.AddChange(usInserted,Result);
  NotifyChange(deAddrecord,result);
end;

procedure TSODataSource.RemoveRecordFromData(row: ISuperObject);
var
  i,last:Integer;
begin
  last:=-1;
  for i:=data.AsArray.Length-1 downto 0 do
    if Data.AsArray[i]=row then
    begin
      Data.AsArray.Delete(i);
      last := i;
      Break;
    end;
  if last<0 then
    ShowMessage('not found');
end;

function TSODataSource.GetParamsJSON: String;
begin
  if Params<>Nil then
    Result := Params.AsJSon
  else
    Result := 'Nil';
end;

function TSODataSource.GetEnabled: Boolean;
begin
  Result := FDisableCount<=0;
end;

function TSODataSource.GetActive: Boolean;
begin
  Result := Data<>Nil;
end;

procedure TSODataSource.SetConnection(AValue: TSOConnection);
begin
  if FConnection=AValue then Exit;
  FConnection:=AValue;
  Data := Nil;
end;

procedure TSODataSource.Reset;
begin
  Data := Nil;
end;

procedure TSODataSource.SetActive(AValue: Boolean);
begin
  if AValue and (Data=Nil) then
    LoadDataset
  else
  if not AValue then
    Data := Nil;
end;

procedure TSODataSource.DeleteRecord(row: ISuperObject);
begin
  RemoveRecordFromData(Row);
  ChangeLog.AddChange(usDeleted,row);
  NotifyChange(deDeleteRecord,row);
end;

procedure TSODataSource.UpdateValue(row: ISuperObject; PropertyName: String;
  NewValue: ISuperObject);
var
  Newvalues:ISuperObject;
begin
  if (row<>Nil) and row.AsObject.Exists(PropertyName) and (row.AsObject[PropertyName] = NewValue) then
    Exit;
  NewValues := TSuperObject.Create;
  NewValues.AsObject[PropertyName] := Newvalue;
  UpdateRecord(row,Newvalues);
end;

procedure TSODataSource.UpdateRecord(row: ISuperObject;
  NewValues: ISuperObject);
var
  PropertyName,oldvalues,rowbefore:ISuperObject;
begin
  oldValues := TSuperObject.Create;
  for PropertyName in NewValues.AsObject.GetNames do
    oldvalues.AsObject[PropertyName.AsString] := row.AsObject[PropertyName.AsString];

  for PropertyName in NewValues.AsObject.GetNames do
    row.AsObject[PropertyName.AsString] := NewValues.AsObject[PropertyName.AsString];

  //opportunity to calc some fields
  if Assigned(OnUpdateRecord) then
  begin
    RowBefore := Row.Clone;
    OnUpdateRecord(deUpdateRecord,Row,oldvalues,NewValues);

    // add the additional modifications
    if row.Compare(rowbefore)<>cpEqu then
    begin
      // find updates
      for PropertyName in row.AsObject.GetNames do
      begin
        if row.AsObject[PropertyName.AsString].compare(rowbefore.AsObject[PropertyName.AsString])<>cpEqu then
        begin
          // add oldvalue if not already there
          if not oldvalues.AsObject.Exists(PropertyName.AsString) then
            oldvalues.AsObject[PropertyName.AsString] := rowbefore.AsObject[PropertyName.AsString];
          //add new value
          NewValues.AsObject[PropertyName.AsString] := row.AsObject[PropertyName.AsString];
        end;
      end;
    end;
  end;

  ChangeLog.AddChange(usModified,row,oldvalues,NewValues);
  NotifyChange(deUpdateRecord,Row,oldvalues,NewValues);
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

procedure TSODataSource.MergeChangeLog;
begin
  ChangeLog.Clear;
end;

function TSODataSource.ChangeCount: Integer;
begin
  Result := ChangeLog.Count;
end;

function TSODataSource.UndoSingle(change:ISORowChange;Notify:Boolean=True):ISuperObject;
var
  PropertyName,OldValue:ISuperObject;
begin
  Result := Nil;
  if change.UpdateType = usDeleted then
  begin
    Data.AsArray.Add(change.Row);
    if notify then
      NotifyChange(deDataSetChange);
    Result := change.Row;
  end
  else
  if change.UpdateType = usInserted then
  begin
    RemoveRecordFromData(change.Row);
    if notify then
      NotifyChange(deDataSetChange);
  end
  else
  if change.UpdateType = usModified then
  begin
    for PropertyName in change.OldValues.AsObject.GetNames do
    begin
      // the key was defined before
      if change.OldValues.AsObject.Exists(PropertyName.AsString) then
        change.Row[PropertyName.AsString] := change.OldValues[PropertyName.AsString]
      else
        // the key was not defined before
        change.Row.AsObject.Delete(PropertyName.AsString);
    end;
    if notify then
      NotifyChange(deDataSetChange);
    Result := change.Row;
  end;
end;

procedure TSODataSource.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  Inherited Notification(AComponent,Operation);
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
end;

function TSODataSource.UndolastChange: ISuperObject;
var
  lastchange:ISORowChange;
begin
  Result := Nil;
  if ChangeLog.Count>0 then
  try
    DisableControls;
    lastchange := ChangeLog.Items[ChangeLog.Count-1];
    result := UndoSingle(lastchange,False);
    ChangeLog.Delete(ChangeLog.Count-1);
  finally
    EnableControls;
  end;
end;

function TSODataSource.RevertRecord(row: ISuperObject):ISuperObject;
var
  i:integer;
  change:ISORowChange;
begin
  for i:=ChangeLog.count-1 downto 0 do
  try
    DisableControls;
    change := ChangeLog.Items[i];
    if change.Row.AsObject = row.AsObject then
    begin
      result := UndoSingle(change,false);
      ChangeLog.Delete(i);
    end;
  finally
    EnableControls;
  end;
end;

// return count of not applied records (remaining changes)
// datachanges keeps only the remaining (not applied changes)
function TSODataSource.ApplyUpdates: Integer;
var
  remaining:ISORowChanges;
begin
  if Assigned(Connection) then
    ChangeLog := Connection.ApplyUpdates(ProviderName,Params,ChangeLog.Flatten)
  else
    raise ESOUndefinedConnection.Create('Datasource is not connected to a connection component');
end;

procedure TSODataSource.Refresh;
begin
  LoadDataset;
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

function TSODataSource.GetKey(Row: ISuperObject): Variant;
begin
  result := Null;
  if Assigned(FOnGetKey) then
    FOnGetKey(Row,Result)
  else
  begin
    if not ObjectIsNull(Row['id']) then
      result := Row.I['id']
  end;
end;

function TSODataSource.FindKey(key: Variant): ISuperObject;
var
  rk : Variant;
begin
  result := Nil;
  if (Data=Nil) or VarIsNull(key) then
    Exit;

  for result in Self.Data do
  begin
    rk := GetKey(Result);
    if not VarIsNull(rk) and VarSameValue(rk,Key) then
      Exit;
  end;
  result := Nil;
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
            NewMenuItem.OnClick := @OnMenuItemClick;
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

procedure TSOGrid.SetSelectedRows(AValue: ISuperObject);
var
  ANodes:TNodeArray;
  ANode: PVirtualNode;
  ARec: ISuperObject;
begin
  If AValue = Nil then
  begin
    ClearSelection;
    FocusedNode:=Nil;
  end
  else
  begin
    ClearSelection;
    ANode := Nil;
    for ARec in AValue do
    begin
      ANodes := NodesForKey(ARec);
      for ANode in ANodes do
        Selected[ANode] := True;
    end;

    if (ANode <> Nil) then
    begin
      FocusedNode:=ANode;
      ScrollIntoView(ANode,False);
    end;
  end;
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

procedure TSOGrid.CreateColumnsFromData(FitWidth,AppendMissingAsHidden: Boolean);
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
          begin
            col :=Header.Columns.Add as TSOGridColumn;
            NewColStartIdx:=col.Index;
            col.Text:=propname.AsString;
            col.PropertyName:=propname.AsString;
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
  propname : String;

begin
  if (AValue <> nil) and (AValue.AsObject <> Nil)  then
  begin
    if AValue.AsObject.Find('columns', columns) then
    begin
      for column in Columns do
      begin
        propname := column.S['propertyname'] ;
        gridcol := FindColumnByPropertyName(propname);
        if gridcol = nil then
        begin
          gridcol := Header.Columns.Add as TSOGridColumn;
          gridcol.Text:=propname;
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
      Header.SortDirection := TSortDirection(prop.AsInteger);

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
    if (toEditable in TreeOptions.MiscOptions) then
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
    else
      inherited WMKeyDown(Message);
end;


procedure TSOGrid.LoadData;
var
  AFocused,ASelected: ISuperObject;
begin
  if (Data = nil) or (Data.AsArray = nil) then
    inherited Clear
  else
  begin
    //Stores previous focused and selected rows
    if Length(FKeyFieldsList) > 0 then
      ASelected := SelectedRows
    else
      ASelected := Nil;
    AFocused := FocusedRow;
    BeginUpdate;
    try
      inherited Clear;
      RootNodeCount := Data.AsArray.Length;
    finally
      EndUpdate;
      if (ASelected <>Nil) and (ASelected.AsArray.Length>0) then
        SelectedRows := ASelected;
      SetFocusedRowNoClearSelection(AFocused);
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
    FData := Value;
    LoadData;
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

procedure TSOGrid.SetFocusedColumnObject(AValue: TSOGridColumn);
begin
  if (AValue<>Nil) and Header.Columns.IsValidColumn(AValue.Index) then
    FocusedColumn:=AValue.Index;
end;

procedure TSOGrid.SetFocusedRow(AValue: ISuperObject);
var
  ANodes:TNodeArray;
begin
  ClearSelection;
  SetFocusedRowNoClearSelection(AValue);
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
  DefaultText := '';
  SetLength(FKeyFieldsList,0);

  FItemDataOffset := AllocateInternalDataArea(SizeOf(TSOItemData));

  WantTabs:=True;
  TabStop:=True;


  with TreeOptions do
  begin
    PaintOptions := PaintOptions - [toShowRoot] +
      [toAlwaysHideSelection, toShowHorzGridLines, toShowVertGridLines, toHideFocusRect];
    SelectionOptions := SelectionOptions + [toExtendedFocus, toSimpleDrawSelection,toRightClickSelect];
    MiscOptions := MiscOptions + [toEditable, toGridExtensions, toFullRowDrag] -
      [toWheelPanning];

    AutoOptions := AutoOptions + [toAutoSort];
  end;

  Header.Options := [hoColumnResize, hoDblClickResize, hoDrag,
    hoShowSortGlyphs, hoVisible,hoHeaderClickAutoSort];
  Header.Style := hsFlatButtons;
  Header.DefaultHeight:=18;
  Header.Height:=18;

  // Initialisation de la boite de dialogue de recherche
  FindDlg := TFindDialog.Create(nil);
  FindDlg.OnFind := @FindDlgFind;
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

      if Assigned(FDatasource) then
      begin
        if (HMUndo = 0) then
          HMUndo := AddItem(GSConst_UndoLastUpdate, ShortCut(Ord('Z'), [ssCtrl]),
            @DoUndoLastUpdate);
        if (HMRevert = 0) then
          HMRevert := AddItem(GSConst_RevertRecord, 0, @DoRevertRecord);
        AddItem('-', 0, nil);
      end;
      HMFind := AddItem(GSConst_Find, ShortCut(Ord('F'), [ssCtrl]), @DoFindText);
      HMFindNext := AddItem(GSConst_FindNext, VK_F3, @DoFindNext);
      {HMFindReplace := AddItem(GSConst_FindReplace, ShortCut(Ord('H'), [ssCtrl]),
        @DoFindReplace);}
      AddItem('-', 0, nil);
      if (toEditable in TreeOptions.MiscOptions) and Assigned(FOnCutToClipBoard) then
        HMCut := AddItem(GSConst_Cut, ShortCut(Ord('X'), [ssCtrl]), @DoCutToClipBoard);
      HMCopy := AddItem(GSConst_Copy, ShortCut(Ord('C'), [ssCtrl]), @DoCopyToClipBoard);
      HMCopyCell := AddItem(GSConst_CopyCell, ShortCut(Ord('C'), [ssCtrl,ssShift]), @DoCopyCellToClipBoard);
      if (toEditable in TreeOptions.MiscOptions) then
        HMPast := AddItem(GSConst_Paste, ShortCut(Ord('V'), [ssCtrl]), @DoPaste);
      AddItem('-', 0, nil);
      if (toEditable in TreeOptions.MiscOptions) then
        HMDelete := AddItem(GSConst_Delete, ShortCut(VK_DELETE, [ssCtrl]), @DoDeleteRows);
      if toMultiSelect in TreeOptions.SelectionOptions then
        HMSelAll := AddItem(GSConst_SelectAll, ShortCut(Ord('A'), [ssCtrl]), @DoSelectAllRows);
      AddItem('-', 0, nil);
      if (toMultiSelect in TreeOptions.SelectionOptions) then
        HMExcel := AddItem(GSConst_ExportSelectedExcel, 0, @DoExportExcel)
      else
        HMExcel := AddItem(GSConst_ExportAllExcel, 0, @DoExportExcel);
      {if (HMPrint = 0) then
        HMPrint := AddItem(GSConst_Print, ShortCut(Ord('P'), [ssCtrl]), @DoPrint);
      AddItem('-', 0, nil);
      HMExpAll := AddItem(GSConst_ExpandAll, Shortcut(Ord('E'), [ssCtrl, ssShift]),
        @DoExpandAll);
      HMCollAll := AddItem(GSConst_CollapseAll, Shortcut(Ord('R'), [ssCtrl, ssShift]),
        @DoCollapseAll);}
      AddItem('-', 0, nil);
      HMCustomize := AddItem(GSConst_CustomizeColumns, 0, @DoCustomizeColumns);
      if (csDesigning in ComponentState) or ShowAdvancedColumnsCustomize then
        HMAdvancedCustomize := AddItem(GSConst_AdvancedCustomizeColumns, 0, @DoAdvancedCustomizeColumns);
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

function TSOGrid.GetSelectedRows: ISuperObject;
var
  N: PVirtualNode;
begin
  N := GetFirstSelected;
  Result := TSuperObject.Create(stArray);
  while (N <> nil) do
  begin
    Result.AsArray.Add(GetNodeSOData(N));
    N := GetNextSelected(N);
  end;
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
    N := GetNextChecked(N);
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
  Result := 0;
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
            cpError :  Result := strcompare(n1.S[propname],n2.S[propname]);
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
  p := TopNode;
  while (p <> nil) do
  begin
    ASO := GetNodeSOData(p);
    if (ASO <> nil) and ((ASO = key) or (SOCompareByKeys(ASO,key,FKeyFieldsList)=cpEqu)) then
    begin
      SetLength(Result, length(Result) + 1);
      Result[length(Result) - 1] := p;
    end;
    p := GetNext(p);

  end;
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
  p := TopNode;
  while (p <> nil) do
  begin
    ASO := GetNodeSOData(p);
    if (ASO <> nil) and (ASO = sodata) then
    begin
      SetLength(Result, length(Result) + 1);
      Result[length(Result) - 1] := p;
      //OutputDebugString('kkk');
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

      st := cells.AsJSon(True);
      Clipboard.AddFormat(ClipbrdJson, st[1], Length(st));

    finally
      Clipboard.Close;
    end;
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
  if Dialogs.MessageDlg(GSConst_Confirmation, 'Confirmez-vous la suppression de ' +
    IntToStr(SelectedCount) + ' enregistrement(s) ?', mtConfirmation, mbYesNoCancel, 0) =
    mrYes then
  try
    if Assigned(Datasource) then
      Datasource.DisableControls;

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
        //grid connected to datasource
        if Assigned(Datasource) then
          Datasource.DeleteRecord(sel)
        else
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
  finally
    if Assigned(Datasource) then
      Datasource.EnableControls;
  end;
end;

procedure TSOGrid.DoPaste(Sender: TObject);
var
  row: ISuperObject;
begin
  row := Nil;
  if Data = Nil then
    Data := TSuperObject.Create(stArray);
  if Assigned(Datasource) then
    Datasource.Enabled:=false;
  try
    for row in ClipboardSOData do
      if Assigned(Datasource) then
        Datasource.AppendRecord(row)
      else
      begin
        Data.AsArray.Add(row);
        LoadData;
      end;
  finally
    if Assigned(Datasource) then
      Datasource.Enabled:=true;
    FocusedRow := row;
  end;
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
  if (CharCode = VK_DOWN) and (FocusedNode = GetLast) and (toEditable in TreeOptions.MiscOptions) and (FocusedRow<>FPendingAppendObject) then
  begin
    if FDatasource<>Nil then
    begin
      newdata := TSUperObject.Create;
      FPendingAppendObject := newdata;
      FDatasource.AppendRecord(newdata);
      ClearSelection;
      SetFocusedRow(NewData);
      Result := True;
    end;
  end
  else
  if (CharCode = VK_INSERT) and (toEditable in TreeOptions.MiscOptions) and (FocusedRow<>FPendingAppendObject) then
  begin
    Result := False;
    if FDatasource<>Nil then
    begin
      newdata := TSUperObject.Create;
      FPendingAppendObject := newdata;
      FDatasource.AppendRecord(newdata);
      //OutputDebugString(pchar('newdata:'+IntToHex(qword(pointer(FPendingAppendObject.AsObject)),32)));
      ClearSelection;
      SetFocusedRow(NewData);
    end
  end
  else
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
  Inherited Notification(AComponent,Operation);
  if (Operation = opRemove) and (AComponent = FDatasource) then
    FDatasource := nil;
end;

procedure TSOGrid.Clear;
begin
  inherited Clear;
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

  finally
    EndUpdate;
  end;
end;

procedure TSOGrid.NotifyChange(EventType:TSODataEvent;Row:ISuperObject;OldValues,NewValues:ISuperObject);
var
  oldkey:variant;
begin
  //deFieldChange, deDataSetChange,deUpdateRecord, deUpdateState,deFieldListChange
  if not (csDestroying in ComponentState) then
  begin
    if (EventType in [deUpdateRecord]) and (Row<>Nil) then
      InvalidateFordata(Row)
    else if EventType in [deUpdateState,deDataSetChange,deAddrecord,deDeleteRecord] then
    begin
      //try to retains focused row...
      if (focusedrow<>Nil) and (FDatasource <>Nil) then
        oldkey := FDatasource.GetKey(focusedrow)
      else
        oldkey := Null;
      LoadData;
      if Not VarIsNull(oldkey) and (FDatasource<>Nil) then
        focusedrow := FDatasource.FindKey(oldkey);
    end
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
  begin
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
  end
  else
  if (CellPaintMode = cpmPaint) and not (toMultiSelect in TreeOptions.SelectionOptions) and
     (Node = FocusedNode) then
  begin
    if (column <> FocusedColumn) then
    begin
      ACanvas.Brush.Color := clLtGray;
      ACanvas.FillRect(CellRect);
    end
    else
    begin
      ACanvas.Brush.Color := Colors.SelectionRectangleBlendColor;
      ACanvas.FillRect(CellRect);
    end;
  end;
  inherited;
end;

procedure TSOGrid.DoTextDrawing(var PaintInfo: TVTPaintInfo;
  const AText: string; CellRect: TRect; DrawFormat: cardinal);
begin
  //Pour affichage lignes multiselect en gris clair avec cellule focused en bleu
  if Focused and
    (vsSelected in PaintInfo.Node^.States) and (PaintInfo.Node = FocusedNode) and
    (PaintInfo.column = FocusedColumn) then
    PaintInfo.Canvas.Font.Color := clWhite;
  inherited;
end;

procedure TSOGrid.DoBeforeItemErase(ACanvas: TCanvas; Node: PVirtualNode;
  const ItemRect: TRect; var AColor: TColor; var EraseAction: TItemEraseAction);
begin
  inherited DoBeforeItemErase(Canvas, Node, ItemRect, AColor, EraseAction);
  if FZebraPaint and (Node<>Nil) and Odd(Node^.Index) then
  begin
      AColor := $00EDF0F1;
      EraseAction := eaColor;
  end;
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

function TSOGrid.GetKeyFieldsNames: String;
begin
  result := StrJoin(';',FKeyFieldsList);
end;

procedure TSOGrid.DoUndoLastUpdate(Sender: TObject);
var
  refocus:ISuperObject;
  nodesArr:TNodeArray;
begin
  if Assigned(FDatasource) then
  begin
    refocus := FDatasource.UndolastChange;
    if refocus<>Nil then
    begin
      nodesArr := NodesForData(refocus);
      if Length(nodesArr)>0 then
      begin
        ClearSelection;
        FocusedNode := nodesArr[0];
        Selected[FocusedNode] := True;
      end;
    end;
  end;
end;

procedure TSOGrid.DoRevertRecord(Sender: TObject);
var
  row:ISuperObject;
begin
  IF Assigned(FDatasource) then
    for row in SelectedRows do
      FDatasource.RevertRecord(row);
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

function TSOGrid.ContentAsCSV(Source: TVSTTextSourceType; const Separator: String):Utf8String;
var
  values,Row,Rows,value:ISuperObject;
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
        if (value<>Nil) and not ObjectIsNull((value)) then
        begin
          if values.DataType in [stInt,stDouble] then
            values.AsArray.Add(value.AsString)
          else
            values.AsArray.Add(UTF8Encode(AnsiQuotedStr(value.AsString,'"')))
        end
        else
          values.AsArray.Add('""');
      end;
    end;
    Result := Result+Join(Separator,values)+LineEnding;
  end;
end;

procedure TSOGrid.ExportExcel(Prefix:String='';Selection:TVSTTextSourceType=tstAll;Separator:Char=#9);
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
    txt := ContentAsCSV(Selection,Separator)+#0;
    txtbuf := pchar(txt);
    l := strlen(txtbuf);
    BlockWrite(st,txtbuf^,l);
  finally
    CloseFile(st);
    OpenDocument(tempfn);
  end;
end;

procedure TSOGrid.DoExportExcel(Sender: TObject);
begin
  if (toMultiSelect in TreeOptions.SelectionOptions) then
    ExportExcel(Name,tstSelected,#9)
  else
    ExportExcel(Name,tstAll,#9);
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
          if FDatasource<>Nil then
            FDatasource.UpdateValue(RowData,PropertyName,SO('"'+UTF8Decode(AText)+'"'))
          else
          //standalone grid
          begin
            OldCelldata := RowData[PropertyName];
            NewCellData := SO('"'+UTF8Decode(AText)+'"');
            RowData[PropertyName] := NewCellData;
          end
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
  Allowed:Boolean;

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
    if Assigned(TSOGrid(Tree).OnEditing) then
    begin
      Allowed:=(toEditable in TSOGrid(Tree).TreeOptions.MiscOptions);
      TSOGrid(Tree).OnEditing(Tree,Node,Column,Allowed);
      FEdit.ReadOnly:=not Allowed;
    end
    else
      FEdit.ReadOnly:= (toEditable in TSOGrid(Tree).TreeOptions.MiscOptions);

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
  AOffset: Integer;
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
      AOffset := 2;
      {if tsUseThemes in FTree.FStates then
        Inc(Offset);}
      InflateRect(R, -FTree.TextMargin + AOffset, AOffset);
      if not (vsMultiline in FNode^.States) then
        OffsetRect(R, 0, FTextBounds.Top - FEdit.Top);

      SendMessage(FEdit.Handle, EM_SETRECTNP, 0, PtrUInt(@R));
    end;
  end;
end;

end.
