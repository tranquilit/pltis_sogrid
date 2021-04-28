unit sogridcommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, superobject;

type
  ESOUnknownKey = class(Exception);
  ESOUndefinedDatasource = class(Exception);
  ESOUndefinedConnection = class(Exception);
  ESONoDataReturned = class(Exception);

  TSODataEvent = (deDataSetChange,
    deAddrecord,deDeleteRecord,deUpdateRecord, deUpdateState,deFieldListChange);

  TSODataChangeEvent = procedure (EventType:TSODataEvent;Row:ISuperObject;OldValues,NewValues:ISuperObject) of object;
  TSuperObjectRowEvent = procedure (ARow:ISuperObject) of object;

  TSOUpdateStatus = (usUnmodified, usModified, usInserted, usDeleted);
  TSOUpdateStatusSet = SET OF TSOUpdateStatus;

  TSOUpdateMode = (upWhereAll, upWhereChanged, upWhereKeyOnly);
  TSOResolverResponse = (rrSkip, rrAbort, rrMerge, rrApply, rrIgnore);

  TSOGetKeyEvent = procedure (ARow:ISuperObject;var key:Variant) of object;

  ISODataView = interface
    ['{2DF865FF-684D-453E-A9F0-7D7307DD0BDD}']
    procedure NotifyChange(EventType:TSODataEvent;Row:ISuperObject;OldValues,NewValues:ISuperObject);
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

  { ISORowChanges }
  ISORowChanges = interface(IInterfaceList)
    ['{310B128D-2751-4860-BC15-2F1AA1A0B2B7}']
    function Get(i : Integer) : ISORowChange;
    procedure Put(i : Integer;item : ISORowChange);

    //overriden to return proper interface
    property Items[Index : Integer]: ISORowChange read Get write Put;default;
    //Add a new change to the list
    function AddChange(UpdateType:TSOUpdateStatus;Row:ISuperObject=nil;OldValues:ISuperObject=Nil;NewValues:ISuperObject=Nil):integer;
    // Remove all changes related to one row
    procedure RemoveRowChanges(row:ISuperObject);
    // return list of changes as a JSon object
    function Flatten:ISORowChanges;
    // Create a new list of changes grouping all changes of each row into one delta
    function Delta:ISuperObject;
    // return True if AChange is the first one for the specified row
    function IsFirstChange(AChange:ISORowChange):Boolean;
  end;

  { ISODataSource }

  // TODO : this should be translated to a wrapper around mormot2 TDocVariant
  //

  ISODataSource = interface
  ['{C3A0ECAE-BFDA-4468-8224-95C0111B34EB}']
    function GetChangeLog: ISORowChanges;
    function GetData: ISuperObject;
    function GetEnabled: Boolean;
    procedure SetChangeLog(AValue: ISORowChanges);
    procedure SetData(AData: ISuperObject);

    // add a component to the list of notified objects
    procedure RegisterView(AComponent:TComponent);
    procedure SetEnabled(AValue: Boolean);
    procedure UnregisterView(AComponent:TComponent);
    procedure NotifyChange(EventType:TSODataEvent;Row:ISuperObject=Nil;OldValues:ISuperObject=Nil;NewValues:ISuperObject=Nil);

    procedure LoadDataset;
    procedure Emptydataset;

    // if attached views are notified from changes through OnDataChange
    procedure EnableControls;
    procedure DisableControls;

    //Append a new row to the collection. update views after initialisation
    function  AppendRecord(new:ISuperObject=Nil):ISuperObject;
    procedure AppendRecords(records:ISuperObject);

    //Delete a row from the collection
    procedure DeleteRecord(row:ISuperObject);
    procedure DeleteRecords(rows:ISuperObject);

    //update a field of a record log changes and notify views
    procedure UpdateValue(row:ISuperObject;PropertyName:String;NewValue:ISuperObject);
    //Update multiple values of a record, log changes and notify views
    procedure UpdateRecord(row:ISuperObject;NewValues:ISuperObject);

    //load data as json from a file
    procedure LoadFromFile(Filename:String);
    //save data as json in a file
    procedure SaveToFile(Filename:String);

    // empty the log of changes : applyupdates is no more possible
    procedure MergeChangeLog;
    // count of single changes
    function ChangeCount:Integer;

    // undo the last single change
    function UndolastChange:ISuperObject;
    // revert row record to initial state, reverting all changes logged in datachanges
    function RevertRecord(row:ISuperObject):ISuperObject;

    //send the delta to the datasource and remove from change log the applied updates
    function ApplyUpdates:Integer;

    //reload data from server keeping initial filtering/parameters
    procedure Refresh;

    //paste json data or csv data from the clipboard
    procedure PasteFromClipboard;

    //get a unique key for the row
    function GetKey(Row:ISuperObject):Variant;
    function GetKeyProperty:String;

    //returns the row associated with a key
    function FindKey(key:Variant):ISuperObject;

  // Properties
    // Data store : current state of records
    property Data:ISuperObject read GetData write SetData;

    // log of all changes for Applyupdates, UndoLastChange etc...:
    property ChangeLog:ISORowChanges read GetChangeLog write SetChangeLog;

  end;


implementation

end.

