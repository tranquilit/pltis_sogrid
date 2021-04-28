{**********************************************************************
 Package pltis_sogrid.pkg
 This unit is based on package virtualtreesextra work from CodeTyphon Studio  (http://www.pilotlogic.com/)
***********************************************************************}

unit sogridsource;
{$mode objfpc}{$H+}

interface

uses
  {$IFDEF windows}
  Windows,
  {$ENDIF}
  Classes, SysUtils,
  LCLType, Types, DefaultTranslator,
  SuperObject, sogridcommon;

type
  { TSOConnection }
  TSOConnection = class(TComponent)
  private
    //FIdHttpClient: TIdHTTP;
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
    //property IdHttpClient:TIdHTTP read FIdHttpClient;

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
    function Get(i : Integer) : ISORowChange; overload;
    procedure Put(i : Integer;item : ISORowChange); overload;

    function GetDataSource: TSODataSource;

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

  TSODataSource = class;



implementation

uses Clipbrd, soutils, base64, IniFiles, LCLIntf, variants, tisstrings;

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
  //FIdHttpClient := TIdHTTP.Create(Self);
  //FIdHttpClient.HandleRedirects:=True;
  inherited;
end;

destructor TSOConnection.Destroy;
begin
  //FIdHttpClient.Free;
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
  {
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
        paramsStr := UTF8Encode(key.AsString)+'='+EncodeURIComponent(UTF8Encode(KWArgs.S[key.AsString]));
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
  end;}
  Raise ENotImplemented.Create('TSOConnection.CallServerMethod');

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
  res,par:String;
  pathargs:TStringArray;
  response:ISuperObject;
begin
  pathargs := StrSplit(provider,'/');
  //pathargs[length(pathargs)-1] := pathargs[length(pathargs)-1]+'.json';
  res := CallServerMethod('GET',pathargs,Params);
  response := SO(res);
  if response=Nil then
  begin
    if Params<>Nil then
      par := UTF8Encode(Params.AsJSon)
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
        par := UTF8Encode(Params.AsJSon)
      else
        par := '""';
      Raise ESONoDataReturned.Create('GET method on server '+ServerURL+'/'+provider+' with params '+Par+' returned bad data : '+copy(res,1,1000))
  end
end;

function TSOConnection.Refresh(provider: String; Params: ISuperObject
  ): ISuperObject;
begin
  Result := Nil;
  raise ENotImplemented.Create('TSOCOnnection.Refresh not implemented');
end;

function TSOConnection.ApplyUpdates(provider: String; Params: ISuperObject;
  Delta: ISORowChanges): ISORowChanges;
var
  change:ISORowChange;
  JSonResult:String;
  SOResult:ISuperObject;
begin
{  result := TSORowChanges.Create(Delta.Datasource);
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
}
  Raise ENotImplemented.Create('TSOConnection.ApplyUpdates');
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
  newChange.Key := Null;
  newChange.OldValues := OldValues;
  newChange.NewValues := NewValues;
  result := Add(newChange);
end;

function SOCompareSeq(so1,so2:Pointer):integer;
var
   seq1,seq2:Int64;
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
  rowchange : ISORowChange;
  rowdelta: ISuperObject;
  key:Variant;

begin
  result := TSuperObject.Create(stArray);
  for rowchange in Self do
  begin
    rowdelta := TSuperObject.Create;
    rowdelta.I['update_type'] := Integer(rowchange.UpdateType);
    rowdelta['old'] := rowchange.OldValues;
    rowdelta['new'] := rowchange.NewValues;

    key := Null;
    if (rowchange.UpdateType=usModified) and (rowchange.OldValues<>Nil) then
      key := Fdatasource.GetKey(rowchange.OldValues);
    if VarIsNull(Key) then
      key := Fdatasource.GetKey(rowchange.Row);
    if not VarIsNull(key) then
      rowdelta.AsObject['id'] := SO(key);
    Result.AsArray.Add(rowdelta);
  end;
end;

function TSORowChanges.Get(i: Integer): ISORowChange;
begin
  result := inherited Get(i) as ISORowChange;
end;

procedure TSORowChanges.Put(i: Integer; item: ISORowChange);
begin
  inherited Put(i,item);
end;


function TSORowChanges.GetDataSource: TSODataSource;
begin
  Result := Fdatasource;
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
  PropertyName:ISuperObject;
  change,rowchange : ISORowChange;
  key:Variant;

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
  if (VarIsNull(Result) or VarIsEmpty(Result)) and Assigned(OldValues) and OldValues.AsObject.Exists('id') then
    Result := OldValues.I['id'];
  if (VarIsNull(Result) or VarIsEmpty(Result)) and Assigned(NewValues) and NewValues.AsObject.Exists('id') then
    Result := NewValues.I['id'];
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
  if new<>Nil then
    Result := new
  else
    result := TSuperObject.Create;
  if Assigned(OnNewRecord) then
    OnNewRecord(result);
  Data.AsArray.Add(Result);
  if Assigned(OnUpdateRecord) then
    OnUpdateRecord(deAddrecord,Result,Nil,Nil);
  ChangeLog.AddChange(usInserted,Result);
  NotifyChange(deAddrecord,Result);
end;

procedure TSODataSource.AppendRecords(records: ISuperObject);
var
  ARecord:ISuperObject;
begin
  for ARecord in records do
    AppendRecord(ARecord);
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
    raise Exception.CreateFmt('Internal error. Record to remove not found in current datasource %s',[Name]);
end;

function TSODataSource.GetParamsJSON: String;
begin
  if Params<>Nil then
    Result := Utf8Encode(Params.AsJSon)
  else
    Result := 'Nil';
end;

function TSODataSource.GetEnabled: Boolean;
begin
  Result := FDisableCount<=0;
end;

function TSODataSource.GetActive: Boolean;
begin
  Result := FData <> Nil;
end;

function TSODataSource.GetData: ISuperObject;
begin
  if not Assigned(FData) then
    FData := TSuperObject.Create(stArray);
  Result := FData;
end;

procedure TSODataSource.SetConnection(AValue: TSOConnection);
begin
  if FConnection=AValue then Exit;
  FConnection:=AValue;
  Reset;
end;

procedure TSODataSource.Reset;
begin
  FData := Nil;
  FChangeLog := TSORowChanges.Create(Self);
  NotifyChange(deDataSetChange,Nil);
end;

procedure TSODataSource.SetActive(AValue: Boolean);
begin
  if AValue then
  begin
    if (FData=Nil) then
      LoadDataset;
  end
  else
    Reset;
end;

procedure TSODataSource.DeleteRecord(row: ISuperObject);
begin
  RemoveRecordFromData(Row);
  ChangeLog.AddChange(usDeleted,row);
  NotifyChange(deDeleteRecord,row);
end;

procedure TSODataSource.DeleteRecords(rows: ISuperObject);
begin

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

function TSODataSource.GetKeyProperty:String;
begin
  Result := 'id';
end;

procedure TSODataSource.UpdateRecord(row: ISuperObject;
  NewValues: ISuperObject);
var
  PropertyName,oldvalues,rowbefore:ISuperObject;
  SPropertyName:String;
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
        if (row.AsObject[PropertyName.AsString].compare(rowbefore.AsObject[PropertyName.AsString])<>cpEqu) or (PropertyName.AsString=GetKeyProperty) then
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
  Data := TSuperObject.ParseFile(Filename,False);
  NotifyChange(deDataSetChange);
end;

procedure TSODataSource.SaveToFile(Filename: String);
begin
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
  PropertyName:ISuperObject;
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

function TSODataSource.BuildDeltaPacket: ISuperObject;
var
  seq: Integer;
  Change: ISORowChange;
  SOChange, k,ov,nv: ISuperObject;
  key: Variant;
begin
  seq := 0;
  Result := TSuperObject.Create(stArray);
  //for Change in ChangeLog.Flatten do
  for Change in ChangeLog do
  begin
    SOChange := TSuperObject.Create(stObject);
    Result.AsArray.Add(SOChange);
    SOChange.I['seq'] := seq;
    SOChange.I['update_type'] := integer(Change.UpdateType);
    if (Change.UpdateType in [usModified,usInserted]) then
    begin
      if Change.UpdateType=usModified then
        SOChange['old_values'] := Change.OldValues;
      SOChange['new_values'] := Change.NewValues;
    end;

    key := Null;
    if (Change.UpdateType=usModified) and (Change.OldValues<>Nil) then
      key := GetKey(Change.OldValues);
    if VarIsNull(Key) then
      key := GetKey(Change.Row);
    if not VarIsNull(key) then
      SOChange.AsObject.I['id'] := key;
    inc(seq);
  end;
end;

// return count of not applied records (remaining changes)
// datachanges keeps only the remaining (not applied changes)
function TSODataSource.ApplyUpdates: Integer;
var
  ReturnStr: String;
  ReturnPacket,DeltaPacket: ISuperObject;
begin
  Result := 0;
  if Assigned(Connection) then
  begin
    DeltaPacket := BuildDeltaPacket;
    //DeltaPacket := ChangeLog.Flatten.Delta;
    ReturnStr := Connection.CallServerMethod('POST',[ProviderName],Nil,DeltaPacket);
    ReturnPacket := SO(ReturnStr);
    //ChangeLog := Connection.ApplyUpdates(ProviderName,Params,DeltaPacket)
  end
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
    if not Row.AsObject.Exists(GetKeyProperty()) then
      result := Row[GetKeyProperty()]
    else
      Raise Exception.create('No key');
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



end.
