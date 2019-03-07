unit Lib.JSONStore;

interface

uses
  System.Types,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  System.JSON,
  Lib.JSONUtils,
  Lib.JSONFormat;

type
  TJSONStore = class
  private type
    TStoreSource = (None,Owned,NewFile,ExistingFile,BadFile);
  private
    FStoreSource: TStoreSource;
    FObject: TJSONObject;
    FStoreFileName: string;
    procedure CreateStore;
    function GetValue<T>(jsRoot: TJSONObject; const Name: string; out Value: T): Boolean;
  public
    procedure WriteString(const Name: string; const Value: string);
    function ReadString(const Name: string; const DefaultValue: string=''): string;
    procedure WriteInteger(const Name: string; Value: Integer);
    function ReadInteger(const Name: string; DefaultValue: Integer=0): Integer;
    procedure WriteDouble(const Name: string; Value: Double);
    function ReadDouble(const Name: string; DefaultValue: Double=0): Double;
    procedure WriteBool(const Name: string; Value: Boolean);
    function ReadBool(const Name: string; DefaultValue: Boolean=False): Boolean;
    procedure WriteStrings(const Name: string; Strings: TStrings);
    procedure ReadStrings(const Name: string; Strings: TStrings);
    procedure WriteRect(const Name: string; const Rect: TRect);
    function ReadRect(const Name: string; const DefaultRect: TRect): TRect;
    procedure WriteJSON(const Name: string; jsValue: TJSONValue);
    function ReadJSON(const Name: string): TJSONValue;
    procedure Remove(const Name: string);
    procedure Flush;
    constructor Create(const StoreFileName: string); overload;
    constructor Create(jsRoot: TJSONObject); overload;
    destructor Destroy; override;
  end;

implementation

constructor TJSONStore.Create(const StoreFileName: string);
begin
  FStoreSource:=TStoreSource.None;
  FObject:=nil;
  FStoreFileName:=StoreFileName;
end;

constructor TJSONStore.Create(jsRoot: TJSONObject);
begin
  FStoreSource:=TStoreSource.Owned;
  FObject:=jsRoot;
end;

destructor TJSONStore.Destroy;
begin
  Flush;
  if FStoreSource<>TStoreSource.Owned then
    FreeAndNil(FObject);
end;

procedure TJSONStore.CreateStore;
begin

  if FStoreSource=TStoreSource.None then
  begin

    if FileExists(FStoreFileName) then
    begin

      FObject:=TJSONObject.ParseJSONValue(TFile.ReadAllText(FStoreFileName)) as TJSONObject;

      if Assigned(FObject) then
        FStoreSource:=TStoreSource.ExistingFile
      else
        FStoreSource:=TStoreSource.BadFile;

    end else
      FStoreSource:=TStoreSource.NewFile;

    if not Assigned(FObject) then
      FObject:=TJSONObject.Create;

    if FStoreSource=TStoreSource.BadFile then
      raise EJSONException.Create('json format error (fix or delete the file): '+FStoreFileName);

  end;

end;

procedure TJSONStore.Flush;
begin
  if FStoreSource in [TStoreSource.NewFile,TStoreSource.ExistingFile] then
    TFile.WriteAllText(FStoreFileName,ToJSON(FObject,True),TEncoding.ANSI);
end;

function TJSONStore.GetValue<T>(jsRoot: TJSONObject; const Name: string; out Value: T): Boolean;
var jsObject: TJSONObject; PairName: string;
begin
  Result:=
    JSONTryGetObject(jsRoot,Name,jsObject,PairName,False) and
    jsObject.TryGetValue(PairName,Value);
end;

procedure TJSONStore.Remove(const Name: string);
var jsObject: TJSONObject; PairName: string;
begin
  CreateStore;
  if JSONTryGetObject(FObject,Name,jsObject,PairName,False) then
    jsObject.RemovePair(PairName).Free;
end;

procedure TJSONStore.WriteString(const Name: string; const Value: string);
begin
  CreateStore;
  JSONSetValue(FObject,Name,TJSONString.Create(Value));
end;

function TJSONStore.ReadString(const Name: string; const DefaultValue: string=''): string;
var jsValue: TJSONValue;
begin
  CreateStore;
  Result:=DefaultValue;
  if GetValue(FObject,Name,jsValue) then
    Result:=jsValue.GetValue<string>;
end;

procedure TJSONStore.WriteInteger(const Name: string; Value: Integer);
begin
  CreateStore;
  JSONSetValue(FObject,Name,TJSONNumber.Create(Value));
end;

function TJSONStore.ReadInteger(const Name: string; DefaultValue: Integer=0): Integer;
var jsValue: TJSONValue;
begin
  CreateStore;
  Result:=DefaultValue;
  if GetValue(FObject,Name,jsValue) then
    Result:=jsValue.GetValue<Integer>;
end;

procedure TJSONStore.WriteDouble(const Name: string; Value: Double);
begin
  CreateStore;
  JSONSetValue(FObject,Name,TJSONNumber.Create(Value));
end;

function TJSONStore.ReadDouble(const Name: string; DefaultValue: Double=0): Double;
var jsValue: TJSONValue;
begin
  CreateStore;
  Result:=DefaultValue;
  if GetValue(FObject,Name,jsValue) then
    Result:=jsValue.GetValue<Double>;
end;

procedure TJSONStore.WriteBool(const Name: string; Value: Boolean);
const V:array[Boolean] of Integer=(0,1);
begin
  WriteInteger(Name,V[Value]);
end;

function TJSONStore.ReadBool(const Name: string; DefaultValue: Boolean=False): Boolean;
const V:array[Boolean] of Integer=(0,1);
begin
  Result:=ReadInteger(Name,V[DefaultValue])=1;
end;

procedure TJSONStore.WriteStrings(const Name: string; Strings: TStrings);
begin
  CreateStore;
  JSONSetValue(FObject,Name,JSONStringsToArray(Strings));
end;

procedure TJSONStore.ReadStrings(const Name: string; Strings: TStrings);
var jsArray: TJSONArray; jsValue: TJSONValue;
begin
  CreateStore;
  if GetValue(FObject,Name,jsArray) then
  for jsValue in jsArray do Strings.Add(jsValue.GetValue<string>);
end;

procedure TJSONStore.WriteRect(const Name: string; const Rect: TRect);
var jsObject: TJSONObject; PairName: string; jsPair: TJSONPair;
begin
  CreateStore;
  if JSONTryGetObject(FObject,Name+'.',jsObject,PairName,True) then
    JSONSetRect(jsObject,Rect)
  else
    JSONSetValue(FObject,Name,JSONRectToObject(Rect));
end;

function TJSONStore.ReadRect(const Name: string; const DefaultRect: TRect): TRect;
var jsRect: TJSONObject;
begin
  CreateStore;
  Result:=DefaultRect;
  if GetValue(FObject,Name,jsRect) then Result:=JSONObjectToRect(jsRect,Result);
end;

procedure TJSONStore.WriteJSON(const Name: string; jsValue: TJSONValue);
begin
  CreateStore;
  JSONSetValue(FObject,Name,jsValue);
end;

function TJSONStore.ReadJSON(const Name: string): TJSONValue;
var jsValue: TJSONValue;
begin
  CreateStore;
  Result:=nil;
  if GetValue(FObject,Name,jsValue) then
    Result:=jsValue.GetValue<TJSONValue>;
end;

end.
