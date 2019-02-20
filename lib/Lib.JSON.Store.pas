unit Lib.JSON.Store;

interface

uses
  System.Types,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  System.JSON,
  Lib.JSON.Format;

type
  TJSONStore = class
  private
    FOwned: Boolean;
    FObject: TJSONObject;
    FStoreFileName: string;
    procedure CreateStore;
    procedure FreeStore;
    function ForceObject(const Path: string; jsRoot: TJSONObject; Created: Boolean): TJSONObject;
    function TryGetObject(jsRoot: TJSONObject; const Name: string; out jsObject: TJSONObject; out PairName: string; Created: Boolean): Boolean;
    procedure SetValue(jsRoot: TJSONObject; const Name: string; jsValue: TJSONValue);
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

function ValueToInt(AValue: TJSONValue; Default: Integer=0): Integer;
begin
  Result:=Default;
  if Assigned(AValue) then Result:=AValue.GetValue<Integer>;
end;

function RectToJSON(const Rect: TRect): TJSONObject;
begin
  Result:=TJSONObject.Create;
  Result.AddPair('left',TJSONNumber.Create(Rect.Left));
  Result.AddPair('top',TJSONNumber.Create(Rect.Top));
  Result.AddPair('right',TJSONNumber.Create(Rect.Right));
  Result.AddPair('bottom',TJSONNumber.Create(Rect.Bottom));
end;

function JSONToRect(jsRect: TJSONObject; const Default: TRect): TRect;
begin
  Result:=Rect(
  ValueToInt(jsRect.Values['left'],Default.Left),
  ValueToInt(jsRect.Values['top'],Default.Top),
  ValueToInt(jsRect.Values['right'],Default.Right),
  ValueToInt(jsRect.Values['bottom'],Default.Bottom));
end;

constructor TJSONStore.Create(const StoreFileName: string);
begin
  FOwned:=False;
  FObject:=nil;
  FStoreFileName:=StoreFileName;
end;

constructor TJSONStore.Create(jsRoot: TJSONObject);
begin
  FOwned:=True;
  FObject:=jsRoot;
end;

destructor TJSONStore.Destroy;
begin
  FreeStore;
end;

procedure TJSONStore.CreateStore;
begin
  if not Assigned(FObject) then
  begin
    if FileExists(FStoreFileName) then
      FObject:=TJSONObject.ParseJSONValue(TFile.ReadAllText(FStoreFileName)) as TJSONObject;
    if not Assigned(FObject) then
      FObject:=TJSONObject.Create;
  end;
end;

procedure TJSONStore.Flush;
begin
  if Assigned(FObject) and not FOwned then
    TFile.WriteAllText(FStoreFileName,ToJSON(FObject),TEncoding.ANSI);
  //TFile.WriteAllText(FStoreFileName,FObject.ToJSON,TEncoding.ANSI);
end;

procedure TJSONStore.FreeStore;
begin
  Flush;
  if not FOwned then FreeAndNil(FObject);
end;

function AddPair(jsObject: TJSONObject; const Name: string): TJSONObject;
begin
  if not jsObject.TryGetValue(Name,Result) then
  begin
    Result:=TJSONObject.Create;
    jsObject.AddPair(Name,Result);
  end;
end;

function TJSONStore.ForceObject(const Path: string; jsRoot: TJSONObject;
  Created: Boolean): TJSONObject;
var I: Integer;
begin
  if not jsRoot.TryGetValue(Path,Result) then
  if Created then
  begin
    I:=Path.IndexOf('.');
    if I=-1 then
      Result:=AddPair(jsRoot,Path)
    else
      Result:=ForceObject(Path.Substring(I+1),AddPair(jsRoot,Path.Substring(0,I)),True);
  end else
    Result:=nil;
end;

function TJSONStore.TryGetObject(jsRoot: TJSONObject; const Name: string;
  out jsObject: TJSONObject; out PairName: string; Created: Boolean): Boolean;
var I: Integer;
begin
  I:=Name.LastIndexOf('.');
  if I=-1 then
  begin
    jsObject:=jsRoot;
    PairName:=Name;
    Result:=True;
  end else begin
    jsObject:=ForceObject(Name.Substring(0,I),jsRoot,Created);
    PairName:=Name.Substring(I+1);
    Result:=Assigned(jsObject);
  end;
end;

procedure TJSONStore.SetValue(jsRoot: TJSONObject; const Name: string; jsValue: TJSONValue);
var jsObject: TJSONObject; PairName: string; jsPair: TJSONPair;
begin
  if TryGetObject(jsRoot,Name,jsObject,PairName,True) then
  begin
    for jsPair in jsObject do
    if jsPair.JsonString.Value=PairName then
    begin
      jsPair.JsonValue:=jsValue;
      Exit;
    end;
    jsObject.AddPair(PairName,jsValue);
  end;
end;

function TJSONStore.GetValue<T>(jsRoot: TJSONObject; const Name: string; out Value: T): Boolean;
var jsObject: TJSONObject; PairName: string;
begin
  Result:=
    TryGetObject(jsRoot,Name,jsObject,PairName,False) and
    jsObject.TryGetValue(PairName,Value);
end;

procedure TJSONStore.Remove(const Name: string);
var jsObject: TJSONObject; PairName: string;
begin
  CreateStore;
  if TryGetObject(FObject,Name,jsObject,PairName,False) then
    jsObject.RemovePair(PairName).Free;
end;

procedure TJSONStore.WriteString(const Name: string; const Value: string);
begin
  CreateStore;
  SetValue(FObject,Name,TJSONString.Create(Value));
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
  SetValue(FObject,Name,TJSONNumber.Create(Value));
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
  SetValue(FObject,Name,TJSONNumber.Create(Value));
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
var jsArray: TJSONArray; S: string;
begin
  CreateStore;
  jsArray:=TJSONArray.Create;
  for S in Strings do jsArray.Add(S);
  SetValue(FObject,Name,jsArray);
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
  if TryGetObject(FObject,Name+'.',jsObject,PairName,True) then
  begin
    SetValue(jsObject,'left',TJSONNumber.Create(Rect.Left));
    SetValue(jsObject,'top',TJSONNumber.Create(Rect.Top));
    SetValue(jsObject,'right',TJSONNumber.Create(Rect.Right));
    SetValue(jsObject,'bottom',TJSONNumber.Create(Rect.Bottom));
  end else
    SetValue(FObject,Name,RectToJSON(Rect));
end;

function TJSONStore.ReadRect(const Name: string; const DefaultRect: TRect): TRect;
var jsRect: TJSONObject;
begin
  CreateStore;
  Result:=DefaultRect;
  if GetValue(FObject,Name,jsRect) then Result:=JSONToRect(jsRect,Result);
end;

procedure TJSONStore.WriteJSON(const Name: string; jsValue: TJSONValue);
begin
  CreateStore;
  SetValue(FObject,Name,jsValue);
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
