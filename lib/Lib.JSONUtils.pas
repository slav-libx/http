unit Lib.JSONUtils;

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.JSON;

function JSONValueToInt(AValue: TJSONValue; Default: Integer=0): Integer;
function JSONAddPairObject(jsObject: TJSONObject; const Name: string): TJSONObject;
procedure JSONSetPairValue(jsPair: TJSONPair; jsValue: TJSONValue);
procedure JSONClearPairValue(jsPair: TJSONPair);
function JSONRectToObject(const Rect: TRect): TJSONObject;
function JSONObjectToRect(jsRect: TJSONObject; const Default: TRect): TRect;
procedure JSONSetRect(jsRect: TJSONObject; const Rect: TRect);
function JSONStringsToArray(Strings: TStrings): TJSONArray;
function JSONForceObject(const Path: string; jsRoot: TJSONObject;
  Created: Boolean): TJSONObject;
function JSONTryGetObject(jsRoot: TJSONObject; const Name: string;
  out jsObject: TJSONObject; out PairName: string; Created: Boolean): Boolean;
procedure JSONSetValue(jsRoot: TJSONObject; const Name: string; jsValue: TJSONValue);

implementation

function JSONValueToInt(AValue: TJSONValue; Default: Integer=0): Integer;
begin
  Result:=Default;
  if Assigned(AValue) then Result:=AValue.GetValue<Integer>;
end;

function JSONAddPairObject(jsObject: TJSONObject; const Name: string): TJSONObject;
begin
  if not jsObject.TryGetValue(Name,Result) then
  begin
    Result:=TJSONObject.Create;
    jsObject.AddPair(Name,Result);
  end;
end;

{$IF CompilerVersion > 29.0} // > XE8

procedure JSONSetPairValue(jsPair: TJSONPair; jsValue: TJSONValue);
begin
  if Assigned(jsValue) then
    jsPair.JsonValue:=jsValue;
end;

{$ELSE}

procedure JSONSetPairValue(jsPair: TJSONPair; jsValue: TJSONValue);
begin
  if Assigned(jsValue) then
  begin
    jsPair.JsonValue.Free;
    jsPair.JsonValue:=jsValue;
  end;
end;

{$ENDIF}

procedure JSONClearPairValue(jsPair: TJSONPair);
begin
  if jsPair.JsonValue is TJSONObject then
    JSONSetPairValue(jsPair,TJSONObject.Create)
  else
  if jsPair.JsonValue is TJSONArray then
    JSONSetPairValue(jsPair,TJSONArray.Create);
end;

function JSONRectToObject(const Rect: TRect): TJSONObject;
begin
  Result:=TJSONObject.Create;
  Result.AddPair('left',TJSONNumber.Create(Rect.Left));
  Result.AddPair('top',TJSONNumber.Create(Rect.Top));
  Result.AddPair('right',TJSONNumber.Create(Rect.Right));
  Result.AddPair('bottom',TJSONNumber.Create(Rect.Bottom));
end;

function JSONObjectToRect(jsRect: TJSONObject; const Default: TRect): TRect;
begin
  Result:=Rect(
  JSONValueToInt(jsRect.Values['left'],Default.Left),
  JSONValueToInt(jsRect.Values['top'],Default.Top),
  JSONValueToInt(jsRect.Values['right'],Default.Right),
  JSONValueToInt(jsRect.Values['bottom'],Default.Bottom));
end;

procedure JSONSetRect(jsRect: TJSONObject; const Rect: TRect);
begin
  JSONSetValue(jsRect,'left',TJSONNumber.Create(Rect.Left));
  JSONSetValue(jsRect,'top',TJSONNumber.Create(Rect.Top));
  JSONSetValue(jsRect,'right',TJSONNumber.Create(Rect.Right));
  JSONSetValue(jsRect,'bottom',TJSONNumber.Create(Rect.Bottom));
end;

function JSONStringsToArray(Strings: TStrings): TJSONArray;
var S: string;
begin
  Result:=nil;
  for S in Strings do
  begin
    if not Assigned(Result) then Result:=TJSONArray.Create;
    Result.Add(S);
  end;
end;

function JSONForceObject(const Path: string; jsRoot: TJSONObject;
  Created: Boolean): TJSONObject;
var I: Integer;
begin
  if not jsRoot.TryGetValue(Path,Result) then
  if Created then
  begin
    I:=Path.IndexOf('.');
    if I=-1 then
      Result:=JSONAddPairObject(jsRoot,Path)
    else
      Result:=JSONForceObject(Path.Substring(I+1),JSONAddPairObject(jsRoot,Path.Substring(0,I)),True);
  end else
    Result:=nil;
end;

function JSONTryGetObject(jsRoot: TJSONObject; const Name: string;
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
    jsObject:=JSONForceObject(Name.Substring(0,I),jsRoot,Created);
    PairName:=Name.Substring(I+1);
    Result:=Assigned(jsObject);
  end;
end;

function JSONTryGetPair(jsObject: TJSONObject; const PairName: string; out jsObjectPair: TJSONPair): Boolean;
var jsPair: TJSONPair;
begin
  Result:=False;
  for jsPair in jsObject do
  if jsPair.JsonString.Value=PairName then
  begin
    jsObjectPair:=jsPair;
    Exit(True);
  end;
end;

procedure JSONSetValue(jsRoot: TJSONObject; const Name: string; jsValue: TJSONValue);
var jsObject: TJSONObject; PairName: string; jsPair: TJSONPair;
begin
  if JSONTryGetObject(jsRoot,Name,jsObject,PairName,Assigned(jsValue)) then
  if JSONTryGetPair(jsObject,PairName,jsPair) then
    if Assigned(jsValue) then
      JSONSetPairValue(jsPair,jsValue)
    else
      JSONClearPairValue(jsPair)
  else
    if Assigned(jsValue) then
      jsObject.AddPair(PairName,jsValue);
end;

end.
