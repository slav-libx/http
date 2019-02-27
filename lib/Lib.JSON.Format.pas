unit Lib.JSON.Format;

interface

uses
  System.SysUtils,
  System.JSON;

function ToJSON(jsValue: TJSONValue; const IndentValue: string=''): string;

implementation

function IsSimpleValue(jsValue: TJSONValue): Boolean;
begin
  Result:=jsValue is TJSONNumber;
end;

function IsSimpleArray(jsArray: TJSONArray): Boolean;
var jsItem: TJSONValue;
begin
  Result:=True;
  for jsItem in jsArray do
  if not IsSimpleValue(jsItem) then Exit(False);
end;

const
  CRLF=#13#10;
  CRLF2=CRLF+CRLF;
  INDENT='  ';

function ToJSON(jsValue: TJSONValue; const IndentValue: string=''): string;
var
  jsPair: TJSONPair;
  jsItem: TJSONValue;
begin

  if jsValue is TJSONObject then
  begin
    Result:='';
    for jsPair in TJSONObject(jsValue) do
      Result:=Result+IndentValue+INDENT+jsPair.JsonString.ToJSON+': '+
        ToJSON(jsPair.JsonValue,IndentValue+INDENT)+','+CRLF;
    Result:=Result.Remove(Result.Length-CRLF.Length-1);
    if Result.Length>0 then Result:=CRLF+Result+CRLF+IndentValue;
    Result:='{'+Result+'}';
  end else

  if jsValue is TJSONArray then
  begin
    Result:='';
    if IsSimpleArray(TJSONArray(jsValue)) then
    begin
      for jsItem in TJSONArray(jsValue) do
        Result:=Result+jsItem.ToJSON+',';
      Result:='['+Result.Remove(Result.Length-1)+']';
    end else begin
      for jsItem in TJSONArray(jsValue) do
        Result:=Result+IndentValue+INDENT+
          ToJSON(jsItem,IndentValue+INDENT)+','+CRLF;
      Result:=Result.Remove(Result.Length-CRLF.Length-1);
      if Result.Length>0 then Result:=CRLF+Result+CRLF;
      Result:='['+Result+IndentValue+']';
    end;

  end else

    Result:=jsValue.ToJSON;

end;

end.
