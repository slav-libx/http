unit Lib.JSONFormat;

interface

uses
  System.SysUtils,
  System.JSON;

function ToJSON(jsValue: TJSONValue; ValuesJSONFormat: Boolean): string;

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

function ToString(jsValue: TJSONValue; ValuesJSONFormat: Boolean): string;
begin
  if ValuesJSONFormat then
    Result:=jsValue.ToJSON
  else
    Result:=jsValue.ToString;
end;

const
  CRLF=#13#10;
  CRLF2=CRLF+CRLF;
  INDENT='  ';

function ValueToJSON(jsValue: TJSONValue; const IndentValue: string; ValuesJSONFormat: Boolean): string;
var
  jsPair: TJSONPair;
  jsItem: TJSONValue;
begin

  if jsValue is TJSONObject then
  begin
    Result:='';
    for jsPair in TJSONObject(jsValue) do
      Result:=Result+IndentValue+INDENT+ToString(jsPair.JsonString,ValuesJSONFormat)+': '+
        ValueToJSON(jsPair.JsonValue,IndentValue+INDENT,ValuesJSONFormat)+','+CRLF;
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
        Result:=Result+ToString(jsItem,ValuesJSONFormat)+',';
      Result:='['+Result.Remove(Result.Length-1)+']';
    end else begin
      for jsItem in TJSONArray(jsValue) do
        Result:=Result+IndentValue+INDENT+
          ValueToJSON(jsItem,IndentValue+INDENT,ValuesJSONFormat)+','+CRLF;
      Result:=Result.Remove(Result.Length-CRLF.Length-1);
      if Result.Length>0 then Result:=CRLF+Result+CRLF;
      Result:='['+Result+IndentValue+']';
    end;

  end else

    Result:=ToString(jsValue,ValuesJSONFormat);

end;

function ToJSON(jsValue: TJSONValue; ValuesJSONFormat: Boolean): string;
begin
  Result:=ValueToJSON(jsValue,'',ValuesJSONFormat);
end;

end.
