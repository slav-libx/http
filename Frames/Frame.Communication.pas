unit Frame.Communication;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  System.JSON,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Lib.JSONFormat,
  Lib.VCL.HTTPGraphic,
  Lib.HTTPConsts,
  Lib.HTTPUtils,
  Lib.HTTPContent;

type
  TCommunicationFrame = class(TFrame)
    TabPanel: TPanel;
    ResponseTab: TSpeedButton;
    LogTab: TSpeedButton;
    CodeLabel: TLabel;
    RequestTab: TSpeedButton;
    ClearButton: TButton;
    LogMemo: TMemo;
    RequestMemo: TMemo;
    ResponseMemo: TMemo;
    ContentImage: TImage;
    PictureScrollBox: TScrollBox;
    procedure LogTabClick(Sender: TObject);
    procedure RequestTabClick(Sender: TObject);
    procedure ResponseTabClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
  private
    FAutoShowResponseContent: Boolean;
    procedure ShowResponseResultCode(ResultCode: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Reset;
    procedure ToLog(const Text: string);
    procedure SetRequest(Request: TRequest);
    procedure SetResponse(Response: TResponse);
    procedure ShowLog;
    procedure ShowResponse;
    property AutoShowResponseContent: Boolean read FAutoShowResponseContent write FAutoShowResponseContent;
  end;

implementation

{$R *.dfm}

function ContentIsText(const ContentType: string): Boolean;
begin
  Result:=(ContentType='') or
    ContentType.StartsWith('text/') or
    ContentType.StartsWith('application/x-www-form-urlencoded') or
    ContentType.StartsWith('application/javascript');
end;

function ContentIsJSON(const ContentType: string): Boolean;
begin
  Result:=ContentType.StartsWith('application/json');
end;

procedure ShowContentText(Content: TContent; Strings: TStrings);
var
  ContentType,ContentTypeCharset,jsText: string;
  jsValue: TJSONValue;
  Encoding: TEncoding;
begin

  Strings.Clear;

  ContentType:=Content.Headers.ContentType;
  ContentTypeCharset:=Content.Headers.ContentTypeCharset.ToLower;

  if ContentTypeCharset='utf-8' then
    Encoding:=TEncoding.UTF8
  else
    Encoding:=TEncoding.ANSI;

  try

    if ContentIsText(ContentType) then
      Strings.Text:=Encoding.GetString(Content.Content)
    else

    if ContentIsJSON(ContentType) then
    begin
      jsText:=Encoding.GetString(Content.Content);
      jsValue:=TJSONObject.ParseJSONValue(jsText);
      if Assigned(jsValue) then
        Strings.Text:=ToJSON(jsValue,False)
      else
        Strings.Text:=jsText;
    end else

      Strings.Text:=Content.Description;

  except
  end;

end;

procedure ShowContentPicture(Content: TContent; Image: TImage);
begin

  if Assigned(Content) and PictureLoadFromContent(Image.Picture,Content) then
  begin

    Image.Stretch:=
      (Image.Picture.Height>Image.Height) or
      (Image.Picture.Width>Image.Width);

    Image.Hint:=HTTPExtractFileName(Content.ResourceName);

  end else

    Image.Picture.Assign(nil);

end;

{ TCommunicationFrame }

constructor TCommunicationFrame.Create(AOwner: TComponent);
begin
  inherited;
  FAutoShowResponseContent:=True;
end;

function GetResponseColor(Code: Integer): TColor;
begin
  case Code of
  HTTPCODE_SUCCESS: Exit(clGreen);
  end;
  Result:=clRed;
end;

procedure TCommunicationFrame.ShowResponseResultCode(ResultCode: Integer);
begin
  CodeLabel.Caption:=' '+ResultCode.ToString+' ';
  CodeLabel.Color:=GetResponseColor(ResultCode);
  CodeLabel.Visible:=ResultCode<>0;
end;

procedure TCommunicationFrame.ToLog(const Text: string);
begin
  LogMemo.Lines.Add(Text);
end;

procedure TCommunicationFrame.Reset;
begin
  SetRequest(nil);
  SetResponse(nil);
  ShowLog;
end;

procedure TCommunicationFrame.SetRequest(Request: TRequest);
begin

  if Request=nil then
  begin

    RequestMemo.Clear;

  end else begin

    ToLog(Request.Composes);

    ShowContentText(Request,RequestMemo.Lines);

  end;

end;

procedure TCommunicationFrame.SetResponse(Response: TResponse);
begin

  if Response=nil then
  begin

    ResponseMemo.Clear;
    ShowResponseResultCode(0);
    ShowContentPicture(nil,ContentImage);

  end else begin

    ToLog(Response.Composes);

    ShowContentPicture(Response,ContentImage);

    if not Assigned(ContentImage.Picture.Graphic) then
      PictureScrollBox.SendToBack;

    ShowContentText(Response,ResponseMemo.Lines);

    ShowResponseResultCode(Response.ResultCode);

    if Length(Response.Content)=0 then ShowLog else
    if FAutoShowResponseContent or ResponseTab.Down then ShowResponse;

  end;

end;

procedure TCommunicationFrame.LogTabClick(Sender: TObject);
begin
  LogMemo.BringToFront;
end;

procedure TCommunicationFrame.RequestTabClick(Sender: TObject);
begin
  RequestMemo.BringToFront;
end;

procedure TCommunicationFrame.ResponseTabClick(Sender: TObject);
begin
  ResponseMemo.BringToFront;
  if Assigned(ContentImage.Picture.Graphic) then
    PictureScrollBox.BringToFront;
end;

procedure TCommunicationFrame.ClearButtonClick(Sender: TObject);
begin
  LogMemo.Clear;
  SetRequest(nil);
  SetResponse(nil);
end;

procedure TCommunicationFrame.ShowLog;
begin
  LogTab.Click;
  LogTab.Down:=True;
end;

procedure TCommunicationFrame.ShowResponse;
begin
  ResponseTab.Click;
  ResponseTab.Down:=True;
end;

end.
