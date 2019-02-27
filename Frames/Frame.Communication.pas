unit Frame.Communication;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Buttons,
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
    procedure ShowPicture(Content: TContent);
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

{ TCommunicationFrame }

constructor TCommunicationFrame.Create(AOwner: TComponent);
begin
  inherited;
  FAutoShowResponseContent:=True;
end;

procedure TCommunicationFrame.ShowPicture(Content: TContent);
begin

  if Assigned(Content) and PictureLoadFromContent(ContentImage.Picture,Content) then
  begin

    ContentImage.Stretch:=
      (ContentImage.Picture.Height>ContentImage.Height) or
      (ContentImage.Picture.Width>ContentImage.Width);

    ContentImage.Hint:=Content.ResourceName;

  end else begin

    ContentImage.Picture.Assign(nil);
    PictureScrollBox.SendToBack;

  end;

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

    ToLog(
      Request.Method+' '+Request.Resource+CRLF+
      Request.Headers.Text);

    Request.ShowTextContentTo(RequestMemo.Lines);

  end;

end;

procedure TCommunicationFrame.SetResponse(Response: TResponse);
begin

  if Response=nil then
  begin

    ResponseMemo.Clear;
    ShowResponseResultCode(0);
    ShowPicture(nil);

  end else begin

    ToLog(Response.ResultCode.ToString+' '+
      Response.ResultText+CRLF+
      Response.Headers.Text);

    ShowPicture(Response);

    Response.ShowTextContentTo(ResponseMemo.Lines);

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
