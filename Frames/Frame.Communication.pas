unit Frame.Communication;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Imaging.JPEG,
  Vcl.Imaging.GIFImg,
  Vcl.Imaging.pngimage,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Buttons,
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
    ImagePanel: TPanel;
    ContentImage: TImage;
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

constructor TCommunicationFrame.Create(AOwner: TComponent);
begin
  inherited;
  FAutoShowResponseContent:=True;
end;

function GetGraphicClass(const ContentType: string):  TGraphicClass;
begin
  Result:=nil;
  if ContentType.StartsWith('image/jpeg') then Result:=TJPEGImage else
  if ContentType.StartsWith('image/gif') then Result:=TGIFImage else
  if ContentType.StartsWith('image/png') then Result:=TPngImage else
  if ContentType.StartsWith('vnd.microsoft.icon') then Result:=TIcon;
end;

function PictureLoadFromContent(Picture: TPicture; Content: TContent): Boolean;
var
  Graphic: TGraphic;
  GraphicClass: TGraphicClass;
  Stream: TBytesStream;
begin

  Graphic:=nil;

  GraphicClass:=GetGraphicClass(Content.GetHeaderValue('Content-Type'));

  if Assigned(GraphicClass) then
  begin

    Graphic:=GraphicClass.Create;

    try
      Stream:=TBytesStream.Create(Copy(Content.Content));
      try
        Graphic.LoadFromStream(Stream);
      finally
        Stream.Free;
      end;
    except
      Graphic.Free;
      Graphic:=nil;
    end;

  end;

  Picture.Assign(Graphic);

  Result:=Assigned(Graphic);

end;

procedure TCommunicationFrame.ShowPicture(Content: TContent);
begin

  if Assigned(Content) and PictureLoadFromContent(ContentImage.Picture,Content) then
  begin

    ContentImage.Hint:=Content.ResourceName;
    //Panel1.Height:=Round(Image1.Picture.Height/Image1.Picture.Width*Panel1.Width);
    ImagePanel.BringToFront;
    ImagePanel.Align:=alClient;
    ImagePanel.Parent:=ResponseMemo;
    ImagePanel.Visible:=True;

    if ContentImage.Picture.Graphic is TGIFImage then
      TGIFImage(ContentImage.Picture.Graphic).Animate:=True;

  end else begin

    ImagePanel.Visible:=False;
    ContentImage.Picture.Assign(nil);
    ContentImage.Hint:='';

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

    ToLog(
      Response.ResultText+CRLF+
      Response.Headers.Text);

    ShowPicture(Response);

    Response.ShowTextContentTo(ResponseMemo.Lines);

    ShowResponseResultCode(Response.ResultCode);

    if Length(Response.Content)=0 then ShowLog else
    if FAutoShowResponseContent then ShowResponse;

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
