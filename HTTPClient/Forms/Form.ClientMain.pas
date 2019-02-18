unit Form.ClientMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.IOUtils,
  System.JSON,
  Vcl.Graphics,
  Vcl.Imaging.JPEG,
  Vcl.Imaging.GIFImg,
  Vcl.Imaging.pngimage,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Samples.Gauges,
  Vcl.Buttons,
  Lib.JSON.Store,
  Lib.HTTPClient,
  Lib.HTTPContent,
  Form.Request;

type
  TForm2 = class(TForm)
    ListBox1: TListBox;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    StatusBar1: TStatusBar;
    Button3: TButton;
    CheckBox1: TCheckBox;
    Edit2: TEdit;
    Label1: TLabel;
    Edit3: TEdit;
    Button4: TButton;
    Timer1: TTimer;
    Gauge1: TGauge;
    Image1: TImage;
    Panel1: TPanel;
    Button5: TButton;
    Label2: TLabel;
    Button6: TButton;
    SpeedButton1: TSpeedButton;
    Panel2: TPanel;
    SpeedButton2: TSpeedButton;
    Memo1: TMemo;
    Memo2: TMemo;
    SpeedButton3: TSpeedButton;
    Memo3: TMemo;
    procedure Button2Click(Sender: TObject);
    procedure ListBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
  private
    FHTTPClient: THTTPClient;
    FStore: TJSONStore;
    GaugeInc: Integer;
    procedure CreateClient;
    procedure ShowPicture(const FileName,PictureHint: string);
    procedure ShowResponseResultCode(ResultCode: Integer);
    procedure OnClientClose(Sender: TObject);
    procedure OnClientRequest(Sender: TObject);
    procedure OnClientResponse(Sender: TObject);
    procedure OnClientMessage(Sender: TObject);
    procedure OnClientResource(Sender: TObject);
    procedure OnIdle(Sender: TObject);
    procedure OnClientException(Sender: TObject);
  public
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin

  FStore:=TJSONStore.Create(ExtractFilePath(ParamStr(0))+'store.json');

  BoundsRect:=FStore.ReadRect('form.rect',BoundsRect);
  Edit1.Text:=FStore.ReadString('url-edit');
  Edit3.Text:=FStore.ReadString('local-storage','');
  Edit2.Text:=FStore.ReadInteger('keep-alive.timeout',10).ToString;
  CheckBox1.Checked:=FStore.ReadBool('keep-alive.enabled',False);
  FStore.ReadStrings('urls',ListBox1.Items);

  SpeedButton2.Down:=True;
  SpeedButton2.OnClick(nil);

  ShowPicture('','');
  ShowResponseResultCode(0);

end;

procedure TForm2.FormDestroy(Sender: TObject);
begin

  if WindowState=TWindowState.wsNormal then
    FStore.WriteRect('form.rect',BoundsRect);
  FStore.WriteString('url-edit',Edit1.Text);
  FStore.WriteString('local-storage',Edit3.Text);
  FStore.WriteInteger('keep-alive.timeout',StrToIntDef(Edit2.Text,10));
  FStore.WriteBool('keep-alive.enabled',CheckBox1.Checked);
  FStore.WriteStrings('urls',ListBox1.Items);

  FStore.Free;

end;

procedure TForm2.ShowPicture(const FileName,PictureHint: string);
var S: string;
begin
  S:=ExtractFileExt(FileName).ToLower+'.';
  if S<>'.' then
  if '.jpeg.jpg.gif.png.'.Contains(S) then
  if FileExists(FileName) then
  try
    Image1.Picture.LoadFromFile(FileName);
    Image1.Hint:=PictureHint;
    Panel1.Height:=Round(Image1.Picture.Height/Image1.Picture.Width*Panel1.Width);
    Panel1.Top:=Height-Panel1.Height-96;
    Panel1.Visible:=True;
    if Image1.Picture.Graphic is TGIFImage then
      TGIFImage(Image1.Picture.Graphic).Animate:=True;
    Exit;
  except
  end;
  Panel1.Visible:=False;
  Image1.Picture.Assign(nil);
  Image1.Hint:='';
end;

procedure TForm2.ShowResponseResultCode(ResultCode: Integer);
begin

  Label2.Caption:=' '+ResultCode.ToString+' ';

  case ResultCode of
  200: Label2.Color:=clGreen;
  else Label2.Color:=clRed;
  end;

  Label2.Visible:=ResultCode<>0;

end;

procedure TForm2.SpeedButton1Click(Sender: TObject);
begin
  Memo2.BringToFront;
end;

procedure TForm2.SpeedButton2Click(Sender: TObject);
begin
  Memo1.BringToFront;
  Panel1.BringToFront;
end;

procedure TForm2.SpeedButton3Click(Sender: TObject);
begin
  Memo3.BringToFront;
end;

procedure TForm2.Image1Click(Sender: TObject);
begin
  ShowPicture('','');
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  ShowPicture('','');
  ShowResponseResultCode(0);
  CreateClient;
  FHTTPClient.Get(Edit1.Text);
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  if (Edit1.Text<>'') and (ListBox1.Items.IndexOf(Edit1.Text)=-1) then
  begin
    ListBox1.Items.Add(Edit1.Text);
    ListBox1.ItemIndex:=ListBox1.Items.Count-1;
  end;
end;

procedure TForm2.Button3Click(Sender: TObject);
var S: string;
begin
  ShowPicture('','');
  ShowResponseResultCode(0);
  CreateClient;
  for S in ListBox1.Items do FHTTPClient.Get(S);
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  Memo1.Clear;
  ShowPicture('','');
  ShowResponseResultCode(0);
end;

procedure TForm2.Button5Click(Sender: TObject);
begin
  ListBox1.Items.Delete(ListBox1.ItemIndex);
end;

procedure TForm2.Button6Click(Sender: TObject);
var F: TRequestForm;
begin
  F:=TRequestForm.Create(Self);
  F.URL:=Edit1.Text;
  F.Request.AddHeaderKeepAlive(CheckBox1.Checked,StrToInt64Def(Edit2.Text,0));
  if F.Execute then
  begin
    ShowPicture('','');
    ShowResponseResultCode(0);
    CreateClient;
    FHTTPClient.Request.Assign(F.Request);
    FHTTPClient.SendRequest;
  end;
end;

procedure TForm2.ListBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var I: Integer;
begin
  I:=ListBox1.ItemAtPos(Point(X,Y),True);
  if I<>-1 then Edit1.Text:=ListBox1.Items[I];
end;

procedure TForm2.CreateClient;
begin

  if not Assigned(FHTTPClient) then
  begin
    FHTTPClient:=THTTPClient.Create;
    FHTTPClient.OnRequest:=OnClientRequest;
    FHTTPClient.OnResponse:=OnClientResponse;
    FHTTPClient.OnMessage:=OnClientMessage;
    FHTTPClient.OnResource:=OnClientResource;
    FHTTPClient.OnClose:=OnClientClose;
    FHTTPClient.OnIdle:=OnIdle;
    FHTTPClient.OnException:=OnClientException;
  end;

  FHTTPClient.KeepAliveTimeout:=StrToInt64Def(Edit2.Text,0);
  FHTTPClient.KeepAlive:=CheckBox1.Checked;

end;

procedure TForm2.OnClientRequest(Sender: TObject);
var C: THTTPClient;
begin
  C:=THTTPClient(Sender);
  Memo1.Lines.Add(C.Request.Method+' '+C.Request.Resource);
  Memo1.Lines.AddStrings(C.Request.Headers);
  Memo1.Lines.Add('');
  C.Request.ShowContentTo(Memo3.Lines);
end;

procedure TForm2.OnClientResponse(Sender: TObject);
var C: THTTPClient; S: string;
begin

  C:=THTTPClient(Sender);

  Memo1.Lines.Add(C.Response.ResultText);
  Memo1.Lines.AddStrings(C.Response.Headers);
  Memo1.Lines.Add('');

  if Length(C.Response.Content)>0 then
  begin
    S:=Edit3.Text+C.Response.ResourceName;
    if C.Response.ResultCode=200 then
      TFile.WriteAllBytes(S,C.Response.Content)
    else
      TFile.WriteAllBytes(S,C.Response.Content);
    ShowPicture(S,C.Response.ResourceName);
  end;

  C.Response.ShowContentTo(Memo2.Lines);

  ShowResponseResultCode(C.Response.ResultCode);

end;

procedure TForm2.OnClientMessage(Sender: TObject);
var C: THTTPClient;
begin
  C:=THTTPClient(Sender);
  Memo1.Lines.Add(C.Message);
  Memo1.Lines.Add('');
end;

procedure TForm2.OnClientResource(Sender: TObject);
var C: THTTPClient;
begin
  C:=THTTPClient(Sender);
  StatusBar1.SimpleText:=C.Request.Resource;
end;

procedure TForm2.OnIdle(Sender: TObject);
begin
  StatusBar1.SimpleText:='Complete';
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  if GaugeInc=0 then GaugeInc:=1;
  if Gauge1.Progress=100 then GaugeInc:=-1 else
  if Gauge1.Progress=0 then GaugeInc:=1;
  Gauge1.Progress:=Gauge1.Progress+GaugeInc;
end;

procedure TForm2.OnClientClose(Sender: TObject);
begin
  Memo1.Lines.Add('close connection');
end;

procedure TForm2.OnClientException(Sender: TObject);
var C: THTTPClient;
begin
  C:=THTTPClient(Sender);
  Memo1.Lines.Add(C.ExceptionCode.ToString+' '+C.ExceptionMessage);
end;

end.
