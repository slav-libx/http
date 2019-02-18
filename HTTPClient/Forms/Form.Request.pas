unit Form.Request;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Lib.HTTPContent,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Lib.HTTPUtils;

type
  TRequestForm = class(TForm)
    RequestButton: TButton;
    CancelButton: TButton;
    MethodLabel: TLabel;
    MethodComboBox: TComboBox;
    HeadersLabel: TLabel;
    AddButton: TButton;
    RequestLabel: TLabel;
    RequestEdit: TEdit;
    ProtocolLabel: TLabel;
    BottomBevel: TBevel;
    HeaderNameComboBox: TComboBox;
    HeaderValueComboBox: TComboBox;
    ContentMemo: TMemo;
    ContentLabel: TLabel;
    ContentTypeComboBox: TComboBox;
    ContentTypeLabel: TLabel;
    FileLabel: TLabel;
    FileNameLabel: TLabel;
    OpenFileButton: TButton;
    OpenDialog1: TOpenDialog;
    RemoveFileButton: TButton;
    HeadersMemo: TMemo;
    ProtocolComboBox: TComboBox;
    procedure AddButtonClick(Sender: TObject);
    procedure HeaderNameComboBoxChange(Sender: TObject);
    procedure OpenFileButtonClick(Sender: TObject);
    procedure RemoveFileButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FURL: string;
    FRequest: TRequest;
    FContentFileName: string;
    procedure SetURL(const Value: string);
  public
    function Execute: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property URL: string read FURL write SetURL;
    property Request: TRequest read FRequest;

  end;

implementation

{$R *.dfm}

constructor TRequestForm.Create(AOwner: TComponent);
begin
  inherited;
  FURL:='';
  FContentFileName:='';
  FRequest:=TRequest.Create;
end;

destructor TRequestForm.Destroy;
begin
  FRequest.Free;
  inherited;
end;

procedure TRequestForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=TCloseAction.caFree;
end;

procedure TRequestForm.SetURL(const Value: string);
begin
  FURL:=Value;
  Request.Reset;
  Request.ParseURL(URL);
  Request.Protocol:='HTTP/1.1';
  Request.Method:='GET';
  Request.AddHeaderValue('Host',Request.Host);
end;

function TRequestForm.Execute: Boolean;
begin

  RequestEdit.Text:=URL;
  ProtocolComboBox.Text:=FRequest.Protocol;
  ContentMemo.Clear;
  MethodComboBox.ItemIndex:=MethodComboBox.Items.IndexOf(FRequest.Method.ToUpper);
  HeaderNameComboBox.OnChange(nil);
  ContentTypeComboBox.ItemIndex:=0;
  FileNameLabel.Caption:='(none)';
  HeadersMemo.Lines.Assign(FRequest.Headers);
  HeaderNameComboBox.Text:='';
  HeaderValueComboBox.Text:='';

  Result:=ShowModal=mrOk;

  if Result then
  begin
    FRequest.ParseURL(RequestEdit.Text);
    FRequest.Protocol:=ProtocolComboBox.Text;
    FRequest.Method:=MethodComboBox.Text;
    FRequest.Headers.Assign(HeadersMemo.Lines);
    if ContentMemo.Text<>'' then
      FRequest.AddContentText(ContentMemo.Text,ContentTypeComboBox.Text)
    else
    if FContentFileName<>'' then
      FRequest.AddContentFile(FContentFileName,ContentTypeComboBox.Text);
  end;

end;

procedure TRequestForm.AddButtonClick(Sender: TObject);
begin
  if string(HeaderNameComboBox.Text).Trim<>'' then
  if string(HeaderValueComboBox.Text).Trim<>'' then
  begin
    HeadersMemo.Lines.Add(HeaderNameComboBox.Text+': '+HeaderValueComboBox.Text);
    HeaderNameComboBox.Text:='';
    HeaderValueComboBox.Text:='';
  end;
end;

procedure TRequestForm.HeaderNameComboBoxChange(Sender: TObject);
var S: string;
begin

  HeaderValueComboBox.Clear;

  S:=HeaderNameComboBox.Text;

  if S='Host' then
  begin
    HeaderValueComboBox.Items.Add(Request.Host);
  end else

  if S='Connection' then
  begin
    HeaderValueComboBox.Items.Add('close');
    HeaderValueComboBox.Items.Add('keep-alive');
    HeaderValueComboBox.Items.Add('keep-alive, Upgrade');
    HeaderValueComboBox.Items.Add('Upgrade');
  end else

  if S='Keep-Alive' then
  begin
    HeaderValueComboBox.Items.Add('timeout=10');
  end else

  if S='Cookie' then
  begin
    HeaderValueComboBox.Items.Add('Name=Value');
  end else

  if S='User-Agent' then
  begin
    HeaderValueComboBox.Items.Add('Mozilla/5.0 (Windows NT 6.1; rv:61.0)');
  end else

  if S='Accept' then
  begin
    HeaderValueComboBox.Items.Add('*/*');
    HeaderValueComboBox.Items.Add('text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8');
  end else

  if S='Accept-Language' then
  begin
    HeaderValueComboBox.Items.Add('ru-RU,ru;q=0.8,en-US;q=0.5,en;q=0.3');
  end else

  if S='Accept-Encoding' then
  begin
    HeaderValueComboBox.Items.Add('gzip, deflate, br');
  end else

  if S='Cache-Control' then
  begin
    HeaderValueComboBox.Items.Add('max-age=0');
    HeaderValueComboBox.Items.Add('no-cache');
  end else

  if S='Pragma' then
  begin
    HeaderValueComboBox.Items.Add('no-cache');
  end else

  if S='Upgrade' then
  begin
    HeaderValueComboBox.Items.Add('websocket');
  end else

  if S='If-Modified-Since' then
  begin
    HeaderValueComboBox.Items.Add('Fri, 15 Feb 2019 21:10:37 GMT');
  end else

  if S='Content-Type' then
  begin
    HeaderValueComboBox.Items.Assign(ContentTypeComboBox.Items);
  end else

  if S='Content-Length' then
  begin
    HeaderValueComboBox.Items.Add('0');
  end else

  if S='Upgrade-Insecure-Requests' then
  begin
    HeaderValueComboBox.Items.Add('1');
  end else

  if S='Authorization' then
  begin
    HeaderValueComboBox.Items.Add('Basic STRING_BASE64');
  end;

  if HeaderValueComboBox.Items.Count>0 then
    HeaderValueComboBox.ItemIndex:=0;

end;

procedure TRequestForm.OpenFileButtonClick(Sender: TObject);
begin
  if OpenDialog1.Execute(Self.Handle) then
  begin
    FContentFileName:=OpenDialog1.FileName;
    FileNameLabel.Caption:=ExtractFileName(FContentFileName);
    ContentTypeComboBox.Text:=HTTPGetMIMEType(ExtractFileExt(FContentFileName));
  end;
end;

procedure TRequestForm.RemoveFileButtonClick(Sender: TObject);
begin
  FContentFileName:='';
  FileNameLabel.Caption:='(none)';
  ContentTypeComboBox.ItemIndex:=-1;
  ContentTypeComboBox.ItemIndex:=0;
end;

end.
