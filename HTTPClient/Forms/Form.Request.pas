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
  Lib.HTTPConsts,
  Lib.HTTPUtils,
  Lib.HeaderValues;

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
    FRequest: TRequest;
    FContentFileName: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    procedure SetURL(const Value: string);
    property Request: TRequest read FRequest;

  end;

implementation

{$R *.dfm}

constructor TRequestForm.Create(AOwner: TComponent);
begin
  inherited;
  FContentFileName:='';
  FRequest:=TRequest.Create;
  HTTPGetContentTypes(ContentTypeComboBox.Items);
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
  Request.Reset;
  Request.ParseURL(Value);
  Request.Protocol:=PROTOCOL_HTTP11;
  Request.Method:=METHOD_GET;
  Request.AddHeaderValue('Host',Request.Host);
  RequestEdit.Text:=Value;
end;

function TRequestForm.Execute: Boolean;
begin

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
begin

  GetHeaderRequestValues(RequestEdit.Text,HeaderNameComboBox.Text,
    HeaderValueComboBox.Items);

  HeaderValueComboBox.Text:='';
  HeaderValueComboBox.ItemIndex:=-1;
  HeaderValueComboBox.ItemIndex:=0;

end;

procedure TRequestForm.OpenFileButtonClick(Sender: TObject);
begin
  if OpenDialog1.Execute(Handle) then
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
