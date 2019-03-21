unit Form.ClientMain;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.IOUtils,
  System.JSON,
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Lib.JSONStore,
  Lib.HTTPTypes,
  Lib.HTTPConsts,
  Lib.HTTPClient,
  Lib.HTTPContent,
  Form.Request,
  Frame.Communication;

type
  TClientMainForm = class(TForm,IHTTPMonitor)
    ResourcesListBox: TListBox;
    ResourceEdit: TEdit;
    GetResourceButton: TButton;
    AddResourceButton: TButton;
    StatusBar: TStatusBar;
    GetLisResourcesButton: TButton;
    KeepAliveCheckBox: TCheckBox;
    KeepAliveTimeoutEdit: TEdit;
    StorageFilesLabel: TLabel;
    StorageFilesEdit: TEdit;
    RemoveResourceButton: TButton;
    GetResourceExButton: TButton;
    CommunicationFrame: TCommunicationFrame;
    procedure AddResourceButtonClick(Sender: TObject);
    procedure ResourcesListBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GetResourceButtonClick(Sender: TObject);
    procedure GetLisResourcesButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RemoveResourceButtonClick(Sender: TObject);
    procedure GetResourceExButtonClick(Sender: TObject);
    procedure ResourceEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FHTTPClient: THTTPClient;
    FStore: TJSONStore;
    procedure StoreRead;
    procedure StoreWrite;
    procedure CreateClient;
    procedure DoTrace(const Text: string); // IHTTPMonitor
    procedure OnClientClose(Sender: TObject);
    procedure OnClientRequest(Sender: TObject);
    procedure OnClientResponse(Sender: TObject);
    procedure OnClientResource(Sender: TObject);
    procedure OnIdle(Sender: TObject);
  public
  end;

var
  ClientMainForm: TClientMainForm;

implementation

{$R *.dfm}

procedure TClientMainForm.FormCreate(Sender: TObject);
begin

  ResourceEdit.Text:='';
  StorageFilesEdit.Text:='';
  KeepAliveTimeoutEdit.Text:='10';
  KeepAliveCheckBox.Checked:=False;
  ResourcesListBox.Items.Clear;

  CommunicationFrame.Reset;

  FStore:=TJSONStore.Create(ExtractFilePath(ParamStr(0))+'client-store.json');

  StoreRead;

end;

procedure TClientMainForm.FormDestroy(Sender: TObject);
begin
  StoreWrite;
  FStore.Free;
end;

procedure TClientMainForm.StoreRead;
begin

  try
    BoundsRect:=FStore.ReadRect('form.bounds',BoundsRect);
    ResourceEdit.Text:=FStore.ReadString('url-edit');
    StorageFilesEdit.Text:=FStore.ReadString('local-storage','');
    KeepAliveTimeoutEdit.Text:=FStore.ReadInteger('keep-alive.timeout',10).ToString;
    KeepAliveCheckBox.Checked:=FStore.ReadBool('keep-alive.enabled',False);
    FStore.ReadStrings('urls',ResourcesListBox.Items);
  except
    on E: EJSONException do ApplicationShowException(E);
    else raise;
  end;

end;

procedure TClientMainForm.StoreWrite;
begin

  if WindowState=TWindowState.wsNormal then
  FStore.WriteRect('form.bounds',BoundsRect);
  FStore.WriteString('url-edit',ResourceEdit.Text);
  FStore.WriteString('local-storage',StorageFilesEdit.Text);
  FStore.WriteInteger('keep-alive.timeout',StrToIntDef(KeepAliveTimeoutEdit.Text,10));
  FStore.WriteBool('keep-alive.enabled',KeepAliveCheckBox.Checked);
  FStore.WriteStrings('urls',ResourcesListBox.Items);

end;

procedure TClientMainForm.ResourceEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then GetResourceButton.Click;
end;

procedure TClientMainForm.GetResourceButtonClick(Sender: TObject);
begin
  CommunicationFrame.Reset;
  CreateClient;
  FHTTPClient.Get(ResourceEdit.Text);
end;

procedure TClientMainForm.AddResourceButtonClick(Sender: TObject);
begin
  if (ResourceEdit.Text<>'') and (ResourcesListBox.Items.IndexOf(ResourceEdit.Text)=-1) then
  begin
    ResourcesListBox.Items.Add(ResourceEdit.Text);
    ResourcesListBox.ItemIndex:=ResourcesListBox.Items.Count-1;
    StoreWrite;
    FStore.Flush;
  end;
end;

procedure TClientMainForm.GetLisResourcesButtonClick(Sender: TObject);
var S: string;
begin
  CommunicationFrame.Reset;
  CreateClient;
  for S in ResourcesListBox.Items do FHTTPClient.Get(S);
end;

procedure TClientMainForm.RemoveResourceButtonClick(Sender: TObject);
begin
  ResourcesListBox.Items.Delete(ResourcesListBox.ItemIndex);
end;

procedure TClientMainForm.GetResourceExButtonClick(Sender: TObject);
var F: TRequestForm;
begin

  F:=TRequestForm.Create(Self);
  F.SetURL(ResourceEdit.Text);
  F.Request.Headers.SetConnection(KeepAliveCheckBox.Checked,StrToIntDef(KeepAliveTimeoutEdit.Text,0));

  if F.Execute then
  begin
    CommunicationFrame.Reset;
    CreateClient;
    FHTTPClient.Request.Assign(F.Request);
    FHTTPClient.SendRequest;
  end;

end;

procedure TClientMainForm.ResourcesListBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var I: Integer;
begin
  I:=ResourcesListBox.ItemAtPos(Point(X,Y),True);
  if I<>-1 then ResourceEdit.Text:=ResourcesListBox.Items[I];
end;

procedure TClientMainForm.CreateClient;
begin

  if not Assigned(FHTTPClient) then
  begin

    FHTTPClient:=THTTPClient.Create;
    FHTTPClient.Monitor:=Self;
    FHTTPClient.OnRequest:=OnClientRequest;
    FHTTPClient.OnResponse:=OnClientResponse;
    FHTTPClient.OnResource:=OnClientResource;
    FHTTPClient.OnClose:=OnClientClose;
    FHTTPClient.OnIdle:=OnIdle;

  end;

  FHTTPClient.KeepAliveTimeout:=StrToIntDef(KeepAliveTimeoutEdit.Text,0);
  FHTTPClient.KeepAlive:=KeepAliveCheckBox.Checked;

end;

procedure TClientMainForm.OnClientRequest(Sender: TObject);
begin
  CommunicationFrame.SetRequest(THTTPClient(Sender).Request);
end;

procedure TClientMainForm.OnClientResponse(Sender: TObject);
var C: THTTPClient; ContentFileName,Location: string;
begin

  C:=THTTPClient(Sender);

  CommunicationFrame.SetResponse(C.Response);

  if StorageFilesEdit.Text<>'' then
  if Length(C.Response.Content)>0 then
  begin
    ContentFileName:=ChangeFilePath(C.Response.LocalResource,StorageFilesEdit.Text);
    TFile.WriteAllBytes(ContentFileName,C.Response.Content);
  end;

  Location:=C.Response.Headers.GetValue('Location');

  if Location<>'' then
  begin
    C.Request.DecomposeURL(Location);
    C.Request.Headers.SetValue('Host',C.Request.Host);
    C.SendRequest;
  end;

end;

procedure TClientMainForm.DoTrace(const Text: string);
begin
  CommunicationFrame.ToLog(Text+CRLF);
end;

procedure TClientMainForm.OnClientResource(Sender: TObject);
begin
  StatusBar.SimpleText:=THTTPClient(Sender).Request.Resource;
end;

procedure TClientMainForm.OnIdle(Sender: TObject);
begin
  StatusBar.SimpleText:='Complete';
end;

procedure TClientMainForm.OnClientClose(Sender: TObject);
begin
end;

end.
