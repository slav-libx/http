unit Lib.VCL.HTTPGraphic;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Imaging.JPEG,
  Vcl.Imaging.GIFImg,
  Vcl.Imaging.PNGImage,
  Lib.HTTPContent;

function PictureLoadFromContent(Picture: TPicture; Content: TContent): Boolean;

implementation

type
  TContentStream = class(TMemoryStream)
  public
    constructor Create(const Content: TBytes);
  end;

constructor TContentStream.Create(const Content: TBytes);
begin
  SetPointer(Pointer(Content),Length(Content));
end;

function GetContentTypeGraphicClass(const ContentType: string):  TGraphicClass;
begin
  Result:=nil;
  if ContentType.StartsWith('image/jpeg') then Result:=TJPEGImage else
  if ContentType.StartsWith('image/gif') then Result:=TGIFImage else
  if ContentType.StartsWith('image/png') then Result:=TPNGImage else
  if ContentType.StartsWith('image/vnd.microsoft.icon') then Result:=TIcon;
end;

function CreatePictureGraphic(Picture: TPicture; const ContentType: string): Boolean;
var
  Graphic: TGraphic;
  GraphicClass: TGraphicClass;
begin

  GraphicClass:=GetContentTypeGraphicClass(ContentType);

  if Assigned(GraphicClass) then
  begin
    if GraphicClass=TPNGImage then
      Graphic:=TPNGImage.CreateBlank(0,1,0,0)
    else
      Graphic:=GraphicClass.Create;
    Picture.Graphic:=Graphic;
    Graphic.Free;
  end else
    Picture.Graphic:=nil;

  Result:=Assigned(Picture.Graphic);

end;

function PictureLoadFromContent(Picture: TPicture; Content: TContent): Boolean;
var Stream: TContentStream;
begin

  Result:=False;

  if CreatePictureGraphic(Picture,Content.ContentType) then
  try
    Stream:=TContentStream.Create(Content.Content);
    try
      Picture.Graphic.LoadFromStream(Stream);
      if Picture.Graphic is TGIFImage then
        TGIFImage(Picture.Graphic).Animate:=True;
      Result:=True;
    finally
      Stream.Free;
    end;
  except
    Picture.Graphic:=nil;
  end;

end;

end.
