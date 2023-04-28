unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,UPagenedStreams, ExtCtrls,UCrc32,math, ComCtrls;

type

  TForm1 = class(TForm)
    Box1: TListBox;
    Edit1: TEdit;
    VBox: TPaintBox;
    BDelete: TButton;
    StatusBar1: TStatusBar;
    BCrcCalc: TButton;
    GroupBox1: TGroupBox;
    BClose: TButton;
    BLoad: TButton;
    Binit: TButton;
    BAdd: TButton;
    BCompact: TButton;
    procedure BmemClick(Sender: TObject);
    procedure BCompactClick(Sender: TObject);
    procedure BFragmentsClick(Sender: TObject);
    procedure VBoxPaint(Sender: TObject);
    procedure BDrawClick(Sender: TObject);
    procedure BLoadClick(Sender: TObject);
    procedure BinitClick(Sender: TObject);
    procedure BCloseClick(Sender: TObject);
    procedure BCrcCalcClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure Box1Click(Sender: TObject);
   private
    FManager:TPagesManager;
    procedure ADDFile(const AFilename: string);
    procedure DrawPt(x, y: integer; C: TColor);
   end;

var
  Form1: TForm1;

implementation
{$R *.dfm}
const
 PagesDataStream='PagesData';
 PagesIndexStream='PagesIndex';
var
  Mem,PageList:TFilestream;
  A,B:TCachedStream;
  Ht:TStream;
function FullPath(const AFilename:string):string;
begin
  Result:=ExtractFilepath(Paramstr(0))+AFilename;
end;
procedure TForm1.ADDFile(const AFilename:string);
var
 nm:string;
 FS:TFileStream;
 Ps:TPageEntryStream;
 tt,hs:integer;
begin
 FS:=TFileStream.Create(AFilename,fmOpenread);
 try
    tt:=FManager.PagesCount;
    Ps:=FManager.NewEntry();
    Ps.BeginUpdate;
    hs:=HashCRC32(FS);
    Ps.Size:=Fs.Size;
    Ps.UserData:=hs;
    nm:='['+inttohex(hs,8)+'] '+ExtractFileName(AFilename);
    Ps.CopyFrom(Fs,0);
    Box1.Items.AddObject(nm,Pointer(Ps));
    Ps.EndUpdate ;
 finally
   FS.Free;
 end;
end;

procedure TForm1.BmemClick(Sender: TObject);
var
 s:string;
begin
  if FManager=nil then
     raise Exception.Create('no active manager');
  if not Promptforfilename(s) then
   Exit;
 ADDFile(s);
end;

procedure TForm1.BCompactClick(Sender: TObject);
begin
  FManager.Compact();
  invalidate;
end;

procedure TForm1.BFragmentsClick(Sender: TObject);
begin
  caption:=inttostr(fmanager.GetFragmentRatio)
end;

procedure TForm1.DrawPt(x,y:integer;C:TColor);
begin
   VBox.canvas.Pen.Color := C;
   VBox.canvas.Brush.Color := C;
   VBox.canvas.FillRect(rect(x ,y ,x+9,y+9));
   VBox.canvas.Pen.Color := clblack;
end;

procedure TForm1.VBoxPaint(Sender: TObject);
var
 I,J,D,Offset,Row:integer;
 EntryStream:TPageEntryStream;
 pg:integer;
    function ccColor(Idx:integer):TColor;
    var
     D:TPageBase;
    begin
       if idx >= pg then
         Result :=clWhite
       else begin
           D:=FManager.Pages[idx];
           if D = nil then
              Result :=clSilver
           else if D._parent =Ht then
              Result :=clYellow
           else if D._parent= EntryStream then
              Result:=clAqua
          else if D.FUsed then
            Result :=clRed
           else
              Result :=clLime
       end;
    end;
begin
    if FManager=nil then
        exit;
    EntryStream :=FManager.EntriesListEntry;
    pg:=FManager.PagesCount;
    Offset:=10;
    Row:=32;
    VBox.Canvas.Brush.Color :=clblack;
   // VBox.Canvas.FillRect(VBox.Canvas.ClipRect);
    for D := 0 to  1000 -1 do
    begin
       I:=D div Row;
       J:=D mod Row;
       DrawPt(J*Offset,I*Offset, ccColor(I*Row+J));
    end;
    for I := 0 to VBox.Height div Offset  do
    begin
       VBox.Canvas.MoveTo(0,I*Offset);
       VBox.Canvas.LineTo(VBox.Width,I*Offset);
    end;
    for I := 0 to Row -1 do
    begin
       VBox.Canvas.MoveTo(I*Offset,0);
       VBox.Canvas.LineTo(I*Offset,VBox.Height);
    end;
end;

procedure TForm1.BDrawClick(Sender: TObject);
begin
  invalidate
end;

procedure TForm1.BLoadClick(Sender: TObject);
var
 I:integer;
 Ps:TPageEntryStream;
begin
  Mem:=TFilestream.Create(FullPath(PagesDataStream),fmOpenReadWrite);
  PageList:=TFilestream.Create(FullPath(PagesIndexStream),fmOpenReadWrite);
  FManager:=TPagesManager.Create;
  A:=TCachedStream.Create(Mem,$1000);
  B:=TCachedStream.Create(PageList,$500);
  FManager.Load(A,B);
  Box1.Clear;
  for I:=0 to FManager.EntryCount-1 do
  begin
    Ps:=FManager.Entries[I];
    Box1.Items.AddObject(Inttostr(Ps.ID)+'['+inttohex(Ps.UserData,8)+']',Ps)
  end;
  invalidate;
end;

procedure TForm1.BinitClick(Sender: TObject);
begin
  Mem:=TFilestream.Create(FullPath(PagesDataStream),fmCreate);
  PageList:=TFilestream.Create(FullPath(PagesIndexStream),fmCreate);
  FManager:=TPagesManager.Create;
  A:=TCachedStream.Create(Mem,$1000);
  B:=TCachedStream.Create(PageList,$500);
  FManager.Init(A,B,$1000);
  invalidate;
end;

procedure TForm1.BCloseClick(Sender: TObject);
begin
  if FManager=nil then
     raise Exception.Create('no active manager');
  FManager.Save;
  A.Flush;
  B.Flush;
  Showmessagefmt('%d:%d ; %d:%d',[
      A.Total,A.Found,
      B.Total,B.Found]);
  FreeAndNil(FManager);
  A.Free;
  B.Free;
  Mem.Free;
  PageList.Free;
  invalidate;
end;

procedure TForm1.BCrcCalcClick(Sender: TObject);
var
 Ps:TPageEntryStream;
begin
 with Box1 do
 begin
     if ItemIndex=-1 then
     begin
        Edit1.Text:='no selected item';
        Exit;
     end;
     Ps:= pointer(Items.Objects[ItemIndex]);
     Edit1.Text:=inttohex(HashCRC32(Ps),8);
  end;
end;

procedure TForm1.BDeleteClick(Sender: TObject);
var
 Ps:TPageEntryStream;
begin
 with Box1 do
 begin
    if ItemIndex=-1 then
      Exit;
    Ps:= pointer(Items.Objects[ItemIndex]);
    Items.Delete(ItemIndex);
    FManager.RemoveEntry(Ps.ID );

 end;
 invalidate;
end;

procedure TForm1.Box1Click(Sender: TObject);
var
 Ps:TPageEntryStream;
begin
 with Box1 do
 begin
    if ItemIndex=-1 then
      Exit;
    Ps:= pointer(Items.Objects[ItemIndex]);
    Ht:=Ps;
 end;
 invalidate;

end;

end.

