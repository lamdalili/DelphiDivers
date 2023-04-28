unit UPagenedStreams;

interface

uses
   SysUtils, Variants, Classes;

const
   END_TAG = Integer(-1);

type
  TInitHDR=packed record
    mCrc:Cardinal;
    mPagesCount:integer;
    mPageSize:integer;
    mEntriesList:integer;
    mEntrieSize:integer;
    mEntriesListCount:integer;//Entries Stream  used pages
    mEntriesCount:integer;
    mFreeList:integer;
  end;
  TEntryState=(esValid=$2300,esUnknown);// deleted entry
  TEntryHDR=packed record
    mStart:integer;
    mSize:integer;
    mID:integer;
    mUserData:Integer;
    mState:TEntryState;
  end;

  TPagesManager=class;
  TPageEntryStream=class;
  PPageBase=^TPageBase;
  TPageBase=class
  private
    FNext: TPageBase;
  public
    FUsed:boolean;
    Address:integer;
    _parent:TStream;
    property Next:TPageBase read FNext write FNext;
  end;
  TCachedStream=class;
  TPagesManager=class
  protected
    FFreeList:TPageBase;
    Stream:TStream;
    FPages:TList;
    FEntries:TList;
    FAutoCompact:boolean;
    FFreeThreshold:integer;
    FPageSize: integer;
    FIniStream:TStream;
    FEntriesListEntry:TPageEntryStream;
    FUpdateCount:integer;
    FDestroing:boolean;
    procedure SetPagesCount(const Value: integer);
    procedure Update;
    procedure UpdateEntryLink(AEntry: TPageEntryStream);
    procedure CompactEntries;
    function NewPages(ACount: integer): TPageBase;
    function PageToLoc(APg: TPageBase): integer;
    function GetEntriesCount: integer;
    function GetEntryByID(ID: integer): TPageEntryStream;
    function GetPage(idx: integer): TPageBase;
    function CalcPages(ASize: integer): integer;
    function GetPagesCount: integer;
    procedure SetPageSize(const Value: integer);
    procedure PageDataMove(Dest,Src: TPageBase);
    function GetPageEntryStream(index: integer): TPageEntryStream;virtual;
    procedure ClearPagesList();
    function FindEntry(ID:integer;var AOut:TPageEntryStream):boolean;virtual;
    function OpenPage(idx: integer): TPageBase;
    procedure UpdatePageNextLink(APage:TPageBase);
    procedure BeginUpdate();
    procedure EndUpdate();
    property FreeList:TPageBase read FFreeList write FFreeList;
    procedure Collect(P:TPageBase);
  public
    constructor Create();
    destructor Destroy();override;
    procedure MarkUsed(P: TPageBase; AUsed: boolean);
    property PageSize:integer read FPageSize write SetPageSize;
    property PagesCount:integer read GetPagesCount write SetPagesCount;
    property EntryCount:integer read GetEntriesCount;
    property EntriesListEntry:TPageEntryStream read fEntriesListEntry;
    procedure Load(AStream,AInit:TStream);
    procedure Init(AStream,AInit:TStream;APagesize:integer);
    function NewEntry():TPageEntryStream;
    property Pages[idx:integer]:TPageBase read GetPage;
    procedure Compact();
    function GetFragmentRatio: integer;
    property Entries[index:integer]:TPageEntryStream read GetPageEntryStream;
    property IDs[ID:integer]:TPageEntryStream read GetEntryByID;
    procedure RemoveEntry(ID:integer);virtual;
    procedure Save;
  end;

  TPageEntryStream = class(TStream)
  private
    FStart : TPageBase;
    FSize   : Integer;
    FCurPage:TPageBase;
    FPosition:integer;
    FPagePos:integer;
    FPagesList:TList;
    FUpdateCount:integer;
    Address:integer;
    function GetPageDataSize: integer;
    procedure AllocPages(ACount: integer);
    procedure SetStart(const Value: TPageBase);
    function GetPagesCount: integer;
    function GetPages(index: integer): TPageBase;
    procedure Update;
  protected
    FManager:TPagesManager;
    function GetSize():Int64;override;
    procedure SetSize(const NewSize: Int64);override;
    property Pages[index:integer]:TPageBase read GetPages;
    property Count:integer read GetPagesCount;
    procedure ReadData(APos:integer;var Buffer;Count: integer);virtual;
    procedure WriteData(APos:integer;const Buffer;Count: integer);virtual;
  public
    State:TEntryState;
    ID:integer;
    UserData:integer;
    constructor Create();
    destructor Destroy();override;
    procedure BeginUpdate();
    procedure EndUpdate();
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property Start:TPageBase read FStart write SetStart;
    procedure Load(AStart,ASize:integer);
    procedure LoadPages();
 end;

 TCacheItem=class
 protected
   Next:TCacheItem;
   Segment:integer;
   FMem:Pointer;
   Owner:TCachedStream;
   Modified:boolean;
   procedure Flush();
   function GetPageSize():Integer;
 end;

 PCacheItem=^TCacheItem;
 TCachedStream=class(TStream)
 protected
    FCount:integer;
    FFirst:TCacheItem;
    FNextList:PCacheItem;
    FStream:TStream;
    FPageSize:integer;
    FPosition:integer;
    FMaxCache: integer;
    FSegments: array of TObject;
    procedure SetMaxCache(const Value: integer);
    function GetSize():Int64;override;
    procedure SetSize(const NewSize: Int64);override;
    function NewCacheItem(ASeg:integer):TCacheItem;
    function LoadPage(ASeg:integer): TCacheItem;
    procedure SetCache(Address:integer;Value:Pointer);
    function GetCache(Address:integer): Pointer;
    property Caches[Address:integer]:Pointer read GetCache write SetCache;
   public
    Total:integer;   // for test
    Found:integer;   // for test
    constructor Create(AStream:TStream;APageSize:integer);
    destructor Destroy();override;
    property MaxCache:integer read FMaxCache write SetMaxCache;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure Flush;
end;

implementation
 uses UCrc32;
{ TPageEntryStream }

function TPageEntryStream.GetPageDataSize: integer;
begin
  if (FCurPage.Next=nil)  then
  begin
  // do not use FSize - FPosition  since  FPosition need updated each
  // write read loop
     Result :=FSize mod FManager.PageSize;
     if (Result=0) and (FSize <> 0) then
        Result:=FManager.PageSize
  end else
     Result:=FManager.PageSize;
end;

function TPageEntryStream.GetSize: Int64;
begin
   Result:=FSize;
end;

procedure TPageEntryStream.ReadData(APos: integer; var Buffer; Count: integer);
begin
    FManager.Stream.Position := APos;
    FManager.Stream.Read(Buffer,Count);
end;

procedure TPageEntryStream.WriteData(APos: integer; const Buffer;Count: integer);
begin
    FManager.Stream.Position := APos;
    FManager.Stream.Write(Buffer,Count);
end;

function TPageEntryStream.Read(var Buffer;Count: Integer): Longint;
var
  P:PByteArray;
  Rd,L:integer;
begin
  Result := 0;
  if FCurPage =nil then
     Exit;
  P:=@Buffer;
  repeat
      Rd := GetPageDataSize - FPagePos;
      if (Rd = 0)and(FCurPage.Next <>nil)then
      begin
         FCurPage:=FCurPage.Next;
         FPagePos:=0;
         Rd:=GetPageDataSize;
      end;
      if Rd > Count then
         Rd := Count;
      if Rd = 0 then
         break;
      L:= FCurPage.Address * FManager.PageSize +FPagePos;
      ReadData(L,P[Result],Rd);
      Inc(FPagePos,Rd);
      Inc(Result,Rd);
      Dec(Count,Rd);
  until Count=0;
  Inc(FPosition,Result);
end;

function TPageEntryStream.Write(const Buffer; Count: Integer): Longint;
var
  P:PByteArray;
  Wt,Sz,L:integer;
begin
  Result := 0;
  Sz:=FPosition+Count;
  if Sz > FSize then
     Size := Sz;
  if FCurPage =nil then
     Exit;
  P:=@Buffer;
  repeat
      Wt := FManager.PageSize - FPagePos;
      if (Wt = 0)and(FCurPage.Next <>nil)then
      begin
         FCurPage:=FCurPage.Next;
         FPagePos:=0;
         Wt:=FManager.PageSize;
      end;
      if Wt > Count then
         Wt := Count;
      if Wt = 0 then
         break;
      L:= FCurPage.Address * FManager.PageSize+FPagePos;
      WriteData(L,P[Result],Wt);
      Inc(FPagePos,Wt);
      Inc(Result,Wt);
      Dec(Count,Wt);
  until Count=0;
  Inc(FPosition,Result);
end;

function TPageEntryStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if FStart <> nil then
  begin
      case Origin of
        soFromBeginning: FPosition := Offset;
          soFromCurrent: FPosition := FPosition + Offset;
              soFromEnd: FPosition := FSize - Offset;
      end;
      if FPosition > FSize then
        FPosition := FSize
      else if FPosition < 0 then
        FPosition := 0;
      FCurPage := FPagesList[FPosition  div FManager.PageSize];
      FPagePos := FPosition mod FManager.PageSize;
  end;
  Result := FPosition;
end;

procedure TPageEntryStream.SetSize(const NewSize: Int64);
var
 N1,N2,OldPosition:integer;
 Last:TPageBase;
begin
    N1:= FPagesList.Count;
    N2:= FManager.CalcPages(NewSize);
    FSize :=NewSize;
    OldPosition := FPosition;
    FManager.BeginUpdate();
    try
      if N1 < N2 then
      begin
         if FCurPage = nil then // null page
         begin
            FCurPage:=FManager.NewPages(1);
            FCurPage._parent:=Self;
            FStart:=FCurPage;
            FPagePos:=0;
            FPagesList.Clear;
            FPagesList.Add(FStart);
         end;
         AllocPages(N2);
      end else if (N1 > N2) then
      begin
         FManager.Collect(Pages[N2]);
         FPageslist.Count :=N2;
      end;

      if NewSize= 0 then
      begin
        FStart:=nil;
        FCurPage:=nil;
        FPagePos:=0;
        FPosition:=0;
        FPageslist.Clear;
      end else if N1 <> N2 then
      begin  //set last page 'next' to nil
        Last:=Pages[N2-1];
        FManager.UpdatePageNextLink(Last);
        if OldPosition > NewSize then
           Position := NewSize;
      end;
      Update();
    finally
       FManager.EndUpdate();
    end;
end;

procedure TPageEntryStream.AllocPages(ACount: integer);
var
 I,Sz:integer;
 P:TPageBase;
begin
  if ACount-FPagesList.Count < 1 then
     Exit;
  P:=FPagesList[FPagesList.Count-1];
  P.Next :=FManager.NewPages(ACount-FPagesList.Count);

  Sz:=SizeOf(TInitHdr)+ Fmanager.PagesCount* SizeOf(integer);
  if Fmanager.FIniStream.Size < Sz then
     Fmanager.FIniStream.Size:=Sz;
  for I := FPagesList.Count to ACount-1 do
  begin
    FManager.UpdatePageNextLink(P);
    P:=P.Next;
    P._parent:=Self;
    FPagesList.Add(P);
  end;
end;

procedure TPageEntryStream.Load(AStart, ASize: integer);
begin
  Start := FManager.Pages[AStart];
  LoadPages();
  FSize:=ASize;
  Update();// for test
end;

procedure TPageEntryStream.SetStart(const Value: TPageBase);
var
 Pg:TPageBase;
begin
  if FStart=Value then
     Exit;
  FStart := Value;
  FCurPage:=Value;
  FPagesList.Clear;
  if Value<>nil then
  begin
     Pg:=Value;
     repeat
       FPagesList.Add(Pg);
       Pg:=Pg.Next;
     until Pg=nil;
  end;
  FPosition:=0;
  FPagePos:=0;
end;

destructor TPageEntryStream.Destroy;
begin
  FPagesList.Free;
  inherited;
end;

function TPageEntryStream.GetPagesCount: integer;
begin
   Result:=FPagesList.Count;
end;

function TPageEntryStream.GetPages(index: integer): TPageBase;
begin
   Result:=FPagesList[index];
end;

procedure TPageEntryStream.LoadPages;
var
 Pg:TPageBase;
begin
  if FStart <> nil then
  begin
     FPagePos :=0;
     Pg:=FStart;
     repeat
       Pg._parent:=Self;
       Pg.FUsed :=True;
       Pg:=Pg.Next;
     until Pg=nil;
  end;
end;

constructor TPageEntryStream.Create;
begin
  FPagesList:=TList.Create;
  Address := END_TAG;
end;

procedure TPageEntryStream.Update;
begin
  if FUpdateCount=0 then
    FManager.UpdateEntryLink(Self);
end;

procedure TPageEntryStream.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TPageEntryStream.EndUpdate;
begin
  dec(FUpdateCount);
  Update();
end;

{ TPagesManager }

constructor TPagesManager.Create;
begin
  FPages:=TList.Create;
  FEntries:=TList.Create;
  FPageSize:=$200;
  FEntriesListEntry:= TPageEntryStream.Create;
  FEntriesListEntry.FManager :=Self;
end;

destructor TPagesManager.Destroy;
begin
  FDestroing:=True;
  ClearPagesList();
  FEntries.Free;
  FPages.Free;
  FEntriesListEntry.Free;
  inherited;
end;

procedure TPagesManager.ClearPagesList;
var
 I:integer;
begin
    FFreeList:=nil;
    FEntriesListEntry.Size:=0;
    for I:= FEntries.Count-1 downto 0 do   
      TObject(FEntries[I]).Free;
    for I:= FPages.Count-1 downto 0 do
      TObject(FPages[I]).Free;
    FPages.Clear;
end;

function TPagesManager.CalcPages(ASize:integer): integer;
begin
  Result:=ASize div PageSize;
  if Result*PageSize < ASize then
     Inc(Result);
end;

function TPagesManager.GetPage(idx: integer): TPageBase;
begin
  if idx =END_TAG then
     Result:=nil
  else begin
     Result:= FPages[idx];
  end;
end;

function TPagesManager.PageToLoc(APg:TPageBase):integer;
begin
   if APg = nil then
      Result:= END_TAG
   else begin
      FPages[APg.Address];
      Result:= APg.Address;
   end;
end;

function TPagesManager.OpenPage(idx: integer): TPageBase;
begin
  if idx =END_TAG then
     Result:=nil
  else begin
     Result:= FPages[idx];
     if Result= nil then
     begin
       Result:=TPageBase.Create;
       FPages[idx]:=Result;
       Result.Address:=idx;
     end;
  end;
end;

procedure TPagesManager.Load(AStream,AInit: TStream);
var
 I,L:integer;
 Hdr:TInitHDR;
 Entrys:TEntryHDR;
 En:TPageEntryStream;
 Crc:cardinal;
begin
    Stream:=AStream;
    FIniStream:= AInit;
    BeginUpdate();
    try
      ClearPagesList();
      FIniStream.Position :=0;
      FIniStream.ReadBuffer(Hdr,SizeOf(TInitHDR));
      Crc:=Hdr.mCrc;
      Hdr.mCrc:=0;
      if HashCRC32Buff(Hdr,SizeOf(TInitHDR))<>Crc then
         raise Exception.Create('crc error');
      PageSize:=Hdr.mPageSize;
      PagesCount:=Hdr.mPagesCount;
      FEntriesListEntry.FSize:=Hdr.mEntrieSize;
      for I:=0 to PagesCount-1 do
          OpenPage(I);  // create pages
      for I:=0 to PagesCount-1 do
      begin // load nexts
          FIniStream.ReadBuffer(L,SizeOf(Integer));
          Pages[i].Next :=Pages[L];{}
      end;
      FFreeList:=Pages[Hdr.mFreeList];
      if Hdr.mEntriesListCount <> 0 then
      begin
         FEntriesListEntry.Load(Hdr.mEntriesList,Hdr.mEntrieSize);
      end;
      for I:= 0 to  Hdr.mEntriesCount-1 do
      begin
         En:=NewEntry();
         FEntriesListEntry.ReadBuffer(Entrys,SizeOf(TEntryHDR));
         En.ID:= Entrys.mID;       //  warning   test if exists
         En.State:=Entrys.mState;
         En.UserData:=Entrys.mUserData;
         En.Load(Entrys.mStart,Entrys.mSize);
      end;
    finally
      EndUpdate();
    end;
end;

procedure TPagesManager.Save();
var
 I:integer;
 En:TPageEntryStream;
 Entrys:TEntryHDR;
begin
    FEntriesListEntry.Position:=0;
    for I:=0 to EntryCount -1 do
    begin
        En:=FEntries[I];
        Entrys.mStart:=PagetoLoc(En.Start);
        Entrys.mSize :=En.Size;
        Entrys.mID :=En.ID;
        Entrys.mUserData :=En.UserData;
        Entrys.mState:=En.State;
        FEntriesListEntry.WriteBuffer(Entrys,SizeOf(TEntryHDR));
    end;
    Update();
end;

procedure TPagesManager.PageDataMove(Dest,Src: TPageBase);
var
  P:Pointer;
begin
  GetMem(P,PageSize);
 try
     Stream.Position := Src.Address * PageSize;
     Stream.Read(P^,PageSize);
     Stream.Position := Dest.Address * PageSize;
     Stream.Write(P^,PageSize);
  finally
    FreeMem(P);
  end;
end;

function TPagesManager.NewPages(ACount:integer): TPageBase;
var
 idx,I:integer;
 Pg,B:TPageBase;
begin
   if FreeList <> nil then
   begin
     Result:=FreeList;
     repeat
        Pg:=FreeList;
        Pg.FUsed:=true;
        Pg._parent:=nil;
        FreeList:=FreeList.Next;
        Dec(ACount);
     until (FreeList = nil) or (ACount = 0) ;
     Pg.Next :=nil;
   end else begin
      idx:=PagesCount;
      PagesCount:=PagesCount+1;
      Result:= OpenPage(idx);
      Result.FUsed:=True;
      Pg:=Result;
      Dec(ACount);
   end;
   if (ACount > 0) then
   begin
      idx:=PagesCount;
      PagesCount:=PagesCount+ACount;
      for I:=Idx to PagesCount-1 do
      begin
        B:= OpenPage(I);
        B.FUsed :=true;
        Pg.Next := B;
        Pg:=B;
      end;
   end;
   if PagesCount*PageSize > Stream.Size then
      Stream.Size:= PagesCount*PageSize;
end;

procedure TPagesManager.Collect(P:TPageBase);
var
  Last,N:TPageBase;
begin
      N:=P;
      while N <> nil do
      begin
         N.FUsed:=False;
         N._parent:=nil;
         N:=N.Next;
      end;
      Last:=P;
      while  Last.Next <> nil do
      begin
         Last:=Last.Next;
         inc(FFreeThreshold);
      end;
      Last.Next :=FreeList;
      UpdatePageNextLink(Last);
      FreeList:=P;
      if FAutoCompact and (FFreeThreshold > 50) then
      begin
         FFreeThreshold:=0;
         if GetFragmentRatio() >=20 then // 20%
            Compact();
      end;
end;

function TPagesManager.GetFragmentRatio():integer;
var
 N,B:TPageBase;
 I,C:integer;
 V:Extended;
begin
   Result:=0;
   N:=nil;
   for I:= FPages.Count-1 downto 0 do
   begin  // last used
      B:=FPages[I];
     if B.FUsed then
      begin
         N:=B;
         break;
      end;
   end;
   if N <> nil then
   begin
     C:=0;
     for I:= 0 to N.Address do
     begin
        N:=FPages[I];
        if not N.FUsed then
           Inc(C);
     end;
     V:= C *100 / (N.Address+1) ;
     Result := Integer(Trunc(V)); //Ciel
     if Frac(V) > 0 then
        Inc(Result);
   end;
end;

procedure TPagesManager.Init(AStream,AInit:TStream;APagesize:integer);
begin
  Stream:=AStream;
  FIniStream:= AInit;
  PageSize:=APagesize;
  BeginUpdate();
  try
    ClearPagesList();
  finally
    EndUpdate();
  end;
end;

function TPagesManager.GetPagesCount: integer;
begin
  Result := FPages.Count;
end;

procedure TPagesManager.MarkUsed(P:TPageBase;AUsed:boolean);
begin
  while P <> nil do
  begin
     P.FUsed :=AUsed;
     P:=P.Next;
  end;
end;

function PagesSort(P1,P2:Pointer):integer;
var
 Pg1:TPageBase absolute P1;
 Pg2:TPageBase absolute P2;
begin
  if Pg1.FUsed = Pg2.FUsed then
  begin
     if Pg1.Address = Pg2.Address then
        Result:=0
     else if Pg1.Address < Pg2.Address then
        Result:=-1
     else
        Result:= 1;
  end else if Pg1.FUsed then
     Result := -1
  else
     Result := 1;
end;

procedure TPagesManager.CompactEntries;
var
 I,V:integer;
 En:TPageEntryStream;
begin
  V:=0;
  for I:= 0 to FEntries.Count-1 do
  begin
     En:=FEntries[I];
     if En.State <> esValid then
     begin
        En.Free;
        continue;
     end;
     FEntries[V]:=En;
     En.Address :=V;// update address
     UpdateEntryLink(En);// update
     inc(V);
  end;
  FEntries.Count:=V;
  FEntriesListEntry.Size := V * SizeOf(TEntryHDR);// trim extra size
end;

procedure TPagesManager.Compact;
var
 Idxs:TList;
 J,I,D,aLiveCount:integer;
 aLive,FreePage :TPageBase;
begin
  BeginUpdate();
  try
    CompactEntries();
    aLiveCount:= 0;
    Idxs:=TList.Create;
    try
      idxs.Assign(FPages);
      idxs.Sort(PagesSort);
      for I:= 0 to idxs.Count-1 do
      begin
          if not TPageBase(idxs[I]).FUsed then
            break;
         inc(aLiveCount);
      end;
      if aLiveCount=FPages.Count then // no free page
         Exit;
      D:=aLiveCount;
      if aLiveCount <> 0 then
        for I:= FPages.Count-1 downto aLiveCount do
        begin
           aLive:=FPages[I];
          if aLive.FUsed then
           begin
             FreePage:=idxs[D];
             J:=FreePage.Address;
             PageDataMove(FreePage,aLive);
             FPages.Exchange(I,J);
             aLive.Address:= J;
             FreePage.Address:=I;
             FreePage.Free;
             Inc(D);
           end;
        end;
    finally
      Idxs.Free;
    end;
    FPages.Count:=aLiveCount;
    FreeList:=nil;
    FIniStream.Size:=SizeOf(TInitHDR)+ aLiveCount*SizeOf(Integer);
    for I:=0 to aLiveCount-1 do
      UpdatePageNextLink(Pages[I]);
    Stream.Size:=aLiveCount*PageSize; // trim extra size
  finally
    EndUpdate();
  end;
end;

procedure TPagesManager.SetPageSize(const Value: integer);
begin
  FPageSize := Value;
end;

function TPagesManager.NewEntry: TPageEntryStream;
begin
   Result := TPageEntryStream.Create;
   Result.Address := FEntries.Add(Result);
   Result.ID := Result.Address;
   Result.FManager :=Self;
   Result.State:=esValid;
end;

function TPagesManager.GetPageEntryStream(index: integer): TPageEntryStream;
begin
   Result:= FEntries[index];
end;

function TPagesManager.FindEntry(ID: integer;var AOut: TPageEntryStream): boolean;
var
  I:integer;
begin
   for I :=0 to FEntries.Count-1 do
   begin
      AOut:=FEntries[I];
      if AOut.ID=ID then
      begin
         Result:=True;
         Exit;
      end;
   end;
   Result:=False;
   AOut:=nil;
end;

procedure TPagesManager.RemoveEntry(ID: integer);
var
  Entry:TPageEntryStream;
begin
  Entry:=GetEntryByID(ID);
 // FEntries.Remove(Entry);
  Entry.Size:=0;
  Entry.State:=esUnknown;
  Entry.ID :=-1;
  UpdateEntryLink(Entry);
end;

function TPagesManager.GetEntriesCount: integer;
begin
   Result:=FEntries.Count;
end;

function TPagesManager.GetEntryByID(ID: integer): TPageEntryStream;
begin
   if not FindEntry(ID,Result)then
      raise Exception.Create('invalid entry '+inttostr(ID));
end;

procedure TPagesManager.SetPagesCount(const Value: integer);
begin
  if Value <= FPages.Count then
     Exit;
  FPages.Count:=Value;
end;

procedure TPagesManager.UpdatePageNextLink(APage: TPageBase);
var
 Sz,Value:integer;
begin
  Sz:=SizeOf(TInitHdr)+ (APage.Address+1)* SizeOf(integer);
  if FIniStream.Size < Sz then
     FIniStream.Size:=Sz;
  Value:=PageToLoc(APage.Next);
  FIniStream.Position :=SizeOf(TInitHdr)+ APage.Address* SizeOf(integer);
  FIniStream.WriteBuffer(Value,SizeOf(integer));
end;

procedure TPagesManager.UpdateEntryLink(AEntry: TPageEntryStream);
var
 Sz:integer;
 EnHdr:TEntryHDR;
begin
  if AEntry = FEntriesListEntry then
     Exit;
  Sz:=(AEntry.Address+1)* SizeOf(TEntryHDR);
  if FEntriesListEntry.Size < Sz then
     FEntriesListEntry.Size:=Sz;
  EnHdr.mStart:=PageToLoc(AEntry.Start);
  EnHdr.mSize:=AEntry.Size;
  EnHdr.mID := AEntry.ID;
  EnHdr.mUserData := AEntry.UserData;
  EnHdr.mState:=AEntry.State;
  FEntriesListEntry.Position := AEntry.Address* SizeOf(TEntryHDR);
  FEntriesListEntry.WriteBuffer(EnHdr,SizeOf(TEntryHDR));
end;

procedure TPagesManager.Update;
var
 Hdr:TInitHDR;
begin
    if FDestroing then
       Exit;
    Hdr.mCrc:=0;
    Hdr.mPagesCount := PagesCount;
    Hdr.mPageSize := PageSize;
    Hdr.mEntriesCount:=EntryCount;
    Hdr.mEntrieSize:= FEntriesListEntry.Size;
    Hdr.mEntriesList:=PageToLoc(FEntriesListEntry.Start);
    Hdr.mEntriesListCount:=FEntriesListEntry.Count;
    Hdr.mFreeList :=PageToLoc(FreeList);
    Hdr.mCrc:=HashCRC32Buff(Hdr,SizeOf(Hdr));
    FIniStream.Position := 0;
    FIniStream.WriteBuffer(Hdr,SizeOf(Hdr));
end;

procedure TPagesManager.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TPagesManager.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount=0 then
  begin
     FUpdateCount:=1;
     Update();
     Dec(FUpdateCount);
  end;
end;

{ TCachedStream }

constructor TCachedStream.Create(AStream: TStream;APageSize:integer);
begin
   FStream:=AStream;
   FPageSize:=APageSize;
   Position:=AStream.Position;
   FNextList:=@FFirst;
   FMaxCache:=5;
end;

destructor TCachedStream.Destroy;
var
 P,N:TCacheItem;
 I:integer;
begin
   Flush();
   P:=FFirst;
   while P <>nil do
   begin
      N:=P.Next;
      Freemem(P.FMem);
      P.Free;
      P:=N;
   end;
  for I := 0 to Length(FSegments) -1 do
    FSegments[I].Free;
  inherited;
end;

function TCachedStream.GetSize: Int64;
begin
  Result:=FStream.Size;
end;

function TCachedStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
    case Origin of
      soFromBeginning: FPosition := Offset;
        soFromCurrent: FPosition := FPosition + Offset;
            soFromEnd: FPosition := FStream.Size - Offset;
    end;
    if FPosition > FStream.Size then
      FPosition := FStream.Size
    else if FPosition < 0 then
      FPosition := 0;
  FStream.Seek(FPosition div FPageSize,soFromBeginning);
  Result := FPosition;
end;

procedure TCachedStream.SetSize(const NewSize: Int64);
begin
   FStream.Size := NewSize;
end;

function TCachedStream.LoadPage(ASeg:integer): TCacheItem;
begin
   Result:=NewCacheItem(ASeg);
   FStream.Position :=ASeg*FPageSize;
   FStream.Read(Result.FMem^,FPageSize);
end;

function TCachedStream.Read(var Buffer; Count: Integer): Longint;
var
  P:PByteArray;
  Rd:integer;
  Cache:TCacheItem;
  Off,Seg:integer;
begin
  Result:=0;
  P:=@Buffer;
  while Count > 0 do
  begin
      Rd:=0;
      Seg:=FPosition div FPageSize;
      Off:=FPosition mod FPageSize;
      Cache:= Caches[Seg];
      if Cache <> nil then
         Rd := Cache.GetPageSize() - Off;
      if (Rd=0)and (FStream.Size > FPosition)then
      begin
         Cache:=LoadPage(Seg);
         Rd:= Cache.GetPageSize();
         Off:=0;
      end;
      if Rd > Count then
         Rd := Count;
      if Rd = 0 then
         break;
      System.Move(Pointer(Longint(Cache.FMem)+ Off)^, P[Result], Rd);
      Inc(Result,Rd);
      Inc(FPosition,Rd);
      Dec(Count,Rd);
  end;
end;

function TCachedStream.Write(const Buffer; Count: Integer): Longint;
var
  P:PByteArray;
  Wt,Sz:integer;
  Cache:TCacheItem;
  Off,Seg:integer;
begin
  Sz:=FPosition+Count;
  if Sz > FStream.Size then
     Size := Sz;
  Result:=0;
  P:=@Buffer;
  while Count > 0 do
  begin
      Wt:=0;
      Seg:=FPosition div FPageSize;
      Off:=FPosition mod FPageSize;
      Cache:= Caches[Seg];
      if Cache <> nil then
         Wt := FPageSize - Off;
      if Wt=0 then
      begin
         Cache:=LoadPage(Seg);
         Wt:=FPageSize;
         Off:=0;
      end;
      if Wt > Count then
         Wt := Count;
      if Wt = 0 then
         break;
      System.Move(P[Result],Pointer(Longint(Cache.FMem)+Off)^, Wt);
      Cache.Modified :=True;
      Inc(Result,Wt);
      Inc(FPosition,Wt);
      Dec(Count,Wt);
  end;
end;

function TCachedStream.NewCacheItem(ASeg:integer):TCacheItem;
begin
   if FCount = FMaxCache then
   begin
       Result:=FFirst;
       FFirst:=FFirst.Next;
       Result.Flush();//must notify
       Caches[Result.Segment]:=nil;
       Result.Next :=nil;
   end else begin
       Result:=TCacheItem.Create;
       Result.Owner :=Self;
       GetMem(Result.FMem,FPageSize);
       inc(FCount);
   end;
   Result.Segment :=ASeg;
   Caches[ASeg]:=Result;
   Result.Modified :=False;
   FNextList^:=Result;
   FNextList:=@Result.Next;
end;

procedure TCachedStream.Flush;
var
 P:TCacheItem;
begin
   P:=FFirst;
   while P <>nil do
   begin
      P.Flush();
      P:=P.Next
   end;
end;

procedure TCachedStream.SetMaxCache(const Value: integer);
var
 N:TCacheItem;
begin
  FMaxCache := Value;
  while FCount > FMaxCache do
  begin
     N:=FFirst; // delete  the oldest Caches
     FFirst:=FFirst.Next;
     N.Flush();//must notify
     Caches[N.Segment]:=nil;
     Freemem(N.FMem);
     N.Free;
     Dec(FCount);
  end;
end;
type
  TWordSegment =class
  public
    Data :array[byte] of Pointer;
  end;

  TAddressRec = packed record
   case integer of
     0:(Address:integer);
     1:(Offset:byte;
        Seg  :Word;)
  end;
function TCachedStream.GetCache(Address: integer): Pointer;
begin
    if Address >= $01000000 then
       raise Exception.Create('invalid index');
    with TAddressRec(Address) do
    begin
        if (Seg < Length(FSegments))and Assigned(FSegments[Seg]) then
            Result:=  TWordSegment(FSegments[Seg]).Data[Offset]
        else
            Result:=  nil;
    end;
    inc(Total);
    if assigned(Result) then
       inc(Found);
end;

procedure TCachedStream.SetCache(Address: integer; Value: Pointer);
begin
    if Address >= $01000000 then
       raise Exception.Create('invalid index');
    with TAddressRec(Address) do
    begin
        if Seg >= Length(FSegments)  then
            SetLength(FSegments,Seg+1);

        if FSegments[Seg] = nil then
           FSegments[Seg] := TWordSegment.Create;
        TWordSegment(FSegments[Seg]).Data[Offset]:= Value
    end;
end;

{ TCacheItem }

procedure TCacheItem.Flush;
begin
   if Modified then
    with Owner do
    begin
       FStream.Position :=Segment*FPageSize;
       FStream.Write(FMem^,GetPageSize());
    end;
   Modified:=False;
end;

function TCacheItem.GetPageSize: Integer;
begin
 with Owner do
  if (Segment+1) * FPageSize >= FStream.Size then
  begin
     Result := FStream.Size mod FPageSize;
     if (Result=0) and (FStream.Size <> 0) then
         Result:=FPageSize
  end else
     Result:= FPageSize;
end;

end.

