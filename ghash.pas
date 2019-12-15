unit GHash;

interface

{
*** FAST HASH LIST IMPLEMENTATION, FOR WELL KNOWN SIZED LISTS ***
*   AUTHOR: GUSTAVO HISPAGNOL (GUSTAVO@WINDOOR.COM.BR)          *
*   R1: 21/10/2000                                              *
*****************************************************************
}

uses Classes,SysUtils;

{$R-}

type
    THashList=class;
    
    PHashRec=^THashRec;
    THashRec=record
       Used:Boolean;
       Item:String;
       Data:Pointer;
       Next:PHashRec;
    end;

    TDuplicates=(dupError,dupIgnore);
    TAutoDeletionType=(dtNone,dtDispose,dtFree);

    TOnDeletion=procedure (sender:TObject;data:Pointer) of object;
    TCompareProc=function  (const i1,i2:String):Boolean;
    TIterateProc=procedure (sender:THashList;const item:String;data:Pointer;var continue:Boolean) of object;

    THashList=class
    private
       FRootList:array of THashRec;
       fSize:Integer;
       fOnDeletion: TOnDeletion;
       fCaseInsensitive: Boolean;
       fAutoDeletion: TAutoDeletionType;
       fDuplicates: GHash.TDuplicates;
       fCompareProc:array [boolean] of TCompareProc;
       fColisions,
       fCount: Integer;
       function  Hash(const Item:String):Integer;
       procedure MyOnDelete(sender: TObject; data: Pointer);
    public
       function  Remove(const Item:String):Boolean;
       procedure Add(const Item:String;Data:Pointer);overload;
       procedure AddInt(const Item:String;Data:Integer);
       procedure Add(const Item:String);overload;
       function  Find(const Item:String;var Data:Pointer):Boolean;overload;
       function  FindInt(const Item: String;var Data:Integer): Boolean;
       function  Find(const Item: String): Boolean;overload;
       function  Exists(const Item:String):Boolean;
       procedure Clear;
       procedure Assign(other:THashList);
       procedure Resize(newsize:Integer);
       procedure Iterate(IterateProc: TIterateProc);

       property  Count:Integer read fCount;
       property  Colisions:Integer read fColisions;
       property  Duplicates:GHash.TDuplicates read fDuplicates write fDuplicates;
       property  AutoDeletion:TAutoDeletionType read fAutoDeletion write fAutoDeletion;
       property  CaseInsensitive:Boolean read fCaseInsensitive write fCaseInsensitive;
       property  OnDeletion:TOnDeletion read fOnDeletion write fOnDeletion;

       constructor Create(size:Integer);
       destructor  Destroy;override;
    end;

    function HashStr(const data:String): LongWord;

implementation

{ THashList }

function SuperFastHashAsm3(AData: pointer; ALength: integer): LongWord;
// asm translation of the SuperFastHash function by Paul Hsieh
// more info: http://www.azillionmonkeys.com/qed/hash.html
// Translation by: Davy Landman
// No warranties, but have fun :)
asm
push esi
push edi
test eax, eax // data
jz @Ret // eax is result
xchg edx, eax // swith data and length
test eax, eax // length, and hash
jle @Ret
@Start:
mov edi, eax // remainer
mov esi, eax // max
and edi, 3 // remainer
shr esi, 2 // number of loops
jz @Last3
@Loop:
movzx ecx, word ptr [edx]
add eax, ecx
movzx ecx, word ptr [edx + 2]

shl ecx, 11
xor ecx, eax
shl eax, 16

xor eax, ecx
mov ecx, eax

shr eax, 11
add eax, ecx
add edx, 4 
dec esi
jnz @Loop
@Last3:
test edi, edi
jz @Done
dec edi
jz @OneLeft
dec edi
jz @TwoLeft

movzx ecx, word ptr [edx]
add eax, ecx
mov ecx, eax
shl eax, 16
xor eax, ecx
movsx ecx, byte ptr [edx + 2]
shl ecx, 18
xor eax, ecx
mov ecx, eax
shr ecx, 11
add eax, ecx
jmp @Done
@TwoLeft:
movzx ecx, word ptr [edx]
add eax, ecx
mov ecx, eax
shl eax, 11
xor eax, ecx
mov ecx, eax
shr eax, 17
add eax, ecx
jmp @Done
@OneLeft:
movsx ecx, byte ptr [edx]
add eax, ecx
mov ecx, eax
shl eax, 10
xor eax, ecx
mov ecx, eax
shr eax, 1
add eax, ecx
@Done:
// avalanche
mov ecx, eax
shl eax, 3
xor eax, ecx

mov ecx, eax
shr eax, 5
add eax, ecx

mov ecx, eax
shl eax, 4
xor eax, ecx

mov ecx, eax
shr eax, 17
add eax, ecx

mov ecx, eax
shl eax, 25
xor eax, ecx

mov ecx, eax
shr eax, 6
add eax, ecx
@Ret:
pop edi
pop esi
ret
end;

function HashStr(const data:String): LongWord;
begin
    if Length(data)>0 then
       Result := SuperFastHashAsm3(@data[1],Length(data))
    else
       Result := 0; 
end;

function SZIsPrime(Number : Cardinal) : Boolean;
var
  I, J : Cardinal;
begin
  if Number < 4 then
    Result := Number > 1
  else begin
    Result := False;

    I:= (Number mod 6);
    if (I=1) or (I=5) then
    begin
      J := round(sqrt(Number));
      I := 5;
      while I <= J do
      begin
        if (Number mod I = 0) then
           Exit;

        if (Number <> J) and
           (Number mod (I+2) = 0) then
           Exit;

        inc(I,6);
      end;
      Result := True;
    end;
  end;
end;

function CompareCi(const i1, i2: String): Boolean;
begin
     Result := CompareText(i1,i2)=0
end;

function CompareCs(const i1, i2: String): Boolean;
begin
     Result := i1=i2;
end;

procedure THashList.Add(const Item: String;Data:Pointer);
var
   i:Integer;
   ni:PHashRec;
begin
     i := Hash(Item);
     if (not FRootList[i].Used) then
     begin
        FRootList[i].Data := Data;
        FRootList[i].Used := True;
        FRootList[i].Item := Item;
        Inc(fCount);
     end
     else
     begin
        Inc(fColisions);
        ni := @FRootList[i];
        while not (ni=nil) do
        begin
              if fCompareProc[fCaseInsensitive](ni.Item,Item) then
                if fDuplicates=dupIgnore then
                   Exit
                else
                   raise Exception.Create('Duplicated item: '+Item);

              if (ni.Next=nil) then
              begin
                    New(ni.Next);

                    ni.Next^.Used := True;
                    ni.Next^.Data := Data;
                    ni.Next^.Item := Item;
                    ni.Next^.Next := nil;

                    Inc(fCount);

                    break;
              end;

              ni := ni.Next;
        end;

     end;

end;

procedure THashList.MyOnDelete(sender:TObject;data:Pointer);
begin

end;

procedure THashList.Clear;
var
   x:Integer;
   isroot:Boolean;
   hrp,ohrp:PHashRec;
begin
     if @fOnDeletion=nil then fOnDeletion := MyOnDelete;

     fCount := 0;
     fColisions := 0;
     for x:=0 to Length(FRootList)-1 do
     begin
          hrp := @FRootList[x];
          isroot := True;

          while not (hrp=nil) do
          begin
               fOnDeletion(self,hrp.data);

               if Assigned(hrp.data) then
               case fAutoDeletion of
               dtDispose: Dispose(hrp.data);
               dtFree:    TObject(hrp.data).Free;
               end;

               ohrp := hrp;
               hrp := hrp.Next;

               if not isroot then
               begin
                  Dispose(ohrp);
               end
               else
                  isroot := False; 
          end;
     end;

     SetLength(FRootList,0);
     SetLength(FRootList,fSize);
end;

procedure THashList.Iterate(IterateProc:TIterateProc);
var
   x:Integer;
   hrp:PHashRec;
   continue:Boolean;
begin
     continue := True;
     for x:=0 to Length(FRootList)-1 do
     begin
          hrp := @FRootList[x];

          while not (hrp=nil) and continue do
          begin
               if hrp.Used then
                  IterateProc(self,hrp.Item,hrp.data,continue);

               hrp := hrp.Next;
          end;
     end;
end;

constructor THashList.Create(size: Integer);
begin
    inherited Create;

    Resize(size);     

    fCompareProc[False] := CompareCs;
    fCompareProc[True]  := CompareCi;
end;

destructor THashList.Destroy;
begin
     Clear;   
     inherited;
end;

function THashList.Find(const Item: String;var Data:Pointer): Boolean;
var
   i:Integer;
   ni:PHashRec;
begin     
     Result := False;
     if FSize>0 then
     begin
         i := Hash(Item);
         if (FRootList[i].Used) then
         begin
            if fCompareProc[fCaseInsensitive](Item,FRootList[i].Item) then
            begin
               Result := True;
               Data := FRootList[i].Data;
            end
            else
            begin
                ni := FRootList[i].Next;
                while not (ni=nil) do
                begin
                      if fCompareProc[fCaseInsensitive](ni.Item,Item) then
                      begin
                            Result := True;
                            Data := ni.Data;
                            Break;
                      end;

                      ni := ni.Next;
                end;
            end;

         end;
     end;
end;

function THashList.Hash(const Item:String):Integer;
var
   i,sz:Integer;
   p:PChar;
begin
     Result := 0;
     sz := Pred(Length(item));
     p := PChar(item);

     if not fCaseInsensitive then
     begin
       for i := 0 to sz do
       begin
           Inc(Result,Byte(p^));
           Inc(Result,Result shl 10);
           Result := Result xor (Result shr 6);
           Inc(p);
       end;
       //Result := SuperFastHashAsm3(p,sz);
     end
     else
     begin
       for i := 0 to sz do
       begin
           Inc(Result,Byte(UpCase(p^)));
           Inc(Result,Result shl 10);
           Result := Result xor (Result shr 6);
           Inc(p);
       end;

       Inc(Result,Result shl 3);
       Result := Result xor (Result shr 11);
       Inc(Result,Result shl 15);
     end;

     Result := (Result mod FSize);
     if Result<0 then Result := -Result;
end;

(*function THashList.Hash(const Item: String): Integer;
const
    CHashConst = 23; { prime number. If changed, ensure it does not create
                       an overflow in the Sum variable. }
var
    I     : Word;
    Max   : Word;

begin
    Max   := Length(Item);
    if Max > 100 then Max := 100;

    I   := 1;
    Result := 0;

    if fCaseInsensitive then
    begin
          while (I <= Max) do
          begin
              Result := (Result * CHashConst) + Ord(UpCase(Item[I]));
              Inc(I)
          end;
    end
    else
    begin
          while (I <= Max) do
          begin
              Result := (Result * CHashConst) + Ord(Item[I]);
              Inc(I)
          end;
    end;

    { keep hash address in range }

    if FSize>0 then Result := (Result mod FSize) else Result := 0;
    if Result<0 then Result := Result*-1;

    {if (Result>=FSize) or (Result<0) then
        raise Exception.Create('Error in Hash Function '+IntToStr(Result));}

end;*)

function THashList.Remove(const Item: String):Boolean;
var
   i:Integer;
   ni,lastni:PHashRec;
begin
     if @fOnDeletion=nil then fOnDeletion := MyOnDelete;

     Result := False;
     i := Hash(Item);
     if (FRootList[i].Used) then
     begin
        ni := @FRootList[i];
        lastni := ni;
        while not (ni=nil) do
        begin
              //found
              if fCompareProc[fCaseInsensitive](ni.Item,Item) then
              begin
                   //redirect "parent" item
                   lastni.Next := ni.Next;

                   fOnDeletion(self,ni.data);

                   if Assigned(ni.data) then
                   case fAutoDeletion of
                   dtDispose: Dispose(ni.data);
                   dtFree:    TObject(ni.data).Free;
                   end;

                   if ni<>@FRootList[i] then
                   begin
                      Dispose(ni);
                   end
                   else if FRootList[i].Next=nil then
                   begin
                      //head and finished!
                      FRootList[i].Used := False;
                      FRootList[i].Next := nil;
                      FRootList[i].Data := nil;
                      FRootList[i].Item := '';
                   end
                   else //head but there are more items
                   begin
                      ni := FRootList[i].Next;
                      FRootList[i] := ni^;
                      Dispose(ni);
                   end;

                   Result := True;
                   Dec(fCount);
                   
                   break;
              end;

              lastni := ni;              
              ni := ni.Next;
        end;

     end;
end;

function THashList.Exists(const Item: String): Boolean;
var
   d:Pointer;
begin
     Result := Find(Item,d);
end;

function THashList.FindInt(const Item: String; var Data: Integer): Boolean;
begin
     Result := Find(Item,Pointer(Data));
end;

function THashList.Find(const Item: String): Boolean;
var
   i:Pointer;
begin
     Result := Find(Item,i);
end;

procedure THashList.AddInt(const Item: String; Data: Integer);
begin
     Add(Item,Pointer(Data));
end;

procedure THashList.Add(const Item: String);
begin
     Add(Item,nil);
end;

procedure THashList.Assign(other: THashList);
var
   x:Integer;
   r,nr:PHashRec;
begin
     Clear;
     
     fSize := other.fSize;
     fCaseInsensitive := other.fCaseInsensitive;
     fColisions := other.fColisions;
     fCount := other.fCount;
     SetLength(FRootList,Length(other.FRootList));
     for x:=0 to High(FRootList) do
     begin
         FRootList[x] := other.FRootList[x];
         r := @FRootList[x];

         while r.Next<>nil do
         begin
              New(nr);
              nr^ := r.Next^;
              r.Next := nr;
              r := r.Next;
         end;
     end;
end;

procedure THashList.Resize(newsize: Integer);
begin
    Clear;

    fSize := Round(newsize * 1.2); //reduce colisions

    while not SZIsPrime(fSize) do
          Inc(fSize);

    SetLength(FRootList,fSize);
end;

end.