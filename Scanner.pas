{
  Scanner.pas

  Written by Frank Plagge
  Copyright (c) 1998 by Frank Plagge, Elsterweg 39, 38446 Wolfsburg, Germany
  All rights reserved

  Please send comments to plagge@positiv.escape.de

  *****************************************************************************
  many thanks for bug reports and ideas to
     - Takayuki Ogawa (t2ogawa@krhm.jvc-victor.co.jp):
             + stop the analysis caused by user event result
             + handling block comments
             + reading system leadbytes

     - Kovacs Lehel - Istvan (klehel@praemium.ro):
             + take a stream instead of files as input for the analysis

     - Oliver Matla (wolfpack@eulink.net) :
             + take a stream instead of files as input for the analysis
             + bug within recognition of special char $ followed by an
               identifier
  *****************************************************************************

  V 1.01 - Jan 3rd, 1998
           first implementation, never trust a version 1.00 :-)
  V 1.02 - Jan 12th, 1998
           hexdecimals with the new token ttHexDecimal added
           the hex numbers are defined like C e.g. 0x12AB or like
           Pascal e.g. $12AB
           the state machine is extended by states 13, 14, 15 and 16
  V 1.03 - Aug 8th, 1998
            + the complete input source is changed from file into stream
            - the filename property is no longer available
            + german mutated vowels added (because i needed this:-) )
            + analysis can be stopped by a return value of the token event
              a stopped analysis can be restarted with the method restart
            + scanner can handle block comments, the result is a ttcomment token
              for each comment line. the state machine is extended by the state
              18 which handles the comment block reading. state 18 is the
              starting point of the machine if the comment block reading is
              active.
  V 1.04 - Aug 14th, 1998
            + bugfix for comment recognition
            + added TToken.CopyFrom method
            + improved recognition of $ as special char and following identifier
            + added the property AllowFigures for inclusion or exclusion of
              figures within identifiers or keywords
            + improved recognition of integer followed directly by , or .
            + default palette name changed to "Enhanced"
            + readonly property version added
            - special sulution for german mutated vowels deleted
            + property AdditionalChars added, language specific letters for
              identifiers can be integrated there
            + the addition to the list of tokens can be prevented by setting
              the variable AddToList within the event OnTokenRead to false

  *****************************************************************************
  Permission to use, copy,  modify, and distribute this software and its
  documentation without fee for any purpose is hereby granted, provided that
  the above copyright notice appears on all copies and that both that copyright
  notice and this permission notice appear in all supporting documentation.

  NO REPRESENTATIONS ARE MADE ABOUT THE SUITABILITY OF THIS SOFTWARE FOR ANY
  PURPOSE. IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
  NEITHER FRANK PLAGGE OR ANY OTHER PERSON SHALL BE LIABLE FOR ANY DAMAGES
  SUFFERED BY THE USE OF THIS SOFTWARE.
  *****************************************************************************

   description:
   This module contains the component TCustomScanner. It contains the complete
   lexical analysis of a stream source. This is a necessary basis to realize
   a parser for any language. The analysis produces a stream of token.


   properties and events at design time and runtime:

    property AdditionalChars: string  -- default: ''
    every language specific letter that is allowed within identifiers
    can be integrated here. the letters 'a' to 'z' and 'A' to 'Z' are the
    standard letters for identifiers. if any other additional letter is needed
    it can be entered in this property. the scanner component sorts the string
    automatical.

    property AllowFigures: Boolean  -- default: true
    if this property is true, figures are allowed within every identifier or
    keyword. if this property is false no figures are allowed and every figure
    will cause a ttIdentifier or ttKeyword token. the figure will be interpreted
    as the beginning of a new token with the type ttInteger.

    property AllowIdentifier: Boolean  -- default: true
    if this property is true, every identifier which is not a keyword will
    result a ttIdentifier token. if this property is false every non keyword
    identifier will cause a ttError token

    property CommentLine: string  -- default: '//'
    this property contains the leading char for a comment. every comment is
    introduced with this string and is ended by the end of line

    property CommentBegin: string  -- default: '{'
    this property contains the introducing string for a block comment.
    every block comment is introduced with this string and is ended by
    the CommentEnd string

    property CommentBegin: string  -- default: sorry can't write it down here :-(
    this property contains the ending string for a block comment.

    property Keywords: TStringList  -- default: empty
    the keyword of the scanner are stored in the property Keywords. the
    scanner component sorts list of keywords automatical. if the scanner reads
    an identifier and the identifier is a member of the keywords it will result
    a token ttKeyword

    property KeywordsCaseSensitive: Boolean -- default: false
    if this property is true the compare if an identifier is made case.

    property SpecialChars: string  -- default: ''
    every special chars that results a ttSpecialChar token have to be entered
    in this string. the scanner component sorts the string automatical.

    property OnTokenRead: TTokenEvent
    this user defined event is called if a new token is read from the input
    stream. this event is called atfer the token is read and before it is stored
    in the internal list of the scanner component. if any application dependent
    changes before registering the new token are neccessary it is possible to
    change every part of the new token.


   properties and methods at runtime:

    procedure Analyze( Source: TStream )
    this is the main method taking a source stream to tokens. the
    number of read tokens is available via the property count. the read
    tokens are available in the array Token. before analyzing a new source stream
    the results of a previously analysis are deleted.

    procedure ClearTokenList;
    the token list produced by the stream analysis can be cleared by this
    method.

    procedure Restart
    a formerly stopped analysis goes on working

    property Count: Integer
    this property contains the number of token read by the method analyze.

    property Token[Index: Integer]
    this property gives an easy access to the available token. a little example
    shows the access:

        for i := 1 to Count do
          WorkWithToken( Token[i-1] );

    property Version: string
    this property contains the internal implementation version. maybe this is
    useful, all my self developed components have such a version control
    mechanism

}

  {$IFDEF VER130}{$DEFINE VER120}{$ENDIF}

{$W-}
{$R-}
{$B-}           // no complete boolean evaluation

unit Scanner;

interface

uses
  SysUtils, Classes, ghash;

type

  // set type for the special chars
  TCharSet = set of Char;
                                      
  // enumeration with the possible result token
  TTokenType = ( ttNone,ttComment, ttEof, ttError,
                 ttHexDecimal, ttIdentifier, ttInteger,
                 ttKeyword, ttReal, ttSpecialChar, ttString, ttOperator, ttSingleLineComment);

  // a token contains the token type, the belonging text and the position
  // within the source stream
  TToken = ^TTokenRec;
  TTokenRec = record
                Token: TTokenType;   // token type
                KeywordIndex: Word;
                Position,Len: Longword;      // positition within the stream
           end;

  // this event type is called if a token from the input stream is read
  // - Token is the actual read token,
  // - AddToList determines if the token is added to the list of read tokens (default is true)
  // - Stop determines if the analysis is stopped now (default is false); a stopped analysis
  //   goes on with the method Restart
  TTokenEvent = procedure (Sender: TObject; Index:Integer; Token: TToken; var AddToList, Stop: Boolean) of object;

  TTokenList=class(TList)
  private
    FSource:String;
    function GetItem(Index: Integer): TToken;
    function GetText(Index: Integer):String;
    function GetTextItem(Token:TToken):String;
  public
    procedure  Clear;override;
    function   IsLast(Index:Integer):Boolean;
    property   Token[Index:Integer]:TToken read GetItem;
    property   Text[Index:Integer]:String read GetText;

    function   Compare(AToken: TToken; const Str: String): Boolean;
    function   CompareL(AToken: TToken; const Str: String;Len:Integer): Boolean;

    function   FindBlockEnd(var Position:Integer;BlockOpen,BlockClose:Char):Integer;

    destructor Destroy;override;
  end;

  TSwitchProc=procedure(ch:Char) of object;

  TScannerBookmark=class
  private
    FCommentRecurse:Integer;
    Eof,FLastIsMathOp,CommentBlockPhase:Boolean;
    FPosition,EAPosition,SourceY,SourceX:Longword;
    FCurChar,FLastChar:PChar;
  end;

  // decralation of the main scanner component
  TCustomScanner = class
  private
    { Private-Decarations}
    FLastIsMathOp,
    FKeyWordsChanged:Boolean;
    FPosition:Longword;
    FLastChar:PChar;
    FCurChar:PChar;
    FCommentRecurse:Integer;
    FSourceSize:Integer;

    CommentBlockPhase: Boolean;      // flag if a comment block reading phase is active
    EAState:    Byte;                // condition of the state machine
    EAToken:    TTokenType;          // recognized token
    EAPosition: Longword;             // Position of the first token char within the stream
    Eof:        Boolean;             // indicate if eof of stream is reached
    FIdentChars:      TCharSet;      // chars allowed within an identifier
    FAllowFigures:    Boolean;       // allow figures within identifiers
    FAllowIdentifier: Boolean;       // allow identifiers
    FCaseSensitive:   Boolean;       // detecting keywords case sensitive
    FCharacters:      TCharSet;      // allowed special chars
    FCommentLine:     string;        // introducing comment chars
    FCommentBegin:    string;        // introducing comment block begining string
    FCommentEnd:      string;        // introducing comment block ending string
    FKeywords:        TStringList;   // list of defined keywords
    FKeywordHL :      THashList;
    FOnTokenRead: TTokenEvent;       // user defined event if a new token is read

    //SourceStream: TStream;           // input stream for lexical analysis
    FSource:String;
    SourceY:  Longword;               // actual row within source stream
    SourceX:  Longword;               // actual column within source stream

    FStateToToken:array[0..30] of TTokenType;
    FStateToSwitch:array[0..20] of TSwitchProc;

    fHexChar:Char;
    fStrHexChar: String;
    fCharEscape: Char;
    fCharEscapeStr: String;

  protected    
    function  CompareAhead(const Str:String):Boolean;

    procedure EASwitch0( ch: Char ); // process a char at state 0 ( start)
    procedure EASwitch1( ch: Char ); // process a char at state 1
    procedure EASwitch3( ch: Char ); // process a char at state 3
    procedure EASwitch5( ch: Char ); // process a char at state 5
    procedure EASwitch7( ch: Char ); // process a char at state 7
    procedure EASwitch9( ch: Char ); // process a char at state 9
    procedure EASwitch11( ch: Char );// process a char at state 11
    procedure EASwitch13( ch: Char );// process a char at state 13
    procedure EASwitch14( ch: Char );// process a char at state 14
    procedure EASwitch16( ch: Char );// process a char at state 16
    procedure EASwitch18( ch: Char );// process a char at state 18
    procedure EASwitch19( ch: Char );// process a char at state 19
    function  GetAdditionalChars: string;  // read method for property AdditinalChars
    //function  GetCount: Integer;           // read method for property Count
    //function  GetToken( Index: Integer ) : TToken;  // read method for property Token
    function  GetSpecialChars: string;     // read method for property SpecialChars
    function  GetVersion: string;          // read the internal version number
    function  IsKeyword( Token:TToken ) : Boolean;  // test if a string is a keyword
    procedure LookAheadChar(var ch:Char;nchars:Integer); // get the net char without reading it
    procedure ProcessChar;                      // process the available char
    procedure ReadCh( var ch: Char );           // read a char from the source stream
    procedure SkipChars( Count: Integer );      // skip a number of chars
    procedure SetAdditionalChars( const Value: string ); // write method for AdditionalChars
    procedure SetKeywords( Value: TStringList );   // write method for property Keywords
    procedure SetSpecialChars( const Value: string );
    procedure IndexKeywords;
    procedure KeyWordsChanged(Sender: TObject);

    { Protected-Declaration }
    procedure Restart;virtual;                                    // restart a former broken analysis    
  public
    { Public-Declaration }
    procedure AnalyzeStr( const Source: String );         // analyze the input string
    property  Version: string read GetVersion;             // internal implementation version

    function  AddKeyW(const KeyW:String):Integer;

    function  Compare(const AToken: TToken; const Str: String): Boolean;virtual;
    function  CompareL(const AToken: TToken;const Str: String;Len:Integer): Boolean;virtual;    
    function  Text(const AToken:TToken):String;virtual;

    function  NextToken(token:TToken):Boolean;         // read the next token

    function  CreateBookmark:TScannerBookmark;
    procedure GotoBookmark(value:TScannerBookmark);

    constructor Create; virtual;                                  // create the scanner component
    destructor  Destroy; override;                        // destroy the scanner component
    
    property AdditionalChars: string read GetAdditionalChars write SetAdditionalChars;
    property AllowFigures: Boolean read FAllowFigures write FAllowFigures;
    property AllowIdentifier: Boolean read FAllowIdentifier write FAllowIdentifier;
    property CommentLine: string read FCommentLine write FCommentLine;
    property CommentBegin: string read FCommentBegin write FCommentBegin;
    property CommentEnd: string read FCommentEnd write FCommentEnd;
    property Keywords: TStringList read FKeywords write SetKeywords;
    property KeywordsCaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    property SpecialChars: string read GetSpecialChars write SetSpecialChars;
    property HexChar:String read fStrHexChar write fStrHexChar;
    property CharEscape:String read fCharEscapeStr write fCharEscapeStr;
    property OnTokenRead: TTokenEvent read FOnTokenRead write FOnTokenRead;
  end;

  TSequentialScanner = class(TCustomScanner)
  end;

  TScanner = class(TCustomScanner)
  private
    TokenList:  TTokenList;
  protected
    function  GetCount: Integer;
    function  GetToken(Index: Integer): TToken;
    procedure Restart;override;
    procedure ReadList;          // list of read token
  public
    function  Compare(const AToken: TToken; const Str: String): Boolean;overload;override;
    function  Compare(i:Integer; const Str: String): Boolean;reintroduce;overload;
    function  CompareL(const AToken: TToken;const Str: String;Len:Integer): Boolean;override;
    function  Text(const AToken:TToken):String;override;

    procedure ClearTokenList;                             // clear token list
    function  ReleaseList:TTokenList;
    procedure SetList(AList:TTokenList);
    property  Count: Integer read GetCount;                // number of found token
    property  Token[Index: Integer] : TToken read GetToken;// array with found token
    function  IsLast(Index:Integer):Boolean;
    function  TextI(Index:Integer):String;
    constructor Create;override;
    destructor  Destroy;override;
  end;

implementation

const
  cVersion:   string   = '1.04';
  WhiteSpace: TCharSet = [' ', #9, #10, #13];   // known white spaces

const
  Upper: array[Char] of Char =
    (#$00,#$01,#$02,#$03,#$04,#$05,#$06,#$07,#$08,#$09,#$0A,#$0B,#$0C,#$0D,#$0E,#$0F,
     #$10,#$11,#$12,#$13,#$14,#$15,#$16,#$17,#$18,#$19,#$1A,#$1B,#$1C,#$1D,#$1E,#$1F,
     #$20,#$21,#$22,#$23,#$24,#$25,#$26,#$27,#$28,#$29,#$2A,#$2B,#$2C,#$2D,#$2E,#$2F,
     #$30,#$31,#$32,#$33,#$34,#$35,#$36,#$37,#$38,#$39,#$3A,#$3B,#$3C,#$3D,#$3E,#$3F,
     #$40,#$41,#$42,#$43,#$44,#$45,#$46,#$47,#$48,#$49,#$4A,#$4B,#$4C,#$4D,#$4E,#$4F,
     #$50,#$51,#$52,#$53,#$54,#$55,#$56,#$57,#$58,#$59,#$5A,#$5B,#$5C,#$5D,#$5E,#$5F,
     #$60,#$41,#$42,#$43,#$44,#$45,#$46,#$47,#$48,#$49,#$4A,#$4B,#$4C,#$4D,#$4E,#$4F,
     #$50,#$51,#$52,#$53,#$54,#$55,#$56,#$57,#$58,#$59,#$5A,#$7B,#$7C,#$7D,#$7E,#$7F,
     #$80,#$81,#$82,#$83,#$84,#$85,#$86,#$87,#$88,#$89,#$8A,#$8B,#$8C,#$8D,#$8E,#$8F,
     #$90,#$91,#$92,#$93,#$94,#$95,#$96,#$97,#$98,#$99,#$9A,#$9B,#$9C,#$9D,#$9E,#$9F,
     #$A0,#$A1,#$A2,#$A3,#$A4,#$A5,#$A6,#$A7,#$A8,#$A9,#$AA,#$AB,#$AC,#$AD,#$AE,#$AF,
     #$B0,#$B1,#$B2,#$B3,#$B4,#$B5,#$B6,#$B7,#$B8,#$B9,#$BA,#$BB,#$BC,#$BD,#$BE,#$BF,
     #$C0,#$C1,#$C2,#$C3,#$C4,#$C5,#$C6,#$C7,#$C8,#$C9,#$CA,#$CB,#$CC,#$CD,#$CE,#$CF,
     #$D0,#$D1,#$D2,#$D3,#$D4,#$D5,#$D6,#$D7,#$D8,#$D9,#$DA,#$DB,#$DC,#$DD,#$DE,#$DF,
     #$E0,#$E1,#$E2,#$E3,#$E4,#$E5,#$E6,#$E7,#$E8,#$E9,#$EA,#$EB,#$EC,#$ED,#$EE,#$EF,
     #$F0,#$F1,#$F2,#$F3,#$F4,#$F5,#$F6,#$F7,#$F8,#$F9,#$FA,#$FB,#$FC,#$FD,#$FE,#$FF);

function StrIComp(const Str1, Str2: PChar): Integer;
var
  Ch1, Ch2 : Char;
  Offset   : Integer;
  PStr     : PChar;
begin;
  PStr   := Str1;
  Offset := Str2 - PStr;
  repeat
    Ch1 := Upper[PStr^];
    Ch2 := Upper[PStr[Offset]];
    if (Ch1 = #0) or (Ch1 <> Ch2) then
      Break;
    Inc(PStr);
  until False;
  Result := Integer(Ch1) - Integer(Ch2);
end;

function TokenGetText(const Source:String;Token: TToken):String;
begin
    Result := Copy(Source,Token.Position+1,Token.Len);
end;

function TokenCompareL(const Source:String;const AToken: TToken; const Str: String; Len: Integer): Boolean;
var
   i,offset:Integer;
   pc,lc,strc:PChar;
begin
     lc     := PChar(Source)+(Length(Source)-1);
     pc     := PChar(Source)+AToken.Position;
     strc   := PChar(Str);
     offset := strc-pc;

     Result := False;

     i := 0;
     while (pc<=lc) and (i<Len) do
     begin
          if Upper[pc^]<>Upper[pc[offset]] then Exit;
          Inc(pc);
          //Inc(strc);
          Inc(i);
     end;

     Result := True;
end;
  
function TokenCompare(const Source:String;const AToken: TToken; const Str: String): Boolean;
begin
     Result := False;

     if (AToken.Len=Longword(Length(Str))) then
     begin
          Result := TokenCompareL(Source,AToken,Str,AToken.Len);
     end;

     //Result := CompareText(source,str)=0;
     //Result := StrIComp(PChar(Source)+AToken.Position,PChar(Str))=0;
end;

procedure TCustomScanner.KeyWordsChanged(Sender:TObject);
begin
     FKeyWordsChanged := True;
end;

// this is create constructor of the scanner. no changes to the
// inherited create, only initialization of internal and external
// variables
constructor TCustomScanner.Create;
begin
  inherited Create;                 // create the class
  FKeywords := TStringList.Create;  // create the list of keywords
  FKeywords.OnChange := KeyWordsChanged;
  FAllowFigures := true;            // default figures are allowed within identifiers
  FAllowIdentifier := true;         // default no identifier is allowed
  FCaseSensitive := false;          // default no case sensitive keyword compare
  FCharacters := [];                // default there are no special chars
  FCommentLine := '//';             // the default comment begin is '//'
  FCommentBegin := '{';             // the default comment block start mark is '{'
  FCommentEnd := '}';               // the default comment block end mark is '}'
  FIdentChars := ['a'..'z', 'A'..'Z']; // first all letters are allowed within identifiers
  fStrHexChar := '$';
  fCharEscapeStr := '#';

  FStateToToken[2]  := ttIdentifier;
  FStateToToken[4]  := ttInteger;
  FStateToToken[6]  := ttReal;
  FStateToToken[8]  := ttString;
  FStateToToken[10] := ttSpecialChar;
  FStateToToken[12] := ttComment;
  FStateToToken[15] := ttHexdecimal;
  FStateToToken[17] := ttSingleLineComment;  
  FStateToToken[19] := ttOperator;
  FStateToToken[29] := ttError;
  FStateToToken[30] := ttEOF;

  FStateToSwitch[0] := EASwitch0;
  FStateToSwitch[1] := EASwitch1;
  FStateToSwitch[3] := EASwitch3;
  FStateToSwitch[5] := EASwitch5;
  FStateToSwitch[7] := EASwitch7;
  FStateToSwitch[9] := EASwitch9;
  FStateToSwitch[11] := EASwitch11;
  FStateToSwitch[13] := EASwitch13;
  FStateToSwitch[14] := EASwitch14;
  FStateToSwitch[16] := EASwitch16;
  FStateToSwitch[18] := EASwitch18;
  FStateToSwitch[20] := EASwitch19;

end;

// this is destructor of the scanner. it is neccessary to free the internal
// dynamic data structures
destructor TCustomScanner.Destroy;
begin
  FKeywords.Free;     // deallocate the memory used by the list of keywords
  FKeywordHL.Free;
  inherited Destroy;  // destroy the class
end;

// internal the allowed chars for an identifier are stored in an set of char
// this method converts the set of char into a string
function TCustomScanner.GetAdditionalChars: string;
var i: Integer;
begin
  Result := '';                                  // first there are no additional chars
  for i := 0 to 255 do begin                     // for all possible chars
    if (Chr(i) in FIdentChars) and               // if the char in in the set
       (not (Chr(i) in ['a'..'z'])) and          // and it's not a lower case letter
       (not (Chr(i) in ['A'..'Z']))  then begin  // an d not a higer case letter
      Result := Result + Chr(i);                 // add the char to the string
    end;
  end;
end;

function TCustomScanner.GetVersion: string;
begin
  Result := cVersion;
end;

// internal the special char are stored in a set of char
// this method converts the set of char into a string
function TCustomScanner.GetSpecialChars: string;
var i: Integer;
begin
  Result := '';                          // first there are no special chars
  for i := 0 to 255 do begin             // for all possible chars
    if Chr(i) in FCharacters then begin  // if the char is in the set
      Result := Result + Chr(i);         // add the char to the string
    end;
  end;
end;

// this method tests if a string is a keyword. the keywords are defined in
// the list Keywords
function TCustomScanner.IsKeyword( Token:TToken ) : Boolean;
var 
    i:          Pointer;
begin
  if FKeywordHL=nil then
      Result := False  
  else
  begin
      Result := FKeywordHL.Find(Text(Token),i);
      if Result then Token.KeywordIndex := Integer(i);
  end;
end;

// get the net char without reading it
procedure TCustomScanner.LookAheadChar(var ch:Char;nchars:Integer);
begin

  if (FCurChar+nchars<=FLastChar) then
     ch := PChar(FCurChar+nchars)^
  else
     ch := #0;

end;

// this method reads a char from the source stream and adds it to the
// actual token text
procedure TCustomScanner.ProcessChar;
begin
     Inc(FCurChar);
     Inc(FPosition);     
end;

// read a new char from the input stream
// the char #10 is used as global linefeed; MAC file has only #13 as linefeed,
// havn't they? Sorry!
// this procedure count the actual row and colum of the input stream.
procedure TCustomScanner.ReadCh( var ch: Char );

procedure ReadOne;
begin
     ch := FCurChar^;
     Inc(FCurChar);
     Inc(FPosition);
end;

begin

  if not Eof then
      ReadOne
  else
      ch := #0;

  if (FCurChar>FLastChar) then
      Eof := true;

end;

// read the next token with a state machine
function TCustomScanner.NextToken(token:TToken):Boolean;
var
    NextChar:Char;
begin
  EAState := 0;                  // the first state is zero

  if Eof then begin    // if the end of stream is reached
    EAToken := ttEof;  // create a ttEof token
  end else begin
    EAToken := ttNone;

    while (EAToken=ttNone) do
    begin

      LookAheadChar(NextChar,0);   // get the net char without reading it

      FStateToSwitch[EAState](NextChar);
      EAToken := FStateToToken[EAState];

      if (NextChar=#0) then
      begin
          if EAToken in [ttNone,ttError] then EAToken:=ttEof;
          Break;
      end;
    end;
  end;

  Result := (EAToken<>ttEof) and ((FPosition-EAPosition>0) or (EAToken=ttString));

  if Result then
  begin
      FLastIsMathOp := False;
      Token.Token := EAToken;          // save the read token
      Token.Position := EAPosition;    // save the position within the stream
      Token.Len := FPosition-EAPosition;

      if Token.Token=ttString then
      begin
          Inc(Token.Position);
          Dec(Token.Len,2);
      end
      else if Token.Token=ttComment then
      begin
          Inc(Token.Position,Length(FCommentBegin));
          Dec(Token.Len,Length(FCommentBegin)+Length(FCommentEnd));
      end;

      if Token.Position+Token.Len>Longword(Length(FSource)) then
         raise Exception.Create('Inconsistent Parse');

      // if an identifier is read and the belonging text is a keyword
      // then change the token type into ttKeyword
      if (Token.Token=ttIdentifier) and IsKeyword(Token) then
          Token.Token := ttKeyword;

      if (Token.Token=ttKeyword) then
          FLastIsMathOp := True
      else if (Token.Token=ttSpecialChar) or (Token.Token=ttError) or (Token.Token=ttOperator) then
      begin
          if Compare(Token,'+')  or
             Compare(Token,'-')  or
             Compare(Token,'/')  or
             Compare(Token,'*')  or
             Compare(Token,'>')  or
             Compare(Token,'<')  or
             Compare(Token,'=')  or
             Compare(Token,'<>') or
             Compare(Token,'>=') or
             Compare(Token,'<=') or
             Compare(Token,',') or
             Compare(Token,'(') then
                FLastIsMathOp := True;
      end;
  end;

end;

// restart a fomerly broken analysis phase
procedure TCustomScanner.Restart;
begin
  FCommentRecurse := 0;
  Eof := false;                         // now end of stream is not yet reached
  FPosition := 0;
end;

// the user defines the additional chars for identifier in a string. this
// string must be converted into a set of char.
procedure TCustomScanner.SetAdditionalChars( const Value: string );
var i: Integer;
begin
  FIdentChars := ['a'..'z', 'A'..'Z'];        // first all letters are allowed
  for i := 1 to Length(Value) do begin        // for every char in the string
    FIdentChars := FIdentChars + [Value[i]];  // add the char to the set
  end;
end;

// this method seems to be unnecessary, but it is very important for
// the correct work of TStringList in the object inspector
procedure TCustomScanner.SetKeywords( Value: TStringList );
begin
  FKeywords.Assign( Value );
end;

// the user defines the special char in a string. this string must be converted
// into a set of char. working with a set of char is much easier, but i
// do not want to implement a new property editor for die usage with the
// object inspector
procedure TCustomScanner.SetSpecialChars( const Value: string );
var i: Integer;
begin
  FCharacters := [];                          // first the set of char is empty
  for i := 1 to Length(Value) do begin        // for every char in the string
    FCharacters := FCharacters + [Value[i]];  // add the char to the set
  end;
end;

// skip the next count chars from the source stream
procedure TCustomScanner.SkipChars( Count: Integer );
begin
  if FCurChar+Count<=FLastChar then
  begin
      Inc(FCurChar,Count);
      Inc(FPosition,Count);
  end
  else
  begin
      FCurChar := FLastChar;
      FPosition := FSourceSize;
      Eof := true;
  end;

end;

// the following methods are characterizing the internal state machine

// process a char if the state machine has the state 0
// state 0 is the starting state
procedure TCustomScanner.EASwitch0( ch: Char );
var
  auxc:Char;
begin
  if Eof then begin    // if the end of source stream is reached
    EAState := 30;     // switch to state 99
  end else begin
    if ch in FIdentChars then begin
      EAState := 1;
      EAPosition := FPosition;
      ProcessChar;

    end
    else begin
      case ch of
        '0'       : begin            // if a '0' is read switch to state 13
                      EAState := 13; // and try to read an integer, real or hex number
                      EAPosition := FPosition;
                      ProcessChar;
                    end;
        '1'..'9': begin           // if number is read switch to state 3
                      EAState := 3; // and try to read a integer or a floting point number
                      EAPosition := FPosition;
                      ProcessChar;
                    end;
        ''''      : begin           // if the char ' is read switch to state 7
                      EAState := 7; // and try to read a string limited by '
                      EAPosition := FPosition;
                      ReadCh( ch );
                    end;
        '"'       : begin           // if the char " is read switch to state 9
                      EAState := 9; // and try to read a string limited by "
                      EAPosition := FPosition;
                      ReadCh( ch );
                    end;
        '|','<','>','=':
                    begin
                        if ch in FCharacters then
                        begin
                            EAState := 10;             // switch to state 10
                            EAPosition := FPosition;
                            ProcessChar;               // process this char
                        end
                        else
                        begin
                            EAState := 20;
                            EAPosition := FPosition;
                        end;
                    end;
      else          begin
                        if ch=fHexChar then
                        begin            // if a '$' is available switch to state 16
                          EAState := 16; // and try to read a hex number or a spacial char
                          EAPosition := FPosition;
                          ProcessChar;
                        end
                        else if ch in WhiteSpace then
                        begin // if a white space is read
                          ReadCh( ch );        // read the next char
                          // if a comment beginning is defined and if the next chars corresend to
                          // the defined comment line begin
                        end
                        else if CompareAhead(FCommentLine) then
                        begin
                          EAState := 11;             // switch to state 11
                          EAPosition := FPosition;
                          SkipChars( Length(FCommentLine ) );  // skip the comment line begin
                          // if a blockcomment beginning is defined and if the next chars corresend to
                          // the defined block comment beginning
                        end
                        else if CompareAhead(FCommentBegin) then
                        begin
                          Inc(FCommentRecurse);
                          if FCommentRecurse=1 then
                          begin
                             EAState := 18;             // switch to state 11
                             EAPosition := FPosition;
                             SkipChars( Length(FCommentBegin ) ); // skip the comment block begin
                          end;
                          // if the actual char is a member of the special chars
                        end
                        else if ch in ['+','-'] then
                        begin

                          //try to read a number

                          LookAheadChar(auxc,1);

                          if ((auxc in ['1'..'9']) or (auxc='0')) and FLastIsMathOp then
                              EAState := 3 
                          else if ch in FCharacters then //special char?
                              EAState := 10
                          else
                              EAState := 29;             // switch to state 98 (error)

                          EAPosition := FPosition;
                          ProcessChar;

                        end
                        else if ch in FCharacters then begin
                          EAState := 10;             // switch to state 10
                          EAPosition := FPosition;
                          ProcessChar;               // process this char
                          // else an illegal char is read and this will cause an error
                        end
                        else begin
                          EAState := 29;             // switch to state 98
                          EAPosition := FPosition;
                          ProcessChar;               // process this char
                        end;
                    end;
      end;
    end;
  end;
end;

// process a char if the state machine has the state 1.
// in this state the state machines tries to read an identifier. an identifier
// consists of a leading char and any following number or char
procedure TCustomScanner.EASwitch1( ch: Char );
begin
  if (ch=fCharEscape) and (fCharEscape<>#0) and (FPosition>EAPosition) then
    EAState := 2
  else if (ch in FIdentChars) or
     ((FAllowFigures) and (ch in ['0'..'9'])) then begin
    ProcessChar         // every letter is a part of the identifier
  end else begin
    EAState := 2;        // else switch to final state 2, identifier is complete
  end;
end;

// process a char if the state machine has the state 3.
// in this state a integer or a floating point number is read
procedure TCustomScanner.EASwitch3( ch: Char );
var
   nch:Char;
begin
  case ch of
    '0'..'9' : ProcessChar;    // if a number is read the char is processed
    '.'       :begin
                     LookAheadChar(nch,1);
                     if (nch<>#0) and (nch in ['0'..'9']) then
                     begin
                        // if a '.' is read and the following char is a figure
                        // the char is processed
                        ProcessChar;  // and the state is switched to state 5 in order
                        EAState := 5; // to read a floating point number
                    end
                    else
                    begin
                        EAState := 4; // the state is switched to final state 4 (integer)
                    end;
               end;
  else         EAState := 4;   // the state is switched to final state 4 (integer)
  end;
end;

// process a char if the state machine has the state 5.
// in this state floating point number is read
procedure TCustomScanner.EASwitch5( ch: Char );
begin
  case ch of
    '0'..'9'  : ProcessChar    // if a number is read process the char
  else          EAState := 6;  // else the state is switched to final state 6
  end;
end;

// process a char if the state machine has the state 7
// in this state string enclosed in ' is read
procedure TCustomScanner.EASwitch7( ch: Char );
begin
  case ch of
    #0, #10,                    // if a #0 (eof) or a linefeed char is read there is an
    #13     : EAState := 29;    // error because the string is not finished
    ''''    : begin             // if the final ' is read
                EAState := 8;   // switch to final state 8 and read the next char
                ReadCh( ch );
              end;
  else        ProcessChar;      // else the char is a member of the string
  end;
end;

// process a char if the state machine has the state 9
// in this state string enclosed in " is read
procedure TCustomScanner.EASwitch9( ch: Char );
begin
  case ch of
    #0{, #10,                    // if a #0 (eof) or a linefeed char is read there is an
    #13}     : EAState := 29;    // error because the string is not finished
    '"'  : begin                // if the final " is read
             EAState := 8;      // switch to final state 8 and read the next char
             ReadCh( ch );
           end;
  else     ProcessChar;         // else the char is a member of the string
  end;
end;

// process a char if the state machine has the state 11
// in this state the state machines reads a comment line.
// a comment begins with the introducing user defined comment string. this
// introducing string is already by the state 0. every linefeed or carridge
// return will end the comment line
procedure TCustomScanner.EASwitch11( ch: Char );
begin
  // is the introducing string read complete
  case ch of                     // every linefeed finishes the comment
     #10, #13 : begin
                    EAState := 17; // switch to final state 17
                    ReadCh( ch );  // read the next char
                end;
  else     ProcessChar;  // if no linefeed is read add the char to the uncomplete comment line
  end;
end;

// process a char if the state machine has the state 13
// in this state a zero is already read and it is possible that it is
// an integer, a real or a hex number
procedure TCustomScanner.EASwitch13( ch: Char );
begin
  case ch of
    'x', 'X': begin           // if a 'x' is read a hexnumer is found
               EAState := 14; // switch to state 14 to read rest of the hex number
               ProcessChar;
             end;
   '0'..'9': begin            // if another number is read it is an integer or
               EAState := 3;  // real number will follow
               ProcessChar;
             end;
   '.'     : begin            // if a '.' or a ',' is read the char is processed
               ProcessChar;   // and the state is switched to state 5 in order
               EAState := 5;  // to read a floating point number
             end;
  else       EAState := 4;  // the state is switched to final state 4 ( single '0' read)
  end;
end;

// process a char if the state machine has the state 14
// in this state a hex number is read
procedure TCustomScanner.EASwitch14( ch: Char );
begin
  case ch of
    'a'..'f',
    'A'..'F',
    '0'..'9' : ProcessChar;
  else         if fHexChar in FCharacters then begin // if $ is defined as a special char
                 EAState := 10;                 // switch to state final 10, do not read next char!!

                 Inc(FCurChar,(EAPosition-FPosition)+1);
                 FPosition := EAPosition+1;

               end else begin
                 //ProcessChar;   // another char without whitespace
                 EAState := 15; // the state is switched to final state 15, hex number is complete
               end;
  end;
end;

// process a char if the state machine has the state 14
// in this state a hex number is read
procedure TCustomScanner.EASwitch16( ch: Char );
begin
  case ch of
    'a'..'f',                    // is an allowed hex char is read
    'A'..'F',
    '0'..'9' : begin
                 EAState := 14;  // switch to state 14 and read the complete hex number
                 ProcessChar;
               end;
  else         begin
                 if fHexChar in FCharacters then begin // if $ is defined as a special char
                   EAState := 10;                 // switch to state final 10, do not read next char!!
                 end else begin
                   ProcessChar;     // if $ is not the first char of a hex number
                   EAState := 29;   // and no special char there is an error
                 end;
               end;
  end;
end;

// process a char if the state machine has the state 18
// in this state the state machines reads a comment block
// a comment begins with the introducing user defined comment block begin. this
// introducing string is already by the state 0. only the correspondign comment
// block end string will end the comment phase
procedure TCustomScanner.EASwitch18( ch: Char );
begin
  // is the introducing string read complete
  // from now to the comment block end the scanner is in a comment block read phase
  CommentBlockPhase := true;

  if CompareAhead(FCommentEnd) then
     begin
          Dec(FCommentRecurse);
          if FCommentRecurse=0 then
          begin
              EAState := 12;                     // switch to final state 11
              SkipChars( Length(FCommentEnd ) ); // skip the comment block begin
              CommentBlockPhase := false;        // coment block phase has ended
          end
          else
              ProcessChar;
     end
     else
     begin

          if CompareAhead(FCommentBegin) then
             Inc(FCommentRecurse);

          ProcessChar;  
     end;
end;


{ TTokenList }

function TTokenList.Compare(AToken: TToken; const Str: String): Boolean;
begin
     Result := TokenCompare(FSource, AToken, Str);
end;

function TTokenList.CompareL(AToken: TToken; const Str: String; Len: Integer): Boolean;
begin
     Result := TokenCompareL(FSource,AToken,Str,Len);
end;

function TTokenList.GetTextItem(Token: TToken): String;
begin
     Result := TokenGetText(FSource,Token);
end;

function TTokenList.FindBlockEnd(var Position:Integer;BlockOpen,BlockClose:Char):Integer;
var
  blkStarted:Boolean;
  blk:Integer;
begin
      blkStarted := False;
      blk := 0;
      Result := Position;
      while Result<Count do
      begin
           if Token[Result].Len>0 then
           begin
               if Compare(Token[Result],BlockOpen) then
               begin
                  if not blkStarted then Position := Result;
                  blkStarted := True;
                  Inc(blk);
               end
               else if Compare(Token[Result],BlockClose) then
               begin
                  if not blkStarted then Break; //close Block before open
                  Dec(blk);
               end;
           end;

           // if the block ends and already started then get out
           if blkStarted and (blk=0) then Exit;

           Inc(Result);

      end;
      Result := -1;
end;

function TTokenList.IsLast(Index: Integer): Boolean;
begin
  Result := Index>=Count;
end;

function TTokenList.GetText(Index: Integer): String;
begin
     Result := GetTextItem(GetItem(Index));
end;

destructor TTokenList.Destroy;
begin
     Clear;
     inherited;
end;

function TTokenList.GetItem(Index: Integer): TToken;
begin
   try
     Result := TToken(inherited Items[Index]);
   except
         raise Exception.Create('Token index out of bounds');
   end;
end;


procedure TCustomScanner.IndexKeywords;
var
  x:Integer;
begin
     if FKeyWordsChanged then
     begin
          FKeywordHL.Free;
          FKeywordHL := nil;

          if FKeywords.Count>0 then
          begin
              FKeywordHL := THashList.Create(FKeywords.Count);
              FKeywordHL.CaseInsensitive := True;

              for x:=0 to FKeywords.Count-1 do
                 FKeywordHL.Add(FKeywords[x],Pointer(x));
          end;

          FKeyWordsChanged := False;
     end;
end;

procedure TCustomScanner.AnalyzeStr(const Source: String);
begin
  IndexKeywords;

  if Length(fStrHexChar)>0 then
     fHexChar := fStrHexChar[1]
  else
     fHexChar := '$';

  if Length(fCharEscapeStr)>0 then
     fCharEscape := fCharEscapeStr[1]
  else
     fCharEscape := #0;

  FSource := Source;
  FSourceSize := Length(FSource);
  FCurChar := PChar(FSource);
  FLastChar := PChar(FSource)+(Length(FSource)-1);
  FLastIsMathOp := True;

  SourceY := 1;                         // first row is 1
  SourceX := 1;                         // first column is 1
  CommentBlockPhase := false;           // the scanner is not in a commant block phase
  Restart;                              // from now it is only a restart
end;

procedure TTokenList.Clear;
var
   x:Integer;
begin
     fSource := '';

     for x:=0 to Pred(Count) do
         Dispose(TToken(inherited Items[x]));

     inherited Clear;
end;

function TCustomScanner.AddKeyW(const KeyW:String):Integer;
begin
     result := fKeyWords.Count;
     fKeyWords.AddObject(KeyW,Pointer(Result));
     FKeyWordsChanged := True;
end;

function TCustomScanner.Compare(const AToken: TToken; const Str: String): Boolean;
begin
     Result := TokenCompare(FSource,AToken,Str);
     //Result := StrIComp(PChar(FSource)+AToken.Position,PChar(Str))=0;
end;

function TCustomScanner.CompareAhead(const Str: String): Boolean;
begin
     Result := False;
     if FCurChar+(Length(Str)-1)<=FLastChar then
        Result := StrLComp(FCurChar,PChar(Str),Length(Str))=0;
        //Result := StrIComp(FCurChar,PChar(Str))=0;
end;

function TCustomScanner.Text(const AToken: TToken): String;
begin
     Result := TokenGetText(FSource,AToken);
end;

function TCustomScanner.CompareL(const AToken: TToken; const Str: String;
  Len: Integer): Boolean;
begin
     Result := TokenCompareL(FSource,AToken,Str,Len);
end;

procedure TCustomScanner.EASwitch19(ch: Char);
var
  auxc:Char;
begin
     case ch of
     '<':begin
              LookAheadChar(auxc,1);
              if auxc in ['>','='] then ProcessChar;
              ProcessChar;
              EAState := 19;
         end;
     '>':
         begin
              LookAheadChar(auxc,1);
              if auxc in ['='] then ProcessChar;
              ProcessChar;
              EAState := 19;
         end;
     '=':begin
              ProcessChar;
              EAState := 19;
         end;
     '|':begin
              LookAheadChar(auxc,1);
              if auxc='|' then ProcessChar;
              ProcessChar;
              EAState := 19;
         end;
     end;
end;

function TCustomScanner.CreateBookmark: TScannerBookmark;
begin
     Result := TScannerBookmark.Create;
     Result.FCommentRecurse   := FCommentRecurse;
     Result.Eof               := Eof;
     Result.FLastIsMathOp     := FLastIsMathOp;
     Result.CommentBlockPhase := CommentBlockPhase;
     Result.FPosition         := FPosition;
     Result.EAPosition        := EAPosition;
     Result.SourceY           := SourceY;
     Result.SourceX           := SourceX;
     Result.FCurChar          := FCurChar;
     Result.FLastChar         := FLastChar;
end;

procedure TCustomScanner.GotoBookmark(value: TScannerBookmark);
begin
     FCommentRecurse   := value.FCommentRecurse;
     Eof               := value.Eof;
     FLastIsMathOp     := value.FLastIsMathOp;
     CommentBlockPhase := value.CommentBlockPhase;
     FPosition         := value.FPosition;
     EAPosition        := value.EAPosition;
     SourceY           := value.SourceY;
     SourceX           := value.SourceX;
     FCurChar          := value.FCurChar;
     FLastChar         := value.FLastChar;
end;

{ TScanner }

// delete the old analysis results and deallocate the used memory
procedure TScanner.ClearTokenList;
begin
  TokenList.Clear;
end;

function TScanner.Compare(i: Integer; const Str: String): Boolean;
begin
  Result := TokenList.Compare(Token[i],Str);
end;

destructor TScanner.Destroy;
begin
  ClearTokenList;
  FreeAndNil(TokenList);
  inherited;
end;

function TScanner.IsLast(Index: Integer): Boolean;
begin
  Result := TokenList.IsLast(Index);
end;

function TScanner.ReleaseList: TTokenList;
begin
     Result := TokenList;
     TokenList := TTokenList.Create;
end;

procedure TScanner.ReadList;
var
    AddToList: Boolean;            // add a new token to the token list (result from user event)
    Stop:      Boolean;            // analysis break (result from user event)
    Index:     Integer;
    NewToken:  TToken;             // the last read token
begin
  NewToken := nil;
  Stop := False;                        // initialize abort flag

  TokenList.FSource := FSource;

  Index := 0;
  repeat                                // repeat until ttEOF Token is read
    if NewToken=nil then New(NewToken);          // create the token structure
                              // read the next token

    if NextToken(NewToken) then
    begin
        AddToList := True;                  // default is add the new token to the list of tokens
        // if a user defined event is available this event is called before the
        // token is put in the tokenlist
        if Assigned( FOnTokenRead ) then
          FOnTokenRead( Self, Index, NewToken, AddToList, Stop );

        if AddToList then
        begin
             TokenList.Add( NewToken );
             NewToken := nil;
        end;
    end;
    Inc(Index);
  until (EAToken = ttEof) or (Stop); // until ttEOF is read or abort flag is set

  Dispose(NewToken); //last token will not be disposed
end;

procedure TScanner.Restart;
begin
  ClearTokenList;
  inherited;
  ReadList;
end;

procedure TScanner.SetList(AList: TTokenList);
begin
  TokenList.Free;
  TokenList := AList;
end;

function TScanner.TextI(Index: Integer): String;
begin
  Result := Text(Token[Index]);
end;

// get the number of read token
function TScanner.GetCount: Integer;
begin
  Result := TokenList.Count; // read token are saved in internal list
end;

// get the already read token at index Index
function TScanner.GetToken( Index: Integer ) : TToken;
begin
  if (Index < 0 ) or (Index >= Count) then begin  // if the index is invalid
    Result := nil
  end else begin
    Result := TokenList.Items[Index];             // else return the token
  end;
end;

constructor TScanner.Create;
begin
  inherited;
  TokenList := TTokenList.Create;   // create the list of read token
end;

function TScanner.Compare(const AToken: TToken;
  const Str: String): Boolean;
begin
     Result := TokenList.Compare(AToken,Str);
end;

function TScanner.CompareL(const AToken: TToken; const Str: String;
  Len: Integer): Boolean;
begin
     Result := TokenList.CompareL(AToken,Str,Len);
end;

function TScanner.Text(const AToken: TToken): String;
begin
     Result := TokenList.GetTextItem(AToken);
end;

end.